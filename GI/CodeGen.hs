
module GI.CodeGen
    ( genConstant
    , genFunction
    , genCode
    , genModule
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Monad.Writer (tell)
import Data.Char (toLower, toUpper)
import Data.List (intercalate, partition)
import Data.Typeable (TypeRep)
import qualified Data.Map as M

import GI.API
import GI.Code
import GI.Type
import GI.Value
import GI.Internal.ArgInfo

valueStr VVoid         = "()"
valueStr (VBoolean x)  = show x
valueStr (VInt8 x)     = show x
valueStr (VUInt8 x)    = show x
valueStr (VInt16 x)    = show x
valueStr (VUInt16 x)   = show x
valueStr (VInt32 x)    = show x
valueStr (VUInt32 x)   = show x
valueStr (VInt64 x)    = show x
valueStr (VUInt64 x)   = show x
valueStr (VFloat x)    = show x
valueStr (VDouble x)   = show x
valueStr (VGType x)    = show x
valueStr (VUTF8 x)     = show x
valueStr (VFileName x) = show x

padTo n s = s ++ replicate (n - length s) ' '

split c s = split' s "" []
    where split' [] w ws = reverse (reverse w : ws)
          split' (x:xs) w ws =
              if x == c then split' xs "" (reverse w:ws)
                  else split' xs (x:w) ws

escapeReserved "type" = "type_"
escapeReserved "in" = "in_"
escapeReserved "data" = "data_"
escapeReserved s = s

ucFirst (x:xs) = toUpper x : map toLower xs
ucFirst "" = error "ucFirst: empty string"

getPrefix :: String -> CodeGen String
getPrefix ns = do
    cfg <- config
    case M.lookup ns (prefixes cfg) of
        Just p -> return p
        Nothing -> error $
            "no prefix defined for namespace " ++ show ns

lowerName (Named ns s _) = do
    cfg <- config

    case M.lookup s (names cfg) of
        Just s' -> return s'
        Nothing -> do
          let ss = split '_' s
          ss' <- addPrefix ss
          return . concat . rename $ ss'

    where addPrefix ss = do
              prefix <- getPrefix ns
              return $ prefix : ss

          rename [w] = [map toLower w]
          rename (w:ws) = map toLower w : map ucFirst' ws
          rename [] = error "rename: empty list"

          ucFirst' "" = "_"
          ucFirst' x = ucFirst x

upperName (Named ns s _) = do
    cfg <- config

    case M.lookup s (names cfg) of
        Just s' -> return s'
        Nothing -> do
            let ss = split '_' s
            ss' <- addPrefix ss
            return . concatMap ucFirst' $ ss'

    where addPrefix ss = do
              prefix <- getPrefix ns
              return $ prefix : ss

          ucFirst' "" = "_"
          ucFirst' x = ucFirst x

haskellType' :: Type -> CodeGen TypeRep
haskellType' (TInterface ns n) = do
    prefix <- getPrefix ns
    return $ haskellType (TInterface "!!!" (ucFirst prefix ++ n))
haskellType' t = return $ haskellType t

foreignType' :: Type -> CodeGen TypeRep
foreignType' (TInterface ns n) = do
    prefix <- getPrefix ns
    return $ foreignType (TInterface undefined (ucFirst prefix ++ n))
foreignType' t = return $ foreignType t

prime = (++ "'")

mkLet name value = line $ "let " ++ name ++ " = " ++ value

mkBind name value = line $ name ++ " <- " ++ value

genConstant :: Named Constant -> CodeGen ()
genConstant n@(Named _ name (Constant value)) = do
    name' <- lowerName n
    ht <- haskellType' $ valueType value
    line $ "-- constant " ++ name
    line $ name' ++ " :: " ++ show ht
    line $ name' ++ " = " ++ valueStr value

foreignImport :: String -> Callable -> CodeGen ()
foreignImport symbol callable = tag Import $ do
    line first
    indent $ do
        mapM_ (\a -> line =<< fArgStr a) (args callable)
        line =<< last
    where
    first = "foreign import ccall \"" ++ symbol ++ "\" " ++
                symbol ++ " :: "
    fArgStr arg = do
        ft <- foreignType' $ argType arg
        let start = show ft ++ " -> "
        return $ padTo 40 start ++ "-- " ++ argName arg
    last = show <$> io <$> foreignType' (returnType callable)

hToF :: Arg -> CodeGen ()
hToF arg = do
    hType <- haskellType' $ argType arg
    fType <- foreignType' $ argType arg
    if hType == fType
        then mkLet' "id"
        else hToF' (show hType) (show fType)

    where
    name = escapeReserved $ argName arg
    mkLet' conv = mkLet (prime name) (conv ++ " " ++ name)
    mkBind' conv = mkBind (prime name) (conv ++ " `fmap` " ++ name)
    hToF' "[Char]" "CString" = mkBind' "newCString"
    hToF' "Word"   "GType"   = mkLet' "fromIntegral"
    hToF' "Bool"   "CInt"    = mkLet' "fromEnum"
    hToF' "[Char]" "Ptr CString" = mkBind' "bullshit"
    hToF' hType fType = error $
        "don't know how to convert " ++ hType ++ " to " ++ fType

genCallable :: String -> Named Callable -> CodeGen ()
genCallable symbol n@(Named _ _ callable) = do
    foreignImport symbol callable
    wrapper

    where
    inArgs = filter ((== DirectionIn) . direction) $ args callable
    outArgs = filter ((== DirectionOut) . direction) $ args callable
    wrapper = do
        let argName' = escapeReserved . argName
        name <- lowerName n
        tag TypeDecl signature
        tag Decl $ line $
            name ++ " " ++
            intercalate " " (map argName' inArgs) ++
            " = do"
        indent $ do
            convertIn
            line $ "result <- " ++ symbol ++
                concatMap (prime . (" " ++) . argName') (args callable)
            convertOut
    signature = do
        name <- lowerName n
        line $ name ++ " ::"
        indent $ do
            mapM_ (\a -> line =<< hArgStr a) inArgs
            result >>= line
    convertIn = forM_ (args callable) $ \arg -> do
        ft <- foreignType' $ argType arg
        if direction arg == DirectionIn
            then hToF arg
            else mkBind (prime $ argName arg) $
                     "malloc :: " ++
                     show (io $ ptr ft)
    -- XXX: Should create ForeignPtrs for pointer results.
    -- XXX: Check argument transfer.
    convertOut = do
        -- XXX: Do reverse conversion here.
        mkLet "result'" "result"
        forM_ outArgs $ \arg ->
            if direction arg == DirectionIn
                then return ()
                else do
                    mkBind (prime $ prime $ argName arg) $
                        "peek " ++ (prime $ argName arg)
        let pps = map (prime . prime . argName) outArgs
        out <- outType
        case (show out, outArgs) of
            ("()", []) -> line $ "return ()"
            ("()", _) -> line $ "return (" ++ intercalate ", " pps ++ ")"
            (_ , []) -> line $ "return result'"
            (_ , _) -> line $
                "return (" ++ intercalate ", " ("result'" : pps) ++ ")"
    hArgStr arg = do
        ht <- haskellType' $ argType arg
        let start = show ht ++ " -> "
        return $ padTo 40 start ++ "-- " ++ argName arg
    result = show <$> io <$> outType
    outType = do
        hReturnType <- haskellType' $ returnType callable
        hOutArgTypes <- mapM (haskellType' . argType) outArgs
        let justType = case outArgs of
                [] -> hReturnType
                _ -> "(,)" `con` (hReturnType : hOutArgTypes)
            maybeType = "Maybe" `con` [justType]
        return $ if returnMayBeNull callable then maybeType else justType

genFunction :: Function -> CodeGen ()
genFunction (Function symbol callable) = do
  line $ "-- function " ++ symbol
  genCallable symbol callable

genStruct :: (Named Struct) -> CodeGen ()
genStruct n@(Named _ name (Struct _fields)) = do
  line $ "-- struct " ++ name
  name' <- upperName n
  line $ "data " ++ name' ++ " = " ++ name' ++ " (Ptr " ++ name' ++ ")"
  -- XXX: Generate code for fields.

genEnum :: Named Enumeration -> CodeGen ()
genEnum n@(Named _ name (Enumeration _fields)) = do
  line $ "-- enum " ++ name
  name' <- upperName n
  line $ "data " ++ name' ++ " = " ++ name'
  -- XXX: Generate code for fields.

genFlags :: Named Flags -> CodeGen ()
genFlags n@(Named _ name (Flags (Enumeration _fields))) = do
  line $ "-- flags " ++ name
  name' <- upperName n
  line $ "data " ++ name' ++ " = " ++ name'
  -- XXX: Generate code for fields.

genCallback :: Callback -> CodeGen ()
genCallback (Callback (Named _ name _)) = do
  line $ "-- callback " ++ name
  -- XXX
  line $ "type " ++ name ++ " =  () -> ()"

genCode :: API -> CodeGen ()
genCode (APIConst c) = genConstant c >> blank
genCode (APIFunction f) = genFunction f >> blank
genCode (APIEnum e) = genEnum e >> blank
genCode (APIFlags f) = genFlags f >> blank
genCode (APICallback c) = genCallback c >> blank
genCode (APIStruct s) = genStruct s >> blank

genModule :: String -> [API] -> CodeGen ()
genModule name apis = do
    line $ "-- Generated code."
    blank
    line $ "{-# LANGUAGE ForeignFunctionInterface #-}"
    blank
    -- XXX: Generate export list.
    line $ "module " ++ name ++ " where"
    blank
    line $ "import Data.Int"
    line $ "import Data.Word"
    line $ "import Foreign"
    line $ "import Foreign.C"
    blank
    cfg <- config
    let (imports, rest) = splitImports $ runCodeGen' cfg $ forM_ apis genCode
    mapM_ (\c -> tell c >> blank) imports
    mapM_ tell rest

    where splitImports = partition isImport . codeToList
          isImport (Tag Import c) = True
          isImport _ = False
