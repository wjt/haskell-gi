
module GI.CodeGen
    ( genConstant
    , genFunction
    , genCode
    , genModule
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM, forM_)
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

ucFirst (x:xs) = toUpper x : xs
ucFirst "" = error "ucFirst: empty string"

getPrefix :: String -> CodeGen String
getPrefix ns = do
    cfg <- config
    case M.lookup ns (prefixes cfg) of
        Just p -> return p
        Nothing -> error $
            "no prefix defined for namespace " ++ show ns

lowerName (Name ns s) = do
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

upperName (Name ns s) = do
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

mapPrefixes :: Type -> CodeGen Type
mapPrefixes t@(TBasicType _) = return t
mapPrefixes (TArray t) = TArray <$> mapPrefixes t
mapPrefixes (TGList t) = TGList <$> mapPrefixes t
mapPrefixes (TGSList t) = TGSList <$> mapPrefixes t
mapPrefixes (TGHash ta tb) =
  TGHash <$> mapPrefixes ta <*> mapPrefixes tb
mapPrefixes t@TError = return t
mapPrefixes (TInterface ns s) = do
    prefix <- getPrefix ns
    return $ TInterface undefined (ucFirst prefix ++ s)

haskellType' :: Type -> CodeGen TypeRep
haskellType' t = haskellType <$> mapPrefixes t

foreignType' :: Type -> CodeGen TypeRep
foreignType' t = foreignType <$> mapPrefixes t

prime = (++ "'")

mkLet name value = line $ "let " ++ name ++ " = " ++ value

mkBind name value = line $ name ++ " <- " ++ value

genConstant :: Name -> Constant -> CodeGen ()
genConstant n@(Name _ name) (Constant value) = do
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
    mkBind' conv = mkBind (prime name) (conv ++ " " ++ name)
    hToF' "[Char]" "CString" = mkBind' "newCString"
    hToF' "Word"   "GType"   = mkLet' "fromIntegral"
    hToF' "Bool"   "CInt"    = mkLet' "fromEnum"
    hToF' "[Char]" "Ptr CString" = mkBind' "bullshit"
    hToF' hType fType = error $
        "don't know how to convert " ++ hType ++ " to " ++ fType

genCallable :: Name -> String -> Callable -> CodeGen ()
genCallable n symbol callable = do
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

genFunction :: Name -> Function -> CodeGen ()
genFunction n (Function symbol callable) = do
  line $ "-- function " ++ symbol
  genCallable n symbol callable

genStruct :: Name -> Struct -> CodeGen ()
genStruct n@(Name _ name) (Struct _fields) = do
  line $ "-- struct " ++ name
  name' <- upperName n
  line $ "data " ++ name' ++ " = " ++ name' ++ " (Ptr " ++ name' ++ ")"
  -- XXX: Generate code for fields.

genEnum :: Name -> Enumeration -> CodeGen ()
genEnum n@(Name ns name) (Enumeration fields) = do
  line $ "-- enum " ++ name
  name' <- upperName n
  line $ "data " ++ name' ++ " = "
  fields' <- forM fields $ \(fieldName, value) -> do
    n <- upperName $ Name ns (name ++ "_" ++ fieldName)
    return (n, value)
  indent $ do
    case fields' of
      ((fieldName, _value):fs) -> do
        line $ "  " ++ fieldName
        forM_ fs $ \(n, _) -> line $ "| " ++ n
      _ -> return()
  blank
  line $ "instance Enum " ++ name' ++ " where"
  indent $ forM_ fields' $ \(n, v) ->
    line $ "fromEnum " ++ n ++ " = " ++ show v
  blank
  -- XXX: Handle ambiguous toEnum conversions correctly.
  indent $ forM_ fields' $ \(n, v) ->
    line $ "toEnum " ++ show v ++ " = " ++ n

genFlags :: Name -> Flags -> CodeGen ()
genFlags n@(Name _ name) (Flags (Enumeration _fields)) = do
  line $ "-- flags " ++ name
  name' <- upperName n
  line $ "data " ++ name' ++ " = " ++ name'
  -- XXX: Generate code for fields.

genCallback :: Name -> Callback -> CodeGen ()
genCallback n _ = do
  name' <- upperName n
  line $ "-- callback " ++ name' ++ " "
  -- XXX
  line $ "data " ++ name' ++ " = " ++ name' ++ " (Ptr (IO ()))"

genCode :: Name -> API -> CodeGen ()
genCode n (APIConst c) = genConstant n c >> blank
genCode n (APIFunction f) = genFunction n f >> blank
genCode n (APIEnum e) = genEnum n e >> blank
genCode n (APIFlags f) = genFlags n f >> blank
genCode n (APICallback c) = genCallback n c >> blank
genCode n (APIStruct s) = genStruct n s >> blank
-- XXX
genCode _ (APIUnion _) = blank
genCode _ a = error $ "can't generate code for " ++ show a

genModule :: String -> [(Name, API)] -> CodeGen ()
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
    let (imports, rest) =
          splitImports $ runCodeGen' cfg $ forM_ apis (uncurry genCode)
    mapM_ (\c -> tell c >> blank) imports
    mapM_ tell rest

    where splitImports = partition isImport . codeToList
          isImport (Tag Import _code) = True
          isImport _ = False
