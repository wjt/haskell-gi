
module GI.CodeGen
    ( genConstant
    , genFunction
    ) where

import Control.Monad (forM_)
import Data.Char (toLower, toUpper)
import Data.Int
import Data.List (intercalate)
import Data.Typeable (mkTyCon, mkTyConApp, typeOf)
import Data.Word

import GI.API
import GI.Code
import GI.Value
import GI.Internal.ArgInfo

con s xs = mkTyConApp (mkTyCon s) xs

haskellBasicType TVoid    = typeOf ()
haskellBasicType TBoolean = typeOf True
haskellBasicType TInt8    = typeOf (0 :: Int8)
haskellBasicType TUInt8   = typeOf (0 :: Word8)
haskellBasicType TInt16   = typeOf (0 :: Int16)
haskellBasicType TUInt16  = typeOf (0 :: Word16)
haskellBasicType TInt32   = typeOf (0 :: Int32)
haskellBasicType TUInt32  = typeOf (0 :: Word32)
haskellBasicType TInt64   = typeOf (0 :: Int64)
haskellBasicType TUInt64  = typeOf (0 :: Word64)
-- XXX: Is this correct?
haskellBasicType TGType   = typeOf (0 :: Word)
haskellBasicType TUTF8    = typeOf ""
haskellBasicType TFloat   = typeOf (0 :: Float)
haskellBasicType TDouble  = typeOf (0 :: Double)
haskellBasicType t        = error $ "haskellBasicType: " ++ show t

haskellType (TBasicType bt) = haskellBasicType bt
haskellType t@(TArray _ ) = foreignType t
haskellType t@(TGHash _ _) = foreignType t
haskellType t@(TInterface _ ) = foreignType t
haskellType t = error $ "haskellType: " ++ show t

foreignBasicType TBoolean = "CInt" `con` []
foreignBasicType TUTF8    = "CString" `con` []
foreignBasicType TGType   = "GType" `con` []
foreignBasicType t        = haskellBasicType t

foreignType (TBasicType t) = foreignBasicType t
foreignType (TArray a) = "GArray" `con` [foreignType a]
foreignType (TGHash a b) = "GHash" `con` [foreignType a, foreignType b]
-- XXX: Possibly nonsense. Perhaps the interface name needs to be qualified,
-- and its existence (in the typelib we're generating code for, or some other
-- typelib) verified.
foreignType (TInterface s) = s `con` []
foreignType t = error $ "foreignType: " ++ show t

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

io t = "IO" `con` [t]

ptr t = "PTR" `con` [t]

padTo n s = s ++ replicate (n - length s) ' '

split c s = split' s "" []
    where split' [] w ws = reverse (reverse w : ws)
          split' (x:xs) w ws =
              if x == c then split' xs "" (reverse w:ws)
                  else split' xs (x:w) ws

ucFirst (x:xs) = toUpper x : map toLower xs

lowerName s =
    case split '_' s of
        [w] -> map toLower w
        (w:ws) -> concat $ map toLower w : map ucFirst ws

upperName = map ucFirst . split '_'

prime = (++ "'")

mkLet name value = line $ "let " ++ name ++ " = " ++ value

mkBind name value = line $ name ++ " <- " ++ value

genConstant :: Constant -> CodeGen ()
genConstant (Constant name value) = do
    let name' = lowerName name
    line $ name' ++ " :: " ++ (show $ haskellType $ valueType value)
    line $ name' ++ " = " ++ valueStr value

foreignImport :: String -> Callable -> CodeGen ()
foreignImport symbol callable = do
    line first
    indent $ do
        mapM_ (line . fArgStr) (args callable)
        line last
    where
    first = "import foreign ccall \"" ++ symbol ++ "\" " ++
                symbol ++ " :: "
    fArgStr arg =
        let start = (show $ foreignType $ argType arg) ++ " -> "
         in padTo 40 start ++ "-- " ++ argName arg
    last = show $ io $ foreignType $ returnType callable

hToF arg =
    if hType == fType
        then mkLet' "id"
        else hToF' (show hType) (show fType)

    where
    name = argName arg
    hType = haskellType $ argType arg
    fType = foreignType $ argType arg
    mkLet' conv = mkLet (prime name) (conv ++ " " ++ name)
    mkBind' conv = mkBind (prime name) (conv ++ " `fmap` " ++ name)
    hToF' "[Char]" "CString" = mkBind' "newCString"
    hToF' "Word"   "GType"   = mkLet' "fromIntegral"
    hToF' "Bool"   "CInt"    = mkLet' "fromEnum"
    hToF' _ _ = error $
        "don't know how to convert " ++ show hType ++ " to " ++ show fType

genCallable :: String -> Callable -> CodeGen ()
genCallable symbol callable = do
    foreignImport symbol callable
    blank
    wrapper

    where
    name = lowerName $ callableName callable
    inArgs = filter ((== DirectionIn) . direction) $ args callable
    outArgs = filter ((== DirectionOut) . direction) $ args callable
    wrapper = do
        signature
        line $ name ++ " " ++ intercalate " " (map argName inArgs) ++ " = do"
        indent $ do
            convertIn
            line $ "result <- " ++ symbol ++
                concatMap (prime . (" " ++) . argName) (args callable)
            convertOut
    signature = do
        line $ name ++ " ::"
        indent $ do
            mapM_ (line . hArgStr) inArgs
            line result
    convertIn = forM_ (args callable) $ \arg ->
        if direction arg == DirectionIn
            then hToF arg
            else mkBind (prime $ argName arg) $
                     "malloc :: " ++
                     show (io $ ptr $ foreignType $ argType arg)
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
        case (show outType, outArgs) of
            ("()", []) -> line $ "return ()"
            ("()", _) -> line $ "return (" ++ intercalate ", " pps ++ ")"
            (_ , []) -> line $ "return result'"
            (_ , _) -> line $
                "return (" ++ intercalate ", " ("result'" : pps) ++ ")"
    hArgStr arg =
        let start = (show $ haskellType $ argType arg) ++ " -> "
         in padTo 40 start ++ "-- " ++ argName arg
    result = show (io outType)
    outType =
        let hReturnType = haskellType $ returnType callable
            hOutArgTypes = map (haskellType . argType) outArgs
            justType = case outArgs of
                [] -> hReturnType
                _ -> "(,)" `con` (hReturnType : hOutArgTypes)
            maybeType = "Maybe" `con` [justType]
         in if returnMayBeNull callable then maybeType else justType

genFunction :: Function -> CodeGen ()
genFunction (Function symbol callable) = genCallable symbol callable

