
module GI.CodeGen
    ( genConstant
    , genFunction
    ) where

import Data.Char (toUpper)
import Data.Int
import Data.Typeable (mkTyCon, mkTyConApp, typeOf)
import Data.Word

import GI.API
import GI.Code
import GI.Value
import GI.Internal.ArgInfo

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

foreignBasicType TUTF8 = mkTyConApp (mkTyCon "CString") []
foreignBasicType TGType = mkTyConApp (mkTyCon "GType") []
foreignBasicType t = haskellBasicType t

foreignType (TBasicType t) = foreignBasicType t
foreignType (TArray a) =
    mkTyConApp (mkTyCon "GArray") [foreignType a]
foreignType (TGHash a b) =
    mkTyConApp (mkTyCon "GHash") [foreignType a, foreignType b]
-- XXX: Possibly nonsense. Perhaps the interface name needs to be qualified,
-- and its existence (in the typelib we're generating code for, or some other
-- typelib) verified.
foreignType (TInterface s) = mkTyConApp (mkTyCon s) []
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

io t = mkTyConApp (mkTyCon "IO") [t]

padTo n s = s ++ replicate (n - length s) ' '

split c s = split' s "" []
    where split' [] w ws = reverse (reverse w : ws)
          split' (x:xs) w ws =
              if x == c then split' xs "" (reverse w:ws)
                  else split' xs (x:w) ws

ucFirst (x:xs) = toUpper x : xs

lowerName s =
    case split '_' s of
        [w] -> w
        (w:ws) -> concat $ w : map ucFirst ws

upperName = map ucFirst . split '_'

genConstant :: Constant -> CodeGen ()
genConstant (Constant name value) = do
    line $ name ++ " :: " ++ (show $ haskellType $ valueType value)
    line $ name ++ " = " ++ valueStr value

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

genCallable :: String -> Callable -> CodeGen ()
genCallable symbol callable = do
    foreignImport symbol callable
    line ""
    wrapper

    where
    wrapper = signature
    signature = do
        line $ name ++ " ::"
        indent $ do
            mapM_ (line . hArgStr) inArgs
            line result
    inArgs = filter ((== DirectionIn) . direction) $ args callable
    outArgs = filter ((== DirectionOut) . direction) $ args callable
    name = lowerName $ callableName callable
    hArgStr arg =
        let start = (show $ haskellType $ argType arg) ++ " -> "
         in padTo 40 start ++ "-- " ++ argName arg
    result = show (io outType)
    outType =
        let hReturnType = haskellType $ returnType callable
            justType = case outArgs of
                [] -> hReturnType
                _ -> mkTyConApp (mkTyCon "(,)")
                        (hReturnType : map (haskellType . argType) outArgs)
            maybeType = mkTyConApp (mkTyCon "Maybe") [justType]
         in if returnMayBeNull callable then maybeType else justType

genFunction :: Function -> CodeGen ()
genFunction (Function symbol callable) = genCallable symbol callable

