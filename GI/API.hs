
module GI.API
    ( API(..)
    , Named(..)
    , Constant(..)
    , Arg(..)
    , Callable(..)
    , Function(..)
    , Signal(..)
    , Property(..)
    , Field(..)
    , Struct(..)
    , Callback(..)
    , Interface(..)
    , Object(..)
    , Enumeration(..)
    , Flags (..)
    , loadAPI
    ) where

import Data.Word

import GI.Internal.Types
import GI.Internal.ArgInfo
import GI.Internal.BaseInfo
import GI.Internal.CallableInfo
import GI.Internal.ConstantInfo
import GI.Internal.EnumInfo
import GI.Internal.FieldInfo
import GI.Internal.FunctionInfo
import GI.Internal.InterfaceInfo
import GI.Internal.ObjectInfo
import GI.Internal.PropertyInfo
import GI.Internal.StructInfo
import GI.Internal.Typelib (getInfos, load)
import GI.Internal.UnionInfo
import GI.Type
import GI.Value

data Named a = Named { namespace :: String, name :: String, named :: a }
    deriving Show

toNamed :: BaseInfoClass bi => bi -> a -> Named a
toNamed bi x =
   let namespace = baseInfoNamespace $ baseInfo bi
       name = baseInfoName $ baseInfo bi
    in Named namespace name x

data Constant = Constant {
    constValue :: Value }
    deriving Show

toConstant :: ConstantInfo -> Named Constant
toConstant ci =
    let typeInfo = constantInfoType ci
        arg = constantInfoValue ci
        value = fromArgument typeInfo arg
     in toNamed ci $ Constant value

data Enumeration = Enumeration {
    enumValues :: [(String, Word64)] }
    deriving Show

toEnumeration :: EnumInfo -> Named Enumeration
toEnumeration ei = toNamed ei $ Enumeration $
    (map (\vi -> (baseInfoName . baseInfo $ vi, valueInfoValue vi))
        (enumInfoValues ei))

data Flags = Flags Enumeration
    deriving Show

toFlags :: EnumInfo -> Named Flags
toFlags ei = let Named ns n x = toEnumeration ei
              in Named ns n (Flags x)

data Arg = Arg {
    argName :: String,
    argType :: Type,
    direction :: Direction,
    scope :: Scope,
    transfer :: Transfer }
    deriving Show

toArg :: ArgInfo -> Arg
toArg ai =
   Arg (baseInfoName . baseInfo $ ai)
        (typeFromTypeInfo . argInfoType $ ai)
        (argInfoDirection ai)
        (argInfoScope ai)
        (argInfoOwnershipTransfer ai)

data Callable = Callable {
    returnType :: Type,
    returnMayBeNull :: Bool,
    returnTransfer :: Transfer,
    returnAttributes :: [(String, String)],
    args :: [Arg] }
    deriving Show

toCallable :: CallableInfo -> Named Callable
toCallable ci =
    let returnType = callableInfoReturnType ci
        argType = typeFromTypeInfo returnType
        ais = callableInfoArgs ci
        in toNamed ci $ Callable argType
               (callableInfoMayReturnNull ci)
               (callableInfoCallerOwns ci)
               (callableInfoReturnAttributes ci)
               (map toArg ais)

data Function = Function {
    fnSymbol :: String,
    fnCallable :: Named Callable }
    deriving Show

toFunction :: FunctionInfo -> Function
toFunction fi =
     let ci = fromBaseInfo (baseInfo fi) :: CallableInfo
      in Function (functionInfoSymbol fi) (toCallable ci)

data Signal = Signal
    deriving Show

toSignal :: SignalInfo -> Signal
toSignal _si = error "fixme"

data Property = Property {
    propName :: String,
    propType :: Type,
    propFlags :: [ParamFlag] }
    deriving Show

toProperty :: PropertyInfo -> Property
toProperty pi =
    Property (baseInfoName $ baseInfo pi)
        (typeFromTypeInfo $ propertyInfoType pi)
        (propertyInfoFlags pi)

data Field = Field {
    fieldName :: String,
    fieldType :: Type,
    fieldFlags :: [FieldInfoFlag] }
    deriving Show

toField :: FieldInfo -> Field
toField fi =
    Field (baseInfoName . baseInfo $ fi)
        (typeFromTypeInfo $ fieldInfoType fi)
        (fieldInfoFlags fi)

data Struct = Struct {
    fields :: [Field] }
    deriving Show

toStruct :: StructInfo -> Named Struct
toStruct si = toNamed si $ Struct (map toField $ structInfoFields si)

-- XXX: Capture alignment and method info.

data Union = Union {
    unionFields :: [Field] }
    deriving Show

toUnion :: UnionInfo -> Named Union
toUnion ui =
    toNamed ui $ Union (map toField $ unionInfoFields ui)

data Callback = Callback (Named Callable)
    deriving Show

data Interface = Interface {
    ifMethods :: [Function],
    ifConstants :: [Named Constant],
    ifProperties :: [Property] }
    deriving Show

toInterface :: InterfaceInfo -> Named Interface
toInterface ii =
    toNamed ii $ Interface
        (map toFunction $ interfaceInfoMethods $ ii)
        (map toConstant $ interfaceInfoConstants $ ii)
        (map toProperty $ interfaceInfoProperties $ ii)

data Object = Object {
    objFields :: [Field],
    objMethods :: [Function],
    -- objSignals :: [Signal],
    objProperties :: [Property] }
    deriving Show

toObject :: ObjectInfo -> Named Object
toObject oi =
    toNamed oi $ Object
        (map toField $ objectInfoFields oi)
        (map toFunction $ objectInfoMethods oi)
        (map toProperty $ objectInfoProperties oi)

data API
    = APIConst (Named Constant)
    | APIFunction Function
    | APICallback Callback
    -- XXX: These plus APIUnion should have their gTypes exposed (via a
    -- binding of GIRegisteredTypeInfo.
    | APIEnum (Named Enumeration)
    | APIFlags (Named Flags)
    | APIInterface (Named Interface)
    | APIObject (Named Object)
    | APIStruct (Named Struct)
    | APIUnion (Named Union)
    deriving Show

toAPI :: BaseInfoClass bi => bi -> API
toAPI i = toInfo' (baseInfoType i) (baseInfo i)
    where

    toInfo' InfoTypeConstant =
        APIConst . toConstant . fromBaseInfo
    toInfo' InfoTypeEnum =
        APIEnum . toEnumeration . fromBaseInfo
    toInfo' InfoTypeFlags =
        APIFlags . toFlags . fromBaseInfo
    toInfo' InfoTypeFunction =
        APIFunction . toFunction . fromBaseInfo
    toInfo' InfoTypeCallback =
        APICallback . Callback . toCallable . fromBaseInfo
    toInfo' InfoTypeStruct =
        APIStruct . toStruct . fromBaseInfo
    toInfo' InfoTypeUnion =
        APIUnion . toUnion . fromBaseInfo
    toInfo' InfoTypeObject =
        APIObject . toObject . fromBaseInfo
    toInfo' InfoTypeInterface =
        APIInterface . toInterface . fromBaseInfo
    toInfo' it = error $ "not expecting a " ++ show it

loadAPI :: String -> IO [API]
loadAPI name = do
    lib <- load name Nothing
    infos <- getInfos lib
    -- XXX: Work out what to do with boxed types.
    return $ map toAPI $ filter (\i -> baseInfoType i /= InfoTypeBoxed) infos

