
module GI.API
    ( API(..)
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
    , loadAPI
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Int
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
import GI.Internal.TypeInfo
import GI.Internal.Typelib (getInfos, load)
import GI.Value

data Constant = Constant {
    constName :: String,
    constValue :: Value }
    deriving Show

toConstant :: ConstantInfo -> Constant
toConstant ci =
    let name = baseInfoName $ baseInfo ci
        typeInfo = constantInfoType ci
        arg = constantInfoValue ci
        value = fromArgument typeInfo arg
     in Constant name value

data Enumeration = Enumeration {
    enumName :: String,
    enumValues :: [(String, Word64)] }
    deriving Show

toEnumeration :: EnumInfo -> Enumeration
toEnumeration ei =
    Enumeration (baseInfoName . baseInfo $ ei)
        (map (\vi -> (baseInfoName . baseInfo $ vi, valueInfoValue vi))
            (enumInfoValues ei))

data Flags = Flags Enumeration
    deriving Show

toFlags :: EnumInfo -> Flags
toFlags = Flags . toEnumeration

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
    callableName :: String,
    returnType :: Type,
    returnMayBeNull :: Bool,
    returnTransfer :: Transfer,
    returnAttributes :: [(String, String)],
    args :: [Arg] }
    deriving Show

toCallable :: CallableInfo -> Callable
toCallable ci =
    let returnType = callableInfoReturnType ci
        argType = typeFromTypeInfo returnType
        ais = callableInfoArgs ci
        name = baseInfoName . baseInfo $ ci
        in Callable name argType
               (callableInfoMayReturnNull ci)
               (callableInfoCallerOwns ci)
               (callableInfoReturnAttributes ci)
               (map toArg ais)

data Function = Function {
    fnSymbol :: String,
    fnCallable :: Callable }
    deriving Show

toFunction :: FunctionInfo -> Function
toFunction fi =
     let ci = fromBaseInfo (baseInfo fi) :: CallableInfo
      in Function (functionInfoSymbol fi) (toCallable ci)

data Signal = Signal
    deriving Show

toSignal :: SignalInfo -> Signal
toSignal si = error "fixme"

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
    structName :: String,
    fields :: [Field] }
    deriving Show

toStruct :: StructInfo -> Struct
toStruct si =
    Struct (baseInfoName . baseInfo $ si)
        (map toField $ structInfoFields si)

data Callback = Callback Callable
    deriving Show

data Interface = Interface {
    ifName :: String,
    ifMethods :: [Function],
    ifConstants :: [Constant],
    ifProperties :: [Property] }
    deriving Show

toInterface :: InterfaceInfo -> Interface
toInterface ii =
    Interface (baseInfoName . baseInfo $ ii)
        (map toFunction $ interfaceInfoMethods $ ii)
        (map toConstant $ interfaceInfoConstants $ ii)
        (map toProperty $ interfaceInfoProperties $ ii)

data Object = Object {
    objName :: String,
    objFields :: [Field],
    objMethods :: [Function],
    -- objSignals :: [Signal],
    objProperties :: [Property] }
    deriving Show

toObject :: ObjectInfo -> Object
toObject oi =
    Object (baseInfoName . baseInfo $ oi)
        (map toField $ objectInfoFields oi)
        (map toFunction $ objectInfoMethods oi)
        (map toProperty $ objectInfoProperties oi)

data API
    = APIConst Constant
    | APIFunction Function
    | APICallback Callback
    -- XXX: These plus APIUnion should have their gTypes exposed (via a
    -- binding of GIRegisteredTypeInfo.
    | APIEnum Enumeration
    | APIFlags Flags
    | APIInterface Interface
    | APIObject Object
    | APIStruct Struct
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

