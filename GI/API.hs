
module GI.API (API(..), dumpAPI) where

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
import GI.Internal.FunctionInfo
import GI.Internal.InterfaceInfo
import GI.Internal.PropertyInfo
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
    objMethods :: [Function],
    objSignals :: [Signal],
    objProperties :: [String] }
    deriving Show

toObject :: ObjectInfo -> Object
toObject oi = error "fixme"

data API
    = APIEnum { name :: String, values :: [(String, Word64)] }
    | APIConst Constant
    | APIObject Object
    | APIFunction Function
    | APIInterface Interface
    deriving Show

toAPI :: BaseInfoClass bi => bi -> [API]
toAPI i = toInfo' (baseInfoType i)
    where

    name = baseInfoName i

    toInfo' InfoTypeType = error "!?"

    toInfo' InfoTypeConstant =
        let ci = fromBaseInfo (baseInfo i) :: ConstantInfo
         in [APIConst $ toConstant ci]

    toInfo' InfoTypeEnum =
        let ei = fromBaseInfo (baseInfo i) :: EnumInfo
            vis = enumInfoValues ei
            names = map (baseInfoName . baseInfo) vis
            values = map valueInfoValue vis
         in [APIEnum name (zip names values)]

    toInfo' InfoTypeFunction =
        let fi = fromBaseInfo (baseInfo i) :: FunctionInfo
         in [APIFunction $ toFunction fi]

    -- toInfo' InfoTypeSignal = 
    -- toInfo' InfoTypeObject = 

    toInfo' InfoTypeInterface =
        let ii = fromBaseInfo (baseInfo i) :: InterfaceInfo
         in [APIInterface $ toInterface ii]

    toInfo' _ = []

dumpAPI name = do
    lib <- load name Nothing
    infos <- getInfos lib
    let apis = map toAPI infos
    forM_ (zip infos apis) $ \(info, api) -> do
        print (baseInfoType info, baseInfoName info)
        print api
        putStrLn ""

