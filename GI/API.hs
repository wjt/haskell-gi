
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
import GI.Internal.TypeInfo
import GI.Internal.Typelib (getInfos, load)
import GI.Value

data Arg = Arg {
    argName :: String,
    type_ :: Type,
    direction :: Direction,
    scope :: Scope,
    transfer :: Transfer }
    deriving Show

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
        type_ = typeFromTypeInfo returnType
        ais = callableInfoArgs ci
        name = baseInfoName . baseInfo $ ci
        in Callable name type_
               (callableInfoMayReturnNull ci)
               (callableInfoCallerOwns ci)
               (callableInfoReturnAttributes ci)
               [Arg (baseInfoName . baseInfo $ ai)
                    (typeFromTypeInfo . argInfoType $ ai)
                    (argInfoDirection ai)
                    (argInfoScope ai)
                    (argInfoOwnershipTransfer ai)
                   | ai <- ais]

data Function = Function
    deriving Show

toFunction :: FunctionInfo -> Function
toFunction fi = error "fixme"

data Signal = Signal
    deriving Show

toSignal :: SignalInfo -> Signal
toSignal si = error "fixme"

data Interface = Interface {
    ifName :: String,
    ifMethods :: [Function] }
    deriving Show

toInterface :: InterfaceInfo -> Interface
toInterface ii = error "fixme"

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
    | APIConst { name :: String, value :: Value }
    | APIObject Object
    -- XXX
    | APIFunction Callable
    deriving Show

toAPI :: BaseInfoClass bi => bi -> [API]
toAPI i = toInfo' (baseInfoType i)
    where

    name = baseInfoName i

    toInfo' InfoTypeType = error "!?"

    toInfo' InfoTypeConstant =
        let ci = fromBaseInfo (baseInfo i) :: ConstantInfo
            typeInfo = constantInfoType ci
            arg = constantInfoValue ci
            value = fromArgument typeInfo arg
         in [APIConst name value]

    toInfo' InfoTypeEnum =
        let ei = fromBaseInfo (baseInfo i) :: EnumInfo
            n = enumInfoNValues ei
            vis = map (enumInfoValue ei) [0..n - 1]
            names = map (baseInfoName . baseInfo) vis
            values = map valueInfoValue vis
         in [APIEnum name (zip names values)]

    toInfo' InfoTypeFunction =
        let ci = fromBaseInfo (baseInfo i) :: CallableInfo
         in [APIFunction $ toCallable ci]

    -- toInfo' InfoTypeSignal = 
    -- toInfo' InfoTypeObject = 
    -- toInfo' InfoTypeInterface = 

    toInfo' _ = []

dumpAPI name = do
    lib <- load name Nothing
    infos <- getInfos lib
    let apis = map toAPI infos
    forM_ (zip infos apis) $ \(info, api) -> do
        print (baseInfoType info, baseInfoName info)
        print api
        putStrLn ""

