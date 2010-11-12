
module GI.Info (Info(..), dumpInfo) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Int
import Data.Word
import GI.ArgInfo
import GI.BaseInfo
import GI.CallableInfo
import GI.ConstantInfo
import GI.EnumInfo
import GI.TypeInfo
import GI.Types
import GI.Typelib (getInfos, load)
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

data Info
    = Enum { name :: String, values :: [(String, Word64)] }
    | Const { name :: String, value :: Value }
    | Object {
        name :: String,
        methods :: [Callable],
        signals :: [Callable],
        properties :: [String] }
    | Function Callable
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

toInfo :: BaseInfoClass bi => bi -> [Info]
toInfo i = toInfo' (baseInfoType i)
    where

    name = baseInfoName i

    toInfo' InfoTypeType = error "!?"

    toInfo' InfoTypeConstant =
        let ci = fromBaseInfo (baseInfo i) :: ConstantInfo
            typeInfo = constantInfoType ci
            arg = constantInfoValue ci
            value = fromArgument typeInfo arg
         in [Const name value]

    toInfo' InfoTypeEnum =
        let ei = fromBaseInfo (baseInfo i) :: EnumInfo
            n = enumInfoNValues ei
            vis = map (enumInfoValue ei) [0..n - 1]
            names = map (baseInfoName . baseInfo) vis
            values = map valueInfoValue vis
         in [Enum name (zip names values)]

    toInfo' InfoTypeFunction =
        let ci = fromBaseInfo (baseInfo i) :: CallableInfo
         in [Function $ toCallable ci]

    -- toInfo' InfoTypeObject = 
    -- toInfo' InfoTypeStruct = 

    toInfo' _ = []

dumpInfo name = do
    lib <- load name Nothing
    infos <- getInfos lib
    let infos' = map toInfo infos
    -- forM_ infos $ \info -> print (baseInfoName info, baseInfoType info)
    -- mapM_ print infos
    -- print $ (toEnum 0 :: TypeTag)
    forM_ (zip infos infos') $ \(i, i') -> do
        print (baseInfoType i, baseInfoName i)
        print i'
        putStrLn ""

