
module GI.Value
    ( BasicType(..)
    , Type(..)
    , Value(..)
    , typeFromTypeInfo
    , fromArgument
    ) where

import Control.Applicative ((<$>))
import Data.Int
import Data.Word
import Foreign
import Foreign.C

import GI.BaseInfo
import GI.Types
import GI.TypeInfo

#include <girepository.h>

data BasicType
     = TVoid
     | TBoolean
     | TInt8
     | TUInt8
     | TInt16
     | TUInt16
     | TInt32
     | TUInt32
     | TInt64
     | TUInt64
     | TFloat
     | TDouble
     | TGType
     | TUTF8
     | TFileName
    deriving (Eq, Enum, Show)

data Type
    = TBasicType BasicType
    | TArray Type
    | TInterface String
    | TGList
    | TGSList
    | TGHash Type Type
    | TError
    deriving (Eq, Show)

data Value
    = VVoid
    | VBoolean Bool
    | VInt8 Int8
    | VUInt8 Word8
    | VInt16 Int16
    | VUInt16 Word16
    | VInt32 Int32
    | VUInt32 Word32
    | VInt64 Int64
    | VUInt64 Word64
    | VFloat Float
    | VDouble Double
    | VGType Word32
    | VUTF8 String
    | VFileName String
    deriving (Eq, Show)

typeFromTypeInfo :: TypeInfo -> Type
typeFromTypeInfo ti =
    if fromEnum tag < fromEnum TypeTagArray
        then TBasicType $ toEnum $ fromEnum tag
        else case tag of
                 TypeTagArray -> TArray p1
                 TypeTagGhash -> TGHash p1 p2
                 -- TypeTagInterface -> TInterface (baseInfoName . baseInfo $ ti)
                 TypeTagInterface -> TInterface (typeTagToString . typeInfoTag $ ti)
                 _ -> error $ "implement me: " ++ show tag

    where tag = typeInfoTag ti
          p1 = typeFromTypeInfo $ typeInfoParamType ti 0
          p2 = typeFromTypeInfo $ typeInfoParamType ti 0

fromArgument :: TypeInfo -> Argument -> Value
fromArgument typeInfo (Argument arg) =
    case typeFromTypeInfo typeInfo of
        TBasicType t -> unsafePerformIO $ basic t

    where

    basic TInt32 = VInt32 <$> fromIntegral <$> {# get GIArgument->v_int32 #} arg
    basic TUTF8 = VUTF8 <$> (peekCString =<< {# get GIArgument->v_string #} arg)
    basic t = error $ "implement me: " ++ show t

