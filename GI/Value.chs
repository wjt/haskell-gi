
module GI.Value
    ( BasicType(..)
    , Type(..)
    , Value(..)
    , typeFromTypeInfo
    , fromArgument
    , valueType
    ) where

import Control.Applicative ((<$>))
import Data.Int
import Data.Word
import Foreign
import Foreign.C

import GI.Internal.BaseInfo
import GI.Internal.TypeInfo
import GI.Internal.Types

#include <girepository.h>

-- This enum mirrors the definition in gitypes.h.
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
     | TUniChar
     | TGType
     | TUTF8
     | TFileName
    deriving (Eq, Enum, Show)

data Type
    = TBasicType BasicType
    | TArray Type
    | TInterface String
    | TGList Type
    | TGSList Type
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

valueType :: Value -> Type
valueType VVoid           = TBasicType TVoid
valueType (VBoolean _)    = TBasicType TBoolean
valueType (VInt8 _)       = TBasicType TInt8
valueType (VUInt8 _)      = TBasicType TUInt8
valueType (VInt16 _)      = TBasicType TInt16
valueType (VUInt16 _)     = TBasicType TUInt16
valueType (VInt32 _)      = TBasicType TInt32
valueType (VUInt32 _)     = TBasicType TUInt32
valueType (VInt64 _)      = TBasicType TInt64
valueType (VUInt64 _)     = TBasicType TUInt64
valueType (VFloat _)      = TBasicType TFloat
valueType (VDouble _)     = TBasicType TDouble
valueType (VGType _)      = TBasicType TGType
valueType (VUTF8 _)       = TBasicType TUTF8
valueType (VFileName _)   = TBasicType TFileName

typeTagIsBasic TypeTagVoid = True
typeTagIsBasic TypeTagBoolean = True
typeTagIsBasic TypeTagInt8 = True
typeTagIsBasic TypeTagInt16 = True
typeTagIsBasic TypeTagInt32 = True
typeTagIsBasic TypeTagInt64 = True
typeTagIsBasic TypeTagUint8 = True
typeTagIsBasic TypeTagUint16 = True
typeTagIsBasic TypeTagUint32 = True
typeTagIsBasic TypeTagUint64 = True
typeTagIsBasic TypeTagFloat = True
typeTagIsBasic TypeTagDouble = True
typeTagIsBasic TypeTagUnichar = True
typeTagIsBasic TypeTagUtf8 = True
typeTagIsBasic TypeTagFilename = True
typeTagIsBasic _ = False

basicTypeFromTypeTag TypeTagVoid = Just TVoid
basicTypeFromTypeTag TypeTagBoolean = Just TBoolean
basicTypeFromTypeTag TypeTagInt8 = Just TInt8
basicTypeFromTypeTag TypeTagInt16 = Just TInt16
basicTypeFromTypeTag TypeTagInt32 = Just TInt32
basicTypeFromTypeTag TypeTagInt64 = Just TInt64
basicTypeFromTypeTag TypeTagUint8 = Just TUInt8
basicTypeFromTypeTag TypeTagUint16 = Just TUInt16
basicTypeFromTypeTag TypeTagUint32 = Just TUInt32
basicTypeFromTypeTag TypeTagUint64 = Just TUInt64
basicTypeFromTypeTag TypeTagFloat = Just TFloat
basicTypeFromTypeTag TypeTagDouble = Just TDouble
basicTypeFromTypeTag TypeTagUnichar = Just TUniChar
basicTypeFromTypeTag TypeTagUtf8 = Just TUTF8
basicTypeFromTypeTag TypeTagFilename = Just TFileName
basicTypeFromTypeTag _ = Nothing

typeFromTypeInfo :: TypeInfo -> Type
typeFromTypeInfo ti =
    case basicTypeFromTypeTag tag of
      Just bt -> TBasicType bt
      Nothing -> case tag of
           TypeTagArray -> TArray p1
           -- TypeTagInterface -> TInterface (typeTagToString . typeInfoTag $ ti)
           TypeTagInterface -> TInterface $
               baseInfoName . baseInfo . typeInfoInterface $ ti
           TypeTagGlist -> TGList p1
           TypeTagGslist -> TGSList p1
           TypeTagGhash -> TGHash p1 p2
           -- XXX: Include more information.
           TypeTagError -> TError
           _ -> error $ "implement me: " ++ show (tag, fromEnum tag, fromEnum TypeTagArray)

    where tag = typeInfoTag ti
          p1 = typeFromTypeInfo $ typeInfoParamType ti 0
          p2 = typeFromTypeInfo $ typeInfoParamType ti 1

fromArgument :: TypeInfo -> Argument -> Value
fromArgument typeInfo (Argument arg) =
    case typeFromTypeInfo typeInfo of
        TBasicType t -> unsafePerformIO $ basic t

    where

    basic TInt32 = VInt32 <$> fromIntegral <$> {# get GIArgument->v_int32 #} arg
    -- XXX: Loss of precision?
    basic TDouble = VDouble <$> fromRational <$> toRational <$>  {# get GIArgument->v_double #} arg
    basic TUTF8 = VUTF8 <$> (peekCString =<< {# get GIArgument->v_string #} arg)
    basic t = error $ "a: implement me: " ++ show t

