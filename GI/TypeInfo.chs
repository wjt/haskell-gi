
module GI.TypeInfo
    ( TypeTag(..)
    , typeInfoIsPointer
    , typeInfoGetTag
    , typeInfoGetParamType
    , typeInfoGetInterface
    , typeInfoGetArrayLength
    , typeInfoGetArrayFixedSize
    , typeInfoIsZeroTerminated
    , typeInfoGetNErrorDomains
    , typeInfoGetErrorDomain
    , typeInfoGetArrayType
    , typeTagToString
    )
where

import Foreign
import Foreign.C

import Control.Applicative ((<$>))

{# import GI.Types #}

#include <girepository.h>

{# context prefix="g_type_info" #}

{# enum GIArrayType as ArrayType {underscoreToCase} with prefix="GI" deriving (Show, Eq) #}
{# enum GITypeTag as TypeTag {underscoreToCase} with prefix="GI" deriving (Show, Eq) #}

-- Because all the C types are synonyms, c2hs picks the last one...
stupidCast :: TypeInfoClass base => base -> Ptr ()
stupidCast ti = castPtr p
  where (TypeInfo p) = typeInfo ti

typeInfoIsPointer :: TypeInfoClass tic => tic -> Bool
typeInfoIsPointer ti = unsafePerformIO $
    (== 0) <$> {# call is_pointer #} (stupidCast ti)

typeInfoGetTag :: TypeInfoClass tic => tic -> TypeTag
typeInfoGetTag ti = unsafePerformIO $ toEnum . fromIntegral <$>
    {# call get_tag #} (stupidCast ti)

typeInfoGetParamType :: TypeInfoClass tic => tic -> Int -> TypeInfo
typeInfoGetParamType ti n = unsafePerformIO $ TypeInfo <$> castPtr <$>
    {# call get_param_type #} (stupidCast ti) (fromIntegral n)

typeInfoGetInterface :: TypeInfoClass tic => tic -> BaseInfo
typeInfoGetInterface ti = unsafePerformIO $ baseInfo <$>
    {# call get_interface #} (stupidCast ti)

typeInfoGetArrayLength :: TypeInfoClass tic => tic -> Int
typeInfoGetArrayLength ti = unsafePerformIO $ fromIntegral <$>
    {# call get_array_length #} (stupidCast ti)

typeInfoGetArrayFixedSize :: TypeInfoClass tic => tic -> Int
typeInfoGetArrayFixedSize ti = unsafePerformIO $ fromIntegral <$>
    {# call get_array_fixed_size #} (stupidCast ti)

typeInfoIsZeroTerminated :: TypeInfoClass tic => tic -> Bool
typeInfoIsZeroTerminated ti = unsafePerformIO $ (== 0) <$>
    {# call is_zero_terminated #} (stupidCast ti)

typeInfoGetNErrorDomains :: TypeInfoClass tic => tic -> Int
typeInfoGetNErrorDomains ti = unsafePerformIO $ fromIntegral <$>
    {# call get_n_error_domains #} (stupidCast ti)

typeInfoGetErrorDomain :: TypeInfoClass tic => tic -> Int -> ErrorDomainInfo
typeInfoGetErrorDomain ti n = unsafePerformIO $ ErrorDomainInfo <$> castPtr <$>
    {# call get_error_domain #} (stupidCast ti) (fromIntegral n)

typeInfoGetArrayType :: TypeInfoClass tic => tic -> ArrayType
typeInfoGetArrayType ti = unsafePerformIO $ toEnum <$> fromIntegral <$>
    {# call get_array_type #} (stupidCast ti)

typeTagToString :: TypeTag -> String
typeTagToString tag = unsafePerformIO $ peekCString =<<
    {# call g_type_tag_to_string #} (fromIntegral $ fromEnum tag)
