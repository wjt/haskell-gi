module GI.BaseInfo
  ( baseInfoName
  , baseInfoNamespace
  , baseInfoType
  )
where

import Foreign
import Foreign.C
import Foreign.C.String

import Control.Applicative ((<$>))

{# import GI.Types #}

#include <girepository.h>

{# context prefix="g_base_info" #}

{# enum GIInfoType as InfoType {underscoreToCase} with prefix="GI" deriving (Show, Eq) #}

-- Because all the C types are synonyms, c2hs picks the last one...
stupidCast :: BaseInfoClass base
           => base
           -> TypeInfo
stupidCast base = TypeInfo (castPtr p)
  where
    (BaseInfo p) = baseInfo base

baseInfoName :: BaseInfoClass base
             => base
             -> String
baseInfoName bi = unsafePerformIO $ do
    ret <- {# call g_base_info_get_name #} (stupidCast bi)
    peekCString ret

baseInfoNamespace :: BaseInfoClass base
                  => base
                  -> String
baseInfoNamespace bi = unsafePerformIO $ do
    ret <- {# call g_base_info_get_namespace #} (stupidCast bi)
    peekCString ret

baseInfoType :: BaseInfoClass base
             => base
             -> InfoType
baseInfoType bi = unsafePerformIO $ do
    toEnum . fromIntegral <$> {# call get_type #} (stupidCast bi)
