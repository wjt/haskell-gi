
module GI.TypeInfo
    ( typeInfoType
    , typeInfoValue
    )
where

import Foreign
import Foreign.C

{# import GI.Types #}

#include <girepository.h>

{# context prefix="g_type_info" #}

-- Because all the C types are synonyms, c2hs picks the last one...
stupidCast :: BaseInfoClass base
           => base
           -> TypeInfo
stupidCast base = TypeInfo (castPtr p)
  where
    (BaseInfo p) = baseInfo base

typeInfoIsPointer :: TypeInfoClass tic => tic -> Bool
typeInfoIsPointer ti = unsafePerformIO $
    {# call g_type_info_is_pointer #} (stupidCast ti)
