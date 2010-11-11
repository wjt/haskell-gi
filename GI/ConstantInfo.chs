
module GI.ConstantInfo
    ( constantInfoType
    , constantInfoValue
    )
where

import Control.Applicative ((<$>))
import Foreign
import Foreign.C

{# import GI.Types #}

#include <girepository.h>

{# context prefix="g_constant_info" #}

-- Because all the C types are synonyms, c2hs picks the last one...
stupidCast :: ConstantInfoClass base => base -> Ptr ()
stupidCast base = castPtr p
  where (ConstantInfo p) = constantInfo base

constantInfoType :: ConstantInfoClass const => const -> TypeInfo
constantInfoType ci = unsafePerformIO $ TypeInfo <$> castPtr <$>
    {# call get_type #} (stupidCast ci)

constantInfoValue :: ConstantInfoClass const => const -> Argument
constantInfoValue ci = unsafePerformIO $ do
    ptr <- mallocBytes {# sizeof GIArgument #}
    -- XXX: We ignore the return value here. Do we need it for anything?
    {# call get_value #} (stupidCast ci) (Argument ptr)
    return (Argument ptr)
