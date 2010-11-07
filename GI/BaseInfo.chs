module GI.BaseInfo
  ( baseInfoGetName
  )
where

import Foreign
import Foreign.C
import Foreign.C.String
{# import GI.Types #}

#include <girepository.h>

baseInfoGetName :: BaseInfo -> IO String
baseInfoGetName (BaseInfo p) = do
    ret <- {# call g_base_info_get_name #} $ TypeInfo $ castPtr p
    peekCString ret
