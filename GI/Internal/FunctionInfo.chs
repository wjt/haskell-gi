
module GI.Internal.FunctionInfo
    ( functionInfoSymbol
    -- , functionInfoFlags,
    -- , functionInfoProperty
    -- , functionInfoVFunc
    )
where

import Control.Applicative ((<$>))
import Foreign
import Foreign.C

{# import GI.Internal.Types #}

#include <girepository.h>

{# context prefix="g_function_info" #}

-- Because all the C types are synonyms, c2hs picks the last one...
stupidCast :: FunctionInfoClass fic => fic -> Ptr ()
stupidCast fi = castPtr p
  where (FunctionInfo p) = functionInfo fi

functionInfoSymbol :: FunctionInfoClass fic => fic -> String
functionInfoSymbol fi = unsafePerformIO $ peekCString =<<
    {# call get_symbol #} (stupidCast fi)
