
module GI.Internal.EnumInfo
    ( enumInfoNValues
    , enumInfoValue
    , valueInfoValue
    ) where

import Control.Applicative ((<$>))
import Data.Word (Word64)
import Foreign
import Foreign.C

{# import GI.Internal.Types #}

#include <girepository.h>

{# context prefix="g_enum_info" #}

stupidEnumCast :: EnumInfoClass enum => enum -> Ptr ()
stupidEnumCast enum = castPtr p
    where (EnumInfo p) = enumInfo enum

stupidValueCast :: ValueInfoClass val => val -> Ptr ()
stupidValueCast val = castPtr p
    where (ValueInfo p) = valueInfo val

enumInfoNValues :: EnumInfoClass enum => enum -> Int
enumInfoNValues ei = unsafePerformIO $ fromIntegral <$>
    {# call get_n_values #} (stupidEnumCast ei)

enumInfoValue :: EnumInfoClass enum => enum -> Int -> ValueInfo
enumInfoValue ei n = unsafePerformIO $ ValueInfo <$> castPtr <$>
    {# call get_value #} (stupidEnumCast ei) (fromIntegral n)

valueInfoValue :: ValueInfoClass val => val -> Word64
valueInfoValue vi = unsafePerformIO $ fromIntegral <$>
    {# call g_value_info_get_value #} (stupidValueCast vi)
