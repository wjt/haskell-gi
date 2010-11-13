
module GI.Internal.PropertyInfo
    ( ParamFlag(..)
    , propertyInfoFlags
    , propertyInfoType
    )
where

import Control.Applicative ((<$>))
import Data.Bits
import Foreign
import Foreign.C

import GI.Internal.ParamFlag

{# import GI.Internal.Types #}

#include <girepository.h>

{# context prefix="g_property_info" #}

{-
This doesn't work; see ParamFlag.hs.

{# enum GParamFlags as ParamFlag {underscoreToCase} with prefix="G"
    deriving (Show, Eq) #}
-}

-- Because all the C types are synonyms, c2hs picks the last one...
stupidCast :: PropertyInfoClass pic => pic -> Ptr ()
stupidCast pi = castPtr p
  where (PropertyInfo p) = propertyInfo pi

toParamFlags :: CInt -> [ParamFlag]
toParamFlags n = loop n (0 :: Int)
    where loop 0 _ = []
          loop n e =
              let rest = loop (n `shiftR` 1) (e + 1)
               -- XXX: Assumes all enum values are 0 mod 2^n.
               in if testBit e 0 then toEnum (2 ^ e) : rest else rest

propertyInfoFlags :: PropertyInfoClass pic => pic -> [ParamFlag]
propertyInfoFlags pi = unsafePerformIO $ toParamFlags <$>
    {# call get_flags #} (stupidCast pi)

propertyInfoType :: PropertyInfoClass pic => pic -> TypeInfo
propertyInfoType pi = unsafePerformIO $ TypeInfo <$> castPtr <$>
    {# call get_type #} (stupidCast pi)

