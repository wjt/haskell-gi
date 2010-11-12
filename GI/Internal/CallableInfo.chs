
module GI.Internal.CallableInfo
    ( callableInfoReturnType
    , callableInfoCallerOwns
    , callableInfoMayReturnNull
    , callableInfoReturnAttributes
    , callableInfoArgs
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM)
import Data.Word (Word64)
import Foreign
import Foreign.C

import GI.Internal.ArgInfo

{# import GI.Internal.Types #}

#include <girepository.h>

{# context prefix="g_callable_info" #}

stupidCast :: CallableInfoClass call => call -> Ptr ()
stupidCast call = castPtr p
    where (CallableInfo p) = callableInfo call

callableInfoReturnType :: CallableInfoClass call => call -> TypeInfo
callableInfoReturnType ci = unsafePerformIO $ TypeInfo <$> castPtr <$>
    {# call get_return_type #} (stupidCast ci)

callableInfoCallerOwns :: CallableInfoClass call => call -> Transfer
callableInfoCallerOwns ci = unsafePerformIO $ toEnum <$> fromIntegral <$>
    {# call get_caller_owns #} (stupidCast ci)

callableInfoMayReturnNull :: CallableInfoClass call => call -> Bool
callableInfoMayReturnNull ci = unsafePerformIO $ (== 0) <$>
    {# call may_return_null #} (stupidCast ci)

callableInfoReturnAttributes :: CallableInfoClass call => call -> [(String, String)]
callableInfoReturnAttributes ci = unsafePerformIO $ do
    allocaBytes {# sizeof GIAttributeIter #} $ \iter -> loop iter []

    where loop iter acc = do
              name <- new nullPtr
              value <- new nullPtr
              let iter' = AttributeIter (castPtr iter)
              ok <- {# call iterate_return_attributes #}
                  (stupidCast ci) iter' name value
              if ok == 0 then return acc
                  else do
                      name' <- peekCString =<< peek name
                      value' <- peekCString =<< peek value
                      loop iter ((name', value') : acc)

callableInfoArgs :: CallableInfoClass call => call -> [ArgInfo]
callableInfoArgs ci = unsafePerformIO $ do
    n <- fromIntegral <$> {# call get_n_args #} (stupidCast ci)
    forM [0..n - 1] $ \i ->
        ArgInfo <$> castPtr <$>
            {# call get_arg #} (stupidCast ci) (fromIntegral i)

