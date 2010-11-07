module GI.Typelib
  ( repositoryGetNInfos
  , repositoryGetInfo
  , repositoryRequire
  , repositoryLoad
  )
where

import Foreign
import Foreign.C

import System.Glib.GError

import GI.Types

#include <girepository.h>

{# pointer *GITypelib as Typelib newtype #}
{# pointer *GIRepository as Repository newtype #}

getDefaultRepository :: IO Repository
getDefaultRepository = {# call g_irepository_get_default #}

nullRepository = Repository nullPtr

repositoryGetNInfos :: String -> IO CInt
repositoryGetNInfos ns = withCString ns $ \nsPtr ->
    {# call unsafe g_irepository_get_n_infos #} nullRepository nsPtr

repositoryGetInfo :: String -> CInt -> IO BaseInfo
repositoryGetInfo ns i = withCString ns $ \nsPtr -> do
    ret <- {# call unsafe g_irepository_get_info #} nullRepository nsPtr (fromIntegral i)
    return $ BaseInfo $ castPtr ret

repositoryRequire :: String -> IO Typelib
repositoryRequire ns = --version =
    withCString ns $ \nsPtr ->
--    withCString version $ \versionPtr ->
    propagateGError $ {# call unsafe g_irepository_require #} nullRepository nsPtr nullPtr 0

repositoryLoad :: Typelib -> IO String
repositoryLoad typelib = do
    ret <- propagateGError $ {# call unsafe g_irepository_load_typelib #} nullRepository typelib 0
    peekCString ret

