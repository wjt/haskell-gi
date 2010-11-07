module GI.Typelib
  ( repositoryGetNInfos
  , repositoryRequire
  , repositoryLoad
  )
where

import Foreign
import Foreign.C

import System.Glib.GError

#include <girepository.h>

{# pointer *GITypelib as Typelib newtype #}
{# pointer *GIRepository as Repository newtype #}

getDefaultRepository :: IO Repository
getDefaultRepository = {# call g_irepository_get_default #}

nullRepository = Repository nullPtr

repositoryGetNInfos :: String -> IO CInt
repositoryGetNInfos ns = withCString ns $ \nsPtr ->
    {# call unsafe g_irepository_get_n_infos #} nullRepository nsPtr

repositoryRequire :: String -> IO Typelib
repositoryRequire ns = --version =
    withCString ns $ \nsPtr ->
--    withCString version $ \versionPtr ->
    propagateGError $ {# call unsafe g_irepository_require #} nullRepository nsPtr nullPtr 0

repositoryLoad :: Typelib -> IO String
repositoryLoad typelib = do
    ret <- propagateGError $ {# call unsafe g_irepository_load_typelib #} nullRepository typelib 0
    peekCString ret
