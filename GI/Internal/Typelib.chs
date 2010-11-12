module GI.Internal.Typelib
  ( getSearchPath

  , load
  , getInfos
  )
where

import Foreign
import Foreign.C

import Control.Applicative ((<$>))
import Control.Monad (when, forM)

import System.Glib.GError
import System.Glib.GList

import GI.Internal.BaseInfo
import GI.Internal.Types
import GI.Util

#include <girepository.h>

{#context prefix="g_irepository"#}

{# pointer *GITypelib as Typelib newtype #}
unTypelib :: Typelib -> Ptr Typelib
unTypelib (Typelib p) = p

{# pointer *GIRepository as Repository newtype #}

nullRepository = Repository nullPtr

getSearchPath :: IO [FilePath]
getSearchPath = do
    paths <- {# call unsafe get_search_path #}
    pathPtrs <- readGSList paths
    mapM peekCString pathPtrs

getInfos :: Typelib -> IO [BaseInfo]
getInfos typelib = do
    nsPtr <- {# call unsafe g_typelib_get_namespace #} typelib
    n <- {# call unsafe get_n_infos #} nullRepository nsPtr
    forM [0..n-1] $ \i -> do
        ret <- {# call unsafe get_info #} nullRepository nsPtr (fromIntegral i)
        return $ BaseInfo $ castPtr ret

load :: String -> Maybe String -> IO Typelib
load namespace version =
    withCString namespace $ \nsPtr ->
    maybeWithCString version $ \versionPtr ->
    propagateGError $ \gError -> do
        -- _require()'s return is annotated as 'transfer none'. I'm assuming
        -- that we don't need to ref this because it's never going to be freed,
        -- though, so we're fine.
        typelib <- {# call unsafe require #} nullRepository nsPtr versionPtr 0
                                             gError
        when (unTypelib typelib /= nullPtr) $ do
            _ <- {# call unsafe load_typelib #} nullRepository typelib 0 gError
            return ()
        return typelib
