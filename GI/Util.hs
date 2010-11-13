module GI.Util
  ( maybeWithCString
  , getList
  )
where

import Foreign
import Foreign.C

maybeWithCString :: Maybe String -> (CString -> IO a) -> IO a
maybeWithCString = maybe ($ nullPtr) withCString

getList :: (a -> IO CInt) -> (a -> CInt -> IO b) -> a -> IO [b]
getList getN getOne x = do
    n <- getN x
    mapM (getOne x) [0..n - 1]

