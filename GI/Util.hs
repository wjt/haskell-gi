module GI.Util
  ( maybeWithCString
  )
where

import Foreign
import Foreign.C

maybeWithCString :: Maybe String -> (CString -> IO a) -> IO a
maybeWithCString = maybe ($ nullPtr) withCString
