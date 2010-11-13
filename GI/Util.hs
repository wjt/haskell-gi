module GI.Util
  ( maybeWithCString
  , getList
  , toFlags
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

-- XXX: Assumes all enum values are 0 mod 2^n.
toFlags :: Enum a => CInt -> [a]
toFlags n = loop n (0 :: Int)
    where loop 0 _ = []
          loop n e =
              let rest = loop (n `shiftR` 1) (e + 1)
               in if testBit e 0 then toEnum (2 ^ e) : rest else rest

