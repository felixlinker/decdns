module Util.Bytes
  ( toHex
  , toB64 ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C
import Data.Char (toUpper)
import Numeric (showHex)

toHex :: BS.ByteString -> String
toHex = concatMap (map toUpper . pad . (`showHex` "")) . BS.unpack
  where
    pad [x] = ['0', x]
    pad xs = xs

toB64 :: BS.ByteString -> String
toB64 = C.unpack . B64.encode
