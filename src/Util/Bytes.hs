module Util.Bytes
  ( Serialize(..)
  , word8
  , word16
  , word32
  , chunk
  , withLength
  , toHex
  , fromHex
  , fromB64
  , toB64
  , fromB32
  , toB32 ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base32.Hex as B32Hex
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BBS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C
import Data.Word (Word8)
import Data.Char (toUpper, digitToInt, isHexDigit)
import Data.IP (IPv4, IPv6, fromIPv4w, fromIPv6w)
import Control.Monad (unless)
import Numeric (showHex)

build :: BBS.Builder -> BS.ByteString
build = LBS.toStrict . BBS.toLazyByteString

class Serialize a where
  serialize :: a -> BS.ByteString

instance Serialize () where
  serialize _ = BS.empty

instance Serialize IPv4 where
  serialize = word32 . fromIPv4w

instance Serialize BS.ByteString where
  serialize = id

instance Serialize IPv6 where
  serialize addr =
    let (w0, w1, w2, w3) = fromIPv6w addr
    in build $ foldr ((<>) . BBS.word32BE) mempty [w0, w1, w2, w3]

word8 :: Integral a => a -> BS.ByteString
word8 = BS.singleton . fromIntegral

word16 :: Integral a => a -> BS.ByteString
word16 = build . BBS.word16BE . fromIntegral

word32 :: Integral a => a -> BS.ByteString
word32 = build . BBS.word32BE . fromIntegral

chunk :: Int -> BS.ByteString -> [BS.ByteString]
chunk size bytes
  | BS.null bytes = []
  | otherwise =
      let (prefix, suffix) = BS.splitAt size bytes
      in prefix : chunk size suffix

withLength :: BS.ByteString -> BS.ByteString
withLength bs = BS.cons (fromIntegral (BS.length bs)) bs

toHex :: BS.ByteString -> String
toHex = concatMap (map toUpper . pad . (`showHex` "")) . BS.unpack
  where
    pad [x] = ['0', x]
    pad xs = xs

fromHex :: MonadFail m => String -> m BS.ByteString
fromHex "" = return BS.empty
fromHex (hi:lo:rest) = do
  unless (isHexDigit hi && isHexDigit lo) (fail "illegal input")
  BS.cons hexPair <$> fromHex rest
  where
    hexPair :: Word8
    hexPair = fromIntegral (16 * digitToInt (toUpper hi) + digitToInt (toUpper lo))
fromHex _ = fail "fromHex called with odd-length input"

fromB64 :: MonadFail m => String -> m BS.ByteString
fromB64 = either (const (fail "illegal b64")) return . B64.decode . C.pack

toB64 :: BS.ByteString -> String
toB64 = C.unpack . B64.encode

fromB32 :: MonadFail m => String -> m BS.ByteString
fromB32 = either (const (fail "illegal b32")) return . B32Hex.decodeBase32Unpadded . C.pack . map toUpper

toB32 :: BS.ByteString -> String
toB32 = C.unpack . B32Hex.encodeBase32Unpadded'
