{-# LANGUAGE FlexibleContexts #-}
module Parsing.Bytes
  ( base64
  , base64WithSep
  , base32Hex
  , hex
  , hexWithSep) where

import qualified Data.ByteString as BS
import Data.Char (digitToInt, isHexDigit, toUpper)
import Data.Word
import Text.Parsec
import Text.Parsec.Char as Char

import Parsing.State
import Util.Bytes ( fromB64, fromB32 )

base64c :: Stream s m Char => ParsecT s u m Char
base64c = Char.oneOf $! ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+', '/', '=']

-- NSEC3 records omit '=', so do we. Also, we parse characters as case
-- insenstive.
base32HexC :: Stream s m Char => ParsecT s u m Char
base32HexC = Char.oneOf $! ['A'..'V'] ++ ['a'..'v'] ++ ['0'..'9']

base64s :: Stream s m Char => ParsecT s u m String
base64s = many1 base64c

hexs :: Stream s m Char => ParsecT s u m String
hexs = many1 (satisfy isHexDigit)

b64dec :: String -> ParsecT s u m BS.ByteString
b64dec = maybe (fail "could not parse base64") return . fromB64

b32dec :: String -> ParsecT s u m BS.ByteString
b32dec = maybe (fail "could not parse b32") return . fromB32

hexdec :: String -> ParsecT s u m BS.ByteString
hexdec "" = return BS.empty
hexdec (hi:lo:rest) = BS.cons hexPair <$> hexdec rest
  where
    hexPair :: Word8
    hexPair = fromIntegral (16 * digitToInt (toUpper hi) + digitToInt (toUpper lo))
hexdec _ = fail "hexdec called with odd-length input"

blobWithSep :: Stream s m Char => Parser s m String -> Parser s m String
blobWithSep chunk = concat <$> sepEndBy1 chunk (many whiteSpace)

base64 :: Stream s m Char => ParsecT s u m BS.ByteString
base64 = b64dec =<< base64s

base64WithSep :: Stream s m Char => Parser s m BS.ByteString
base64WithSep = b64dec =<< blobWithSep base64s

base32Hex :: Stream s m Char => ParsecT s u m BS.ByteString
base32Hex = b32dec =<< many1 base32HexC

hex :: Stream s m Char => ParsecT s u m BS.ByteString
hex = hexdec =<< hexs

hexWithSep :: Stream s m Char => Parser s m BS.ByteString
hexWithSep = hexdec =<< blobWithSep hexs
