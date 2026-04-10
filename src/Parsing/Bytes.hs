{-# LANGUAGE FlexibleContexts #-}
module Parsing.Bytes
  ( base64
  , base64WithSep
  , hex
  , hexWithSep) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Base64 as B64
import Data.Char (digitToInt, isHexDigit, toUpper)
import Text.Parsec
import Text.Parsec.Char as Char

import Parsing.State

base64c :: Stream s m Char => ParsecT s u m Char
base64c = Char.oneOf $! ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+', '/', '=']

base64s :: Stream s m Char => ParsecT s u m String
base64s = many1 base64c

hexs :: Stream s m Char => ParsecT s u m String
hexs = many1 (satisfy isHexDigit)

b64dec :: String -> ParsecT s u m BS.ByteString
b64dec = either fail return . B64.decode . C.pack

hexdec :: String -> ParsecT s u m BS.ByteString
hexdec = return . BS.pack . go
  where
    go [] = []
    go (hi:lo:rest) = hexPair hi lo : go rest
    go _ = error "hexdec called with odd-length input"

    hexPair hi lo = fromIntegral (16 * digitToInt (toUpper hi) + digitToInt (toUpper lo))

blobWithSep :: Stream s m Char => Parser s m String -> Parser s m String
blobWithSep chunk = concat <$> sepEndBy1 chunk (many whiteSpace)

base64 :: Stream s m Char => ParsecT s u m BS.ByteString
base64 = b64dec =<< base64s

base64WithSep :: Stream s m Char => Parser s m BS.ByteString
base64WithSep = b64dec =<< blobWithSep base64s

hex :: Stream s m Char => ParsecT s u m BS.ByteString
hex = hexdec =<< hexs

hexWithSep :: Stream s m Char => Parser s m BS.ByteString
hexWithSep = hexdec =<< blobWithSep hexs
