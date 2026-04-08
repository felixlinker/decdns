{-# LANGUAGE FlexibleContexts #-}
module Parsing.Tokens
  ( hspace
  , dot
  , colon
  , int
  , category
  , base64
  , base64WithSeparators
  , hex
  , hexWithSeparators ) where

import Data.Char (digitToInt, isHexDigit, toUpper)
import Data.Functor (($>))
import Text.Parsec
import Text.Parsec.Char as Char

import DNS.Record
import Text.Read (readMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Base64 as B64

hspace :: Stream s m Char => ParsecT s u m Char
hspace = char ' ' <|> tab

dot :: Stream s m Char => ParsecT s u m Char
dot = Char.char '.'

colon :: Stream s m Char => ParsecT s u m Char
colon = Char.char ':'

int :: Stream s m Char => ParsecT s u m Int
int = do
  ttls <- many1 digit
  maybe (fail "cannot parse int") return (readMaybe ttls)

category :: Stream s m Char => ParsecT s u m Cat
category = string "IN" $> IN

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

blobWithSeparators :: Stream s m Char => ParsecT s u m String -> ParsecT s u m sep -> (String -> ParsecT s u m a) -> ParsecT s u m a
blobWithSeparators chunk separator decode = decode . concat =<< sepEndBy1 chunk separator

base64 :: Stream s m Char => ParsecT s u m BS.ByteString
base64 = b64dec =<< base64s

base64WithSeparators :: Stream s m Char => ParsecT s u m sep -> ParsecT s u m BS.ByteString
base64WithSeparators separator = blobWithSeparators base64s separator b64dec

hex :: Stream s m Char => ParsecT s u m BS.ByteString
hex = hexdec =<< hexs

hexWithSeparators :: Stream s m Char => ParsecT s u m sep -> ParsecT s u m BS.ByteString
hexWithSeparators separator = blobWithSeparators hexs separator hexdec
