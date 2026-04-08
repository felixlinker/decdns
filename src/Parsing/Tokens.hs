{-# LANGUAGE FlexibleContexts #-}
module Parsing.Tokens
  ( hspace
  , dot
  , colon
  , int
  , category
  , base64
  , base64WithSpace ) where

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

-- TODO: Permit nesting in parantheses
base64sWithSpace :: Stream s m Char => ParsecT s u m String
base64sWithSpace = concat <$> sepBy1 base64s hspace

b64dec :: String -> ParsecT s u m BS.ByteString
b64dec = either fail return . B64.decode . C.pack

base64 :: Stream s m Char => ParsecT s u m BS.ByteString
base64 = b64dec =<< base64s

base64WithSpace :: Stream s m Char => ParsecT s u m BS.ByteString
base64WithSpace = b64dec =<< base64sWithSpace
