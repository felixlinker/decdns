{-# LANGUAGE FlexibleContexts #-}
module Parsing.Tokens
  ( hspace
  , eol
  , dot
  , colon
  , int
  , category
  , ipv4
  , ipv6 ) where

import Data.Functor (($>), void)
import Data.IP (IPv4, IPv6)
import Text.Parsec
import Text.Parsec.Char as Char

import DNS.Record
import Text.Read (readMaybe)

hspace :: Stream s m Char => ParsecT s u m ()
hspace = void $ char ' ' <|> tab

eol :: Stream s m Char => ParsecT s u m ()
eol = void endOfLine

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

ipv4 :: Stream s m Char => ParsecT s u m IPv4
ipv4 = do
  addr <- many1 (digit <|> dot)
  maybe (fail "cannot parse IPv4") return (readMaybe addr)

ipv6 :: Stream s m Char => ParsecT s u m IPv6
ipv6 = do
  addr <- many1 (hexDigit <|> colon)
  maybe (fail "cannot parse IPv6") return (readMaybe addr)

