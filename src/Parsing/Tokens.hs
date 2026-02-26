{-# LANGUAGE FlexibleContexts #-}
module Parsing.Tokens
  ( hspace
  , dot
  , colon
  , int
  , category ) where

import Data.Functor (($>))
import Text.Parsec
import Text.Parsec.Char as Char

import DNS.Record
import Text.Read (readMaybe)

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
