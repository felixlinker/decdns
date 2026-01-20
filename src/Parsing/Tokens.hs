{-# LANGUAGE FlexibleContexts #-}
module Parsing.Tokens
  ( spaces1
  , dot
  , recordType
  , category ) where

import Control.Applicative (asum)
import Data.Functor (($>))
import Text.Parsec
import Text.Parsec.Char as Char

import ZoneFile

spaces1 :: Stream s m Char => ParsecT s u m ()
spaces1 = skipMany1 space

dot :: Stream s m Char => ParsecT s u m Char
dot = Char.char '.'

recordType :: Stream s m Char => ParsecT s u m Type
recordType = asum $ map try
  [ string "CNAME" $> CNAME
  , string "DNAME" $> DNAME
  , string "NS" $> NS
  , string "A" $> A
  , string "AAAA" $> AAAA
  , string "TXT" $> TXT
  , string "MX" $> MX
  , string "SOA" $> SOA ]

category :: Stream s m Char => ParsecT s u m Cat
category = string "IN" $> IN
