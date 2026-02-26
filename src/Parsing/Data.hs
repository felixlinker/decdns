{-# LANGUAGE FlexibleContexts #-}
module Parsing.Data
  ( TypeParser
  , recordType
  , recordTypeData ) where

import Data.Functor (($>))
import Control.Applicative (asum)
import Text.Parsec

import Parsing.State
import Parsing.Util
import DNS.Record
import qualified DNS.RecordType as T
import Parsing.Name
import Parsing.Tokens

type TypeParser s m = Parser s m Type

recordType :: Stream s m Char => ParsecT s u m T.Type
recordType = asum $ map try
  [ string "CNAME" $> T.CNAME
  , string "DNAME" $> T.DNAME
  , string "NS" $> T.NS
  , string "AAAA" $> T.AAAA
  , string "A" $> T.A
  , string "TXT" $> T.TXT
  , string "MX" $> T.MX
  , string "SOA" $> T.SOA ]

recordTypeData :: Stream s m Char => TypeParser s m
recordTypeData = do
  typ <- recordType
  sep
  case typ of
    T.CNAME -> cname
    T.DNAME -> dname
    T.NS -> ns
    T.A -> a
    T.AAAA -> aaaa
    T.TXT -> txt
    T.MX -> mx
    T.SOA -> soa

cname :: Stream s m Char => TypeParser s m
cname = CNAME <$> name

dname :: Stream s m Char => TypeParser s m
dname = DNAME <$> name

ns :: Stream s m Char => TypeParser s m
ns = NS <$> name

a :: Stream s m Char => TypeParser s m
a = A <$> manyTill1 anyChar endOfEntry

aaaa :: Stream s m Char => TypeParser s m
aaaa = AAAA <$> manyTill1 anyChar endOfEntry

txt :: Stream s m Char => TypeParser s m
txt = TXT <$> manyTill1 anyChar endOfEntry

mx :: Stream s m Char => TypeParser s m
mx = MX <$> int <*> (sep >> name)

soa :: Stream s m Char => TypeParser s m
soa = do
  mname <- name
  sep
  rnam <- name
  sep
  serial <- int
  sep
  refresh <- int
  sep
  retry <- int
  sep
  expire <- int
  sep
  SOA mname rnam serial refresh retry expire <$> int
