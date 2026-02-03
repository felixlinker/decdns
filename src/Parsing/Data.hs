{-# LANGUAGE FlexibleContexts #-}
module Parsing.Data
  ( TypeDataParser
  , recordType
  , recordTypeData ) where

import Data.Functor (($>))
import Control.Applicative (asum)
import Text.Parsec

import Parsing.State
import ZoneFile
import Parsing.Name
import Parsing.Tokens

type TypeDataParser s m = Parser s m TypeData

recordType :: Stream s m Char => ParsecT s u m Type
recordType = asum $ map try
  [ string "CNAME" $> CNAME
  , string "DNAME" $> DNAME
  , string "NS" $> NS
  , string "A" $> A
  , string "AAAA" $> AAAA
  , string "TXT" $> TXT
  , string "MX" $> MX
  , string "SOA" $> SOA
  , string "DNSKEY" $> DNSKEY ]

recordTypeData :: Stream s m Char => ParsecT s u m (TypeDataParser s m)
recordTypeData = do
  typ <- recordType
  case typ of
    CNAME -> return cname
    DNAME -> return dname
    NS -> return ns
    A -> return a
    AAAA -> return aaaa
    TXT -> return txt
    MX -> return mx
    SOA -> return soa
    DNSKEY -> return dnskey

cname :: Stream s m Char => TypeDataParser s m
cname = RCNAME <$> name

dname :: Stream s m Char => TypeDataParser s m
dname = RDNAME <$> name

ns :: Stream s m Char => TypeDataParser s m
ns = RNS <$> name

a :: Stream s m Char => TypeDataParser s m
a = RA <$> untilEndOfLine

aaaa :: Stream s m Char => TypeDataParser s m
aaaa = RAAAA <$> untilEndOfLine

txt :: Stream s m Char => TypeDataParser s m
txt = RTXT <$> untilEndOfLine

mx :: Stream s m Char => TypeDataParser s m
mx = RMX <$> int <*> (spaces1 >> name)

soa :: Stream s m Char => TypeDataParser s m
soa = do
  mname <- name
  spaces1
  rnam <- name
  spaces1
  serial <- int
  spaces1
  refresh <- int
  spaces1
  retry <- int
  spaces1
  expire <- int
  spaces1
  RSOA mname rnam serial refresh retry expire <$> int

dnskey :: Stream s m Char => TypeDataParser s m
dnskey = do
  -- Can only have three values: 0, 256, and 257. SEP implies zone key flag.
  flags <- int
  spaces1
  _ <- char '3'
  spaces1
  alg <- int  -- TODO: support memnonics
  RDNSKEY (flags >= 256) (flags == 257) alg <$> untilEndOfLine
