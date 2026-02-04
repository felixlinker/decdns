{-# LANGUAGE FlexibleContexts #-}
module Parsing.Data
  ( TypeParser
  , recordType
  , recordTypeData ) where

import Data.Functor (($>))
import Control.Applicative (asum)
import Text.Parsec

import Parsing.State
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
  , string "A" $> T.A
  , string "AAAA" $> T.AAAA
  , string "TXT" $> T.TXT
  , string "MX" $> T.MX
  , string "SOA" $> T.SOA
  , string "DNSKEY" $> T.DNSKEY
  , string "RRSIG" $> T.RRSIG
  , string "NSEC" $> T.NSEC
  , string "DS" $> T.DS ]

recordTypeData :: Stream s m Char => ParsecT s u m (TypeParser s m)
recordTypeData = do
  typ <- recordType
  case typ of
    T.CNAME -> return cname
    T.DNAME -> return dname
    T.NS -> return ns
    T.A -> return a
    T.AAAA -> return aaaa
    T.TXT -> return txt
    T.MX -> return mx
    T.SOA -> return soa
    T.DNSKEY -> return dnskey
    T.RRSIG -> return rrsig
    T.NSEC -> return nsec
    T.DS -> return ds

cname :: Stream s m Char => TypeParser s m
cname = CNAME <$> name

dname :: Stream s m Char => TypeParser s m
dname = DNAME <$> name

ns :: Stream s m Char => TypeParser s m
ns = NS <$> name

a :: Stream s m Char => TypeParser s m
a = A <$> manyTill anyChar endOfEntry

aaaa :: Stream s m Char => TypeParser s m
aaaa = AAAA <$> manyTill anyChar endOfEntry

txt :: Stream s m Char => TypeParser s m
txt = TXT <$> manyTill anyChar endOfEntry

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

dnskey :: Stream s m Char => TypeParser s m
dnskey = do
  -- Can only have three values: 0, 256, and 257. SEP implies zone key flag.
  flags <- int
  sep
  _ <- char '3'
  sep
  alg <- int  -- TODO: support memnonics
  DNSKEY (flags >= 256) (flags == 257) alg <$> manyTill anyChar sep

rrsig :: Stream s m Char => TypeParser s m
rrsig = do
  typ <- recordType
  sep
  alg <- int
  sep
  lbls <- int
  sep
  orgTTL <- int
  sep
  expire <- int
  sep
  inception <- int
  sep
  tag <- int
  sep
  -- TODO: I would accept @ as a name here but must not
  signer <- name
  sep
  RRSIG typ alg lbls orgTTL expire inception tag signer <$> manyTill anyChar space

nsec :: Stream s m Char => TypeParser s m
nsec = do
  next <- name
  NSEC next <$> manyTill (sep >> recordType) endOfEntry

ds :: Stream s m Char => TypeParser s m
ds = do
  tag <- int
  sep
  alg <- int
  sep
  typ <- int
  sep
  DS tag alg typ <$> manyTill anyChar endOfEntry
