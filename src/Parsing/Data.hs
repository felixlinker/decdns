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
  , string "DNSKEY" $> DNSKEY
  , string "RRSIG" $> RRSIG
  , string "NSEC" $> NSEC
  , string "DS" $> DS ]

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
    RRSIG -> return rrsig
    NSEC -> return nsec
    DS -> return ds

cname :: Stream s m Char => TypeDataParser s m
cname = RCNAME <$> name

dname :: Stream s m Char => TypeDataParser s m
dname = RDNAME <$> name

ns :: Stream s m Char => TypeDataParser s m
ns = RNS <$> name

a :: Stream s m Char => TypeDataParser s m
a = RA <$> manyTill anyChar endOfEntry

aaaa :: Stream s m Char => TypeDataParser s m
aaaa = RAAAA <$> manyTill anyChar endOfEntry

txt :: Stream s m Char => TypeDataParser s m
txt = RTXT <$> manyTill anyChar endOfEntry

mx :: Stream s m Char => TypeDataParser s m
mx = RMX <$> int <*> (sep >> name)

soa :: Stream s m Char => TypeDataParser s m
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
  RSOA mname rnam serial refresh retry expire <$> int

dnskey :: Stream s m Char => TypeDataParser s m
dnskey = do
  -- Can only have three values: 0, 256, and 257. SEP implies zone key flag.
  flags <- int
  sep
  _ <- char '3'
  sep
  alg <- int  -- TODO: support memnonics
  RDNSKEY (flags >= 256) (flags == 257) alg <$> manyTill anyChar sep

rrsig :: Stream s m Char => TypeDataParser s m
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
  RRRSIG typ alg lbls orgTTL expire inception tag signer <$> manyTill anyChar space

nsec :: Stream s m Char => TypeDataParser s m
nsec = do
  next <- name
  RNSEC next <$> manyTill (sep >> recordType) endOfEntry

ds :: Stream s m Char => TypeDataParser s m
ds = do
  tag <- int
  sep
  alg <- int
  sep
  typ <- int
  sep
  RDS tag alg typ <$> manyTill anyChar endOfEntry
