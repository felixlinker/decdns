{-# LANGUAGE FlexibleContexts #-}
module Parsing.Data
  ( TypeParser
  , recordType
  , recordTypeData ) where

import Data.Char (isPrint)
import Data.Functor (($>))
import Control.Applicative (asum)
import Control.Monad (when)
import Text.Parsec

import Parsing.State
import Parsing.Bytes
import DNS.Record
import qualified DNS.RecordType as T
import DNS.SEC.Crypto
import DNS.SEC.Keys
import Parsing.Name
import Parsing.Tokens

type TypeParser s m = Parser s m Type

recordType :: Stream s m Char => ParsecT s u m T.Type
recordType = asum $ map try
  [ string "CNAME" $> T.CNAME
  , string "DNAME" $> T.DNAME
  , string "NSEC" $> T.NSEC
  , string "NS" $> T.NS
  , string "AAAA" $> T.AAAA
  , string "A" $> T.A
  , string "TXT" $> T.TXT
  , string "MX" $> T.MX
  , string "SOA" $> T.SOA
  , string "DNSKEY" $> T.DNSKEY
  , string "RRSIG" $> T.RRSIG
  , string "DS" $> T.DS ]

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
    T.DNSKEY -> dnskey
    T.RRSIG -> rrsig
    T.NSEC -> nsec
    T.DS -> ds

cname :: Stream s m Char => TypeParser s m
cname = CNAME <$> absName

dname :: Stream s m Char => TypeParser s m
dname = DNAME <$> absName

ns :: Stream s m Char => TypeParser s m
ns = NS <$> absName

a :: Stream s m Char => TypeParser s m
a = A <$> ipv4

aaaa :: Stream s m Char => TypeParser s m
aaaa = AAAA <$> ipv6

txt :: Stream s m Char => TypeParser s m
txt = TXT <$> many1 (satisfy isPrint)

mx :: Stream s m Char => TypeParser s m
mx = MX <$> int <*> (sep >> absName)

soa :: Stream s m Char => TypeParser s m
soa = do
  mname <- absName
  sep
  rnam <- absName
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
  flagValue <- int
  sep
  protocol <- int
  when (protocol /= 3) (fail "DNSKEY protocol must be 3")
  sep
  algorithm <- algorithmType
  sep
  DNSKEY (dnsKeyFlagsFromInt flagValue) algorithm <$> base64WithSep

rrsig :: Stream s m Char => TypeParser s m
rrsig = do
  typeCovered <- recordType
  sep
  algorithm <- algorithmType
  sep
  labelCount <- int
  sep
  originalTTL <- int
  sep
  signatureExpiration <- int
  sep
  signatureInception <- int
  sep
  tag <- int
  sep
  signersName <- absName
  sep
  RRSIG . RRSIGData typeCovered algorithm labelCount originalTTL signatureExpiration signatureInception tag signersName <$> base64WithSep

nsec :: Stream s m Char => TypeParser s m
nsec = do
  nextDomainName <- absName
  sep
  firstType <- recordType
  restTypes <- many (try (sep >> recordType))
  return $ NSEC nextDomainName (firstType : restTypes)

ds :: Stream s m Char => TypeParser s m
ds = do
  tag <- int
  sep
  algorithm <- algorithmType
  sep
  digestType <- digestTypeCode
  sep
  DS tag algorithm digestType <$> hexWithSep

algorithmType :: Stream s m Char => Parser s m AlgorithmType
algorithmType =
  asum (map try mnemonics) <|> fromNumber
  where
    mnemonics =
      [ string "RSAMD5" $> RSAMD5
      , string "DH" $> DH
      , string "DSA" $> DSA
      , string "RSASHA1" $> RSASHA1
      , string "INDIRECT" $> INDIRECT
      , string "PRIVATEDNS" $> PRIVATEDNS
      , string "PRIVATEOID" $> PRIVATEOID
      ]

    fromNumber = do
      n <- int
      maybe (fail "unknown DNSSEC algorithm") return (algorithmFromInt n)

digestTypeCode :: Stream s m Char => Parser s m DigestType
digestTypeCode = do
  n <- int
  maybe (fail "unknown DNSSEC digest type") return (digestFromInt n)
