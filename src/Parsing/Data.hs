{-# LANGUAGE FlexibleContexts #-}
module Parsing.Data
  ( TypeParser
  , recordType
  , recordTypeData ) where

import Data.Functor (($>))
import Control.Applicative (asum)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C
import Data.Char (digitToInt, isHexDigit, isPrint, toUpper)
import Text.Parsec

import Parsing.State
import DNS.Record
import qualified DNS.RecordType as T
import DNS.SEC.Crypto
import Parsing.Name
import Parsing.Tokens
import Control.Monad (when)

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
cname = CNAME <$> name

dname :: Stream s m Char => TypeParser s m
dname = DNAME <$> name

ns :: Stream s m Char => TypeParser s m
ns = NS <$> name

a :: Stream s m Char => TypeParser s m
a = A <$> many1 (digit <|> dot)

aaaa :: Stream s m Char => TypeParser s m
aaaa = AAAA <$> many1 (hexDigit <|> colon)

txt :: Stream s m Char => TypeParser s m
txt = TXT <$> many1 (satisfy isPrint)

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
  flagValue <- int
  fieldSep
  protocol <- int
  when (protocol /= 3) (fail "DNSKEY protocol must be 3")
  fieldSep
  algorithm <- algorithmType
  fieldSep
  DNSKEY (dnsKeyFlagsFromInt flagValue) algorithm <$> base64Blob

rrsig :: Stream s m Char => TypeParser s m
rrsig = do
  typeCovered <- recordType
  fieldSep
  algorithm <- algorithmType
  fieldSep
  labelCount <- int
  fieldSep
  originalTTL <- int
  fieldSep
  signatureExpiration <- int
  fieldSep
  signatureInception <- int
  fieldSep
  keyTag <- int
  fieldSep
  signersName <- name
  fieldSep
  RRSIG typeCovered algorithm labelCount originalTTL signatureExpiration signatureInception keyTag signersName <$> base64Blob

nsec :: Stream s m Char => TypeParser s m
nsec = do
  nextDomainName <- name
  fieldSep
  firstType <- recordType
  restTypes <- many (try (fieldSep >> recordType))
  return $ NSEC nextDomainName (firstType : restTypes)

ds :: Stream s m Char => TypeParser s m
ds = do
  keyTag <- int
  fieldSep
  algorithm <- algorithmType
  fieldSep
  digestType <- digestTypeCode
  fieldSep
  DS keyTag algorithm digestType <$> hexBlob

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

-- TODO: Remove code redundancies to Parsing.Tokens
base64Blob :: Stream s m Char => Parser s m BS.ByteString
base64Blob = blobWithSpace base64Chunk decodeBase64

hexBlob :: Stream s m Char => Parser s m BS.ByteString
hexBlob = blobWithSpace hexChunk decodeHex

blobWithSpace :: Stream s m Char => Parser s m String -> (String -> Parser s m a) -> Parser s m a
blobWithSpace chunk decode = decode . concat =<< sepBy1 chunk (many1 dataSpace)

base64Chunk :: Stream s m Char => Parser s m String
base64Chunk = many1 (oneOf (['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "+/="))

hexChunk :: Stream s m Char => Parser s m String
hexChunk = many1 (satisfy isHexDigit)

decodeBase64 :: String -> Parser s m BS.ByteString
decodeBase64 = either fail return . B64.decode . C.pack

-- TODO: Move into Parsing.Tokens
decodeHex :: String -> Parser s m BS.ByteString
decodeHex xs
  | odd (length xs) = fail "hex text must contain an even number of digits"
  | otherwise = return (BS.pack (go xs))
  where
    go [] = []
    go (hi:lo:rest) = hexPair hi lo : go rest
    go _ = error "decodeHex called with odd-length input"

    hexPair hi lo = fromIntegral (16 * digitToInt (toUpper hi) + digitToInt (toUpper lo))
