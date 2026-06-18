{-# LANGUAGE FlexibleInstances #-}
module DNS.Record
  ( Cat(..)
  , DNSKeyFlag(..)
  , Type(..)
  , showData
  , toType
  , RData(..)
  , Record
  , record
  , NamedRecord
  , StandardRecord
  , canonicalRRForm
  , dnskeyKeyTag ) where

import qualified Data.ByteString as BS
import Data.IP (IPv4, IPv6)

import DNS.Name
import qualified DNS.RecordType as T
import DNS.SEC.Crypto
import DNS.SEC.Keys
import Util.Bytes
import Data.Word (Word16)

data Type =
    CNAME (Name String)
  | DNAME (Name String)
  | NS (Name String)
  | A IPv4
  | AAAA IPv6
  | TXT String
  | MX { _preference :: Int
        , _exchange :: Name String }
  | SOA { _mname :: Name String
        , _rname :: Name String
        , _serial :: Int
        , _refresh :: Int
        , _retry :: Int
        , _expire :: Int
        , _minimum :: Int }
  | DNSKEY  { _flags :: [DNSKeyFlag]
            , _algorithm :: AlgorithmType
            , _publicKey :: BS.ByteString }
  | RRSIG { _typeCovered :: T.Type
          , _algorithm :: AlgorithmType
          , _labels :: Int
          , _originalTTL :: Int
          , _signatureExpiration :: Int
          , _signatureInception :: Int
          , _keyTag :: Int
          , _signersName :: Name String
          , _signature :: BS.ByteString }
    | NSEC  { _nextDomainName :: Name String
            , _typeBitMaps :: [T.Type] }
    | DS  { _keyTag :: Int
          , _algorithm :: AlgorithmType
          , _digestType :: DigestType
          , _digest :: BS.ByteString }
  deriving (Eq, Ord)

showData :: Type -> String
showData (CNAME n) = show n
showData (DNAME n) = show n
showData (NS n) = show n
showData (A addr) = show addr
showData (AAAA addr) = show addr
showData (TXT addr) = addr
showData (MX p n) = unwords [show p, show n]
showData (SOA a b c d e f g) = unwords [show a, show b, show c, show d, show e, show f, show g]
showData (DNSKEY flags algorithm publicKey) =
  unwords [show (dnsKeyFlagsToInt flags), "3", show algorithm, toB64 publicKey]
showData (RRSIG typeCovered algorithm lc originalTTL signatureExpiration signatureInception tag signersName signature) =
  unwords
    [ show typeCovered
    , show algorithm
    , show lc
    , show originalTTL
    , show signatureExpiration
    , show signatureInception
    , show tag
    , show signersName
    , toB64 signature
    ]
showData (NSEC nextDomainName typeBitMaps) =
  unwords (show nextDomainName : map show typeBitMaps)
showData (DS tag algorithm digestType digest) =
  unwords [show tag, show algorithm, show digestType, toHex digest]

canonicalType :: Type -> Type
canonicalType (NS n) = NS $ canonicalName n
canonicalType (CNAME n) = CNAME $ canonicalName n
canonicalType r@(SOA { _mname = mn, _rname = rn }) = r { _mname = canonicalName mn, _rname = canonicalName rn }
canonicalType r@(MX _ n) = r { _exchange = canonicalName n }
canonicalType (DNAME n) = DNAME $ canonicalName n
canonicalType r@(RRSIG { _signersName = sn }) = r { _signersName = canonicalName sn }
canonicalType r@(NSEC n _) = r { _nextDomainName = canonicalName n }
canonicalType r = r

instance Show Type where
  show t = show (toType t) ++ " " ++ showData t

toType :: Type -> T.Type
toType (DNAME _) = T.DNAME
toType (CNAME _) = T.CNAME
toType (NS _) = T.NS
toType (A _) = T.A
toType (AAAA _) = T.AAAA
toType (TXT _) = T.TXT
toType (MX {}) = T.MX
toType (SOA {}) = T.SOA
toType (DNSKEY {}) = T.DNSKEY
toType (RRSIG {}) = T.RRSIG
toType (NSEC {}) = T.NSEC
toType (DS {}) = T.DS

data Cat = IN
  deriving (Eq, Ord, Show)

data RData = RData
  { rcat  :: Cat
  , rttl  :: Int
  , rtype :: Type }
  deriving (Eq, Ord)

type Record a = Named a RData
type NamedRecord a = Record a
type StandardRecord = NamedRecord String

record :: Name a -> RData -> Record a
record = Named

instance Show RData where
  show (RData c ttl t) = show ttl ++ " " ++ show c ++ " " ++ show t

canonicalRData :: RData -> RData
canonicalRData d@(RData _ _ ty) = d { rtype = canonicalType ty }

canonicalRRForm :: StandardRecord -> StandardRecord
canonicalRRForm (Named n d) = Named (canonicalName n) (canonicalRData d)

dnskeyKeyTag :: Type -> Word16
dnskeyKeyTag (DNSKEY fs alg bs) = keyTag fs alg bs
dnskeyKeyTag _ = 0x00
