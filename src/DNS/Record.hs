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
  , StandardRecord ) where

import qualified Data.ByteString as BS

import DNS.Name
import qualified DNS.RecordType as T
import DNS.SEC.Crypto

data DNSKeyFlag = ZoneKey | Other | SecureEntryPoint
  deriving (Eq, Ord)

data Type =
    CNAME (Name String)
  | DNAME (Name String)
  | NS (Name String)
  | A String
  | AAAA String
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
showData (A addr) = addr
showData (AAAA addr) = addr
showData (TXT addr) = addr
showData (MX p n) = unwords [show p, show n]
showData (SOA a b c d e f g) = unwords [show a, show b, show c, show d, show e, show f, show g]
showData (DNSKEY {}) = undefined
showData (RRSIG {}) = undefined
showData (NSEC {}) = undefined
showData (DS {}) = undefined

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
