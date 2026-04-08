{-# LANGUAGE FlexibleInstances #-}
module DNS.Record
  ( Cat(..)
  , DNSKeyFlag(..)
  , dnsKeyFlagsFromInt
  , Type(..)
  , showData
  , toType
  , RData(..)
  , Record
  , record
  , NamedRecord
  , StandardRecord ) where

import qualified Data.ByteString as BS
import Data.Bits ((.|.), (.&.), complement)

import DNS.Name
import qualified DNS.RecordType as T
import DNS.SEC.Crypto
import Util.Bytes

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
showData (DNSKEY flags algorithm publicKey) =
  unwords [show (dnsKeyFlagsToInt flags), "3", show algorithm, toB64 publicKey]
showData (RRSIG typeCovered algorithm labelCount originalTTL signatureExpiration signatureInception keyTag signersName signature) =
  unwords
    [ show typeCovered
    , show algorithm
    , show labelCount
    , show originalTTL
    , show signatureExpiration
    , show signatureInception
    , show keyTag
    , show signersName
    , toB64 signature
    ]
showData (NSEC nextDomainName typeBitMaps) =
  unwords (show nextDomainName : map show typeBitMaps)
showData (DS keyTag algorithm digestType digest) =
  unwords [show keyTag, show algorithm, show digestType, toHex digest]

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

dnsKeyFlagsToInt :: [DNSKeyFlag] -> Int
dnsKeyFlagsToInt = foldr ((.|.) . dnsKeyFlagToInt) 0

dnsKeyFlagToInt :: DNSKeyFlag -> Int
dnsKeyFlagToInt ZoneKey = 256
dnsKeyFlagToInt Other = 128
dnsKeyFlagToInt SecureEntryPoint = 1

knownDNSKeyFlagsMask :: Int
knownDNSKeyFlagsMask = dnsKeyFlagsToInt [ZoneKey, Other, SecureEntryPoint]

dnsKeyFlagsFromInt :: Int -> [DNSKeyFlag]
dnsKeyFlagsFromInt flags =
  concat
    [ [ZoneKey | flags .&. dnsKeyFlagToInt ZoneKey /= 0]
    , [SecureEntryPoint | flags .&. dnsKeyFlagToInt SecureEntryPoint /= 0]
    , [Other | flags .&. otherDNSKeyFlagsMask /= 0]
    ]
  where
    otherDNSKeyFlagsMask = dnsKeyFlagToInt Other .|. complement knownDNSKeyFlagsMask
