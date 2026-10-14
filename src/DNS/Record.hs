{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
module DNS.Record
  ( Cat(..)
  , DNSKeyFlag(..)
  , DNSKEYData(..)
  , DSData(..)
  , RRSIGData(..)
  , NSECData(..)
  , NSEC3Data(..)
  , canonicalRRSIGData
  , Type(..)
  , canonicalType
  , showData
  , toType
  , RData(..)
  , Record
  , record
  , recordType
  , StandardRecord
  , canonicalRData
  , canonicalRecord
  , hasSignersName
  , coversSOA ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import Data.Bits (setBit, zeroBits)
import Data.List (groupBy, sort)
import Data.Word (Word8)
import Data.IP (IPv4, IPv6)

import DNS.Name
import qualified DNS.RecordType as T
import DNS.SEC.Crypto
import DNS.SEC.Keys
import Util.Bytes

data DNSKEYData = DNSKEYData
  { _flags :: [DNSKeyFlag]
  , _keyAlgorithm :: AlgorithmType
  , _publicKey :: BS.ByteString
  }
  deriving (Eq, Ord)

instance Show DNSKEYData where
  show (DNSKEYData flags algorithm publicKey) =
    unwords [show (dnsKeyFlagsToInt flags), "3", show algorithm, toB64 publicKey]

instance Serialize DNSKEYData where
  serialize (DNSKEYData flags algorithm publicKey) =
    word16 (dnsKeyFlagsToInt flags)
      <> word8 (3 :: Int)
      <> word8 (algorithmToInt algorithm)
      <> publicKey

data DSData = DSData
  { _keyTag :: Int
  , _algorithm :: AlgorithmType
  , _digestType :: DigestType
  , _digest :: BS.ByteString
  }
  deriving (Eq, Ord)

instance Show DSData where
  show (DSData tag algorithm digestType digest) =
    unwords [show tag, show algorithm, show digestType, toHex digest]

instance Serialize DSData where
  serialize (DSData tag algorithm digestType digest) =
    word16 tag
      <> word8 (algorithmToInt algorithm)
      <> word8 (digestToInt digestType)
      <> digest

data RRSIGData a = RRSIGData
  { _typeCovered :: T.Type
  , _signatureAlgorithm :: AlgorithmType
  , _labels :: Int
  , _originalTTL :: Int
  , _signatureExpiration :: Int
  , _signatureInception :: Int
  , _verificationKeyTag :: Int
  , _signersName :: Name String
  , _signature :: a
  }
  deriving (Eq, Ord)

instance Functor RRSIGData where
  fmap f sig = sig { _signature = f (_signature sig) }

instance Show (RRSIGData BS.ByteString) where
  show (RRSIGData typeCovered algorithm lc originalTTL signatureExpiration signatureInception tag signersName signature) = unwords
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

instance Serialize a => Serialize (RRSIGData a) where
  serialize (RRSIGData cov alg lc ttl sigExp sigNbf tag sigName sig) = serialize cov
    <> word8 (algorithmToInt alg)
    <> word8 lc
    <> word32 ttl
    <> word32 sigExp
    <> word32 sigNbf
    <> word16 tag
    <> serialize sigName
    <> serialize sig

canonicalRRSIGData :: RRSIGData a -> RRSIGData a
canonicalRRSIGData rrsig =
  rrsig { _signersName = canonicalName (_signersName rrsig) }

data NSECData = NSECData
  { _nextDomainName :: Name String
  , _nsecTypeBitMaps :: [T.Type] }
  deriving (Eq, Ord)

instance Show NSECData where
  show (NSECData nextDomainName typeBitMaps) =
    unwords (show nextDomainName : map show typeBitMaps)

instance Serialize NSECData where
  serialize (NSECData nextDomainName typeBitMaps) =
    serialize nextDomainName <> nsecTypeBitMaps typeBitMaps

canonicalNSECData :: NSECData -> NSECData
canonicalNSECData (NSECData next ts) = NSECData (canonicalName next) ts

data NSEC3Data = NSEC3Data
  { _hashAlgorithm :: NSEC3DigestType
  , _nsec3Flags :: [NSEC3Flags]
  , _iterations :: Int
  , _salt :: BS.ByteString
  , _nextHashedOwner :: BS.ByteString
  , _nsec3TypeBitMaps :: [T.Type]
  }
  deriving (Eq, Ord)

instance Show NSEC3Data where
  show (NSEC3Data hashAlgorithm optOut iterations salt nextHashedOwner typeBitMaps) = unwords $
    [ show hashAlgorithm
    , show (nsec3FlagsToInt optOut)
    , show iterations
    , if BS.null salt then "-" else toHex salt
    , toB32 nextHashedOwner
    ] ++ map show typeBitMaps

instance Serialize NSEC3Data where
  serialize (NSEC3Data hashAlgorithm optOut iterations salt nextHashedOwner typeBitMaps) =
    word8 (nsec3DigestToInt hashAlgorithm)
      <> word8 (nsec3FlagsToInt optOut)
      <> word16 iterations
      <> withLength salt
      <> withLength nextHashedOwner
      <> nsecTypeBitMaps typeBitMaps

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
  | DNSKEY DNSKEYData
  | RRSIG (RRSIGData BS.ByteString)
  | NSEC NSECData
  | NSEC3 NSEC3Data
  | NSEC3PARAM
      { _paramHashAlgorithm :: NSEC3DigestType
      , _paramFlags :: [NSEC3Flags]
      , _paramIterations :: Int
      , _paramSalt :: BS.ByteString
      }
  | DS DSData
  | ZONEMD Int Int Int BS.ByteString
  deriving (Eq, Ord)

showData :: Type -> String
showData (CNAME n) = show n
showData (DNAME n) = show n
showData (NS n) = show n
showData (A addr) = show addr
showData (AAAA addr) = show addr
showData (TXT addr) = show addr
showData (MX p n) = unwords [show p, show n]
showData (SOA a b c d e f g) = unwords [show a, show b, show c, show d, show e, show f, show g]
showData (DNSKEY keyData) = show keyData
showData (RRSIG sigDat) = show sigDat
showData (NSEC nsecDat) = show nsecDat
showData (NSEC3 nsec3Dat) = show nsec3Dat
showData (NSEC3PARAM hashAlgorithm optOut iterations salt) = unwords
  [ show hashAlgorithm
  , show (nsec3FlagsToInt optOut)
  , show iterations
  , if BS.null salt then "-" else toHex salt
  ]
showData (DS dsData) = show dsData
showData (ZONEMD serial scheme hashAlgorithm digest) = unwords
  [ show serial, show scheme, show hashAlgorithm, toHex digest ]

canonicalType :: Type -> Type
canonicalType (NS n) = NS $ canonicalName n
canonicalType (CNAME n) = CNAME $ canonicalName n
canonicalType r@(SOA { _mname = mn, _rname = rn }) = r { _mname = canonicalName mn, _rname = canonicalName rn }
canonicalType r@(MX _ n) = r { _exchange = canonicalName n }
canonicalType (DNAME n) = DNAME $ canonicalName n
canonicalType (RRSIG sig) = RRSIG $ canonicalRRSIGData sig
canonicalType (NSEC nsecDat) = NSEC $ canonicalNSECData nsecDat
canonicalType r@(NSEC3 _) = r
canonicalType r@(NSEC3PARAM {}) = r
canonicalType r@(A _) = r
canonicalType r@(AAAA _) = r
canonicalType r@(TXT _) = r
canonicalType r@(DNSKEY _) = r
canonicalType r@(DS _) = r
canonicalType r@(ZONEMD {}) = r

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
toType (RRSIG _) = T.RRSIG
toType (NSEC {}) = T.NSEC
toType (NSEC3 {}) = T.NSEC3
toType (NSEC3PARAM {}) = T.NSEC3PARAM
toType (DS {}) = T.DS
toType (ZONEMD {}) = T.ZONEMD

data Cat = IN
  deriving (Eq, Ord, Show)

instance Serialize Cat where
  serialize IN = word16 (1 :: Int)

data RData = RData
  { rcat  :: Cat
  , rttl  :: Int
  , rtype :: Type }

-- For equality and ordering, we ignore category  because it is constant, and
-- TTL because we are not concerned with TTL in this implementation.
instance Eq RData where
  (RData _ _ t) == (RData _ _ t') = t == t'

instance Ord RData where
  compare (RData _ _ t) (RData _ _ t') = compare t t'

type Record a = Named a RData
type StandardRecord = Record String

record :: Name a -> RData -> Record a
record = Named

recordType :: Record a -> T.Type
recordType = toType . rtype . value

instance Show RData where
  show (RData c ttl t) = show ttl ++ " " ++ show c ++ " " ++ show t

instance Serialize RData where
  serialize (RData c ttl t) =
    serialize (toType t)
      <> serialize c
      <> word32 ttl
      <> word16 (BS.length dat)
      <> dat
    where
      dat = serialize t

characterStrings :: String -> BS.ByteString
characterStrings (CBS.pack -> txt) = foldMap withLength chunks
  where
    chunks :: [BS.ByteString]
    chunks
      | BS.null txt = [BS.empty]
      | otherwise = chunk 255 txt

nsecTypeBitMaps :: [T.Type] -> BS.ByteString
nsecTypeBitMaps = foldMap encodeWindow
    . groupBy sameWindow
    . sort
    . map T.typeToInt
  where
    sameWindow a b = a `div` 256 == b `div` 256

encodeWindow :: [Int] -> BS.ByteString
encodeWindow [] = BS.empty
encodeWindow codes@(firstCode:_) =
  BS.pack [fromIntegral window, fromIntegral (length bitmap)] <> BS.pack bitmap
  where
    window :: Int
    window = firstCode `div` 256

    bitmap :: [Word8]
    bitmap = foldr (setTypeBit . (\c -> c - (window * 256))) [] codes

    setTypeBit :: Int -> [Word8] -> [Word8]
    setTypeBit position []
      | position < 8 = [setBit zeroBits (7 - position)]
      | otherwise = zeroBits : setTypeBit (position - 8) []
    setTypeBit position (w:ws)
      | position < 8 = setBit w (7 - position):ws
      | otherwise = w : setTypeBit (position - 8) ws

instance Serialize Type where
  serialize (CNAME n) = serialize n
  serialize (DNAME n) = serialize n
  serialize (NS n) = serialize n
  serialize (A addr) = serialize addr
  serialize (AAAA addr) = serialize addr
  serialize (TXT txt) = characterStrings txt
  serialize (MX preference exchange) = word16 preference <> serialize exchange
  serialize (SOA mname rname serial refresh retry expire minimumTtl) =
    serialize mname
      <> serialize rname
      <> foldMap word32 [serial, refresh, retry, expire, minimumTtl]
  serialize (DNSKEY keyData) = serialize keyData
  serialize (RRSIG sig) = serialize sig
  serialize (NSEC nsecData) = serialize nsecData
  serialize (NSEC3 nsec3Data) = serialize nsec3Data
  serialize (NSEC3PARAM hashAlgorithm optOut iterations salt) =
    word8 (nsec3DigestToInt hashAlgorithm)
      <> word8 (nsec3FlagsToInt optOut)
      <> word16 iterations
      <> withLength salt
  serialize (DS dsData) = serialize dsData
  serialize (ZONEMD serial scheme hashAlgorithm digest) =
    word32 serial <> word8 scheme <> word8 hashAlgorithm <> digest

canonicalRData :: RData -> RData
canonicalRData d@(RData _ _ ty) = d { rtype = canonicalType ty }

canonicalRecord :: StandardRecord -> StandardRecord
canonicalRecord (Named n d) = Named (canonicalName n) (canonicalRData d)

hasSignersName :: RData -> Name String -> Bool
hasSignersName (rtype -> RRSIG sig) n = _signersName sig == n
hasSignersName _ _ = False

coversSOA :: RData -> Bool
coversSOA (rtype -> NSEC nsec) = T.SOA `elem` _nsecTypeBitMaps nsec
coversSOA _ = False
