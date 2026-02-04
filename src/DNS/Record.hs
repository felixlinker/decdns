module DNS.Record
  ( Name(..)
  , (<|)
  , Cat(..)
  , isSubDomain
  , canonicalCompare
  , dnameRewrite
  , Type(..)
  , showData
  , toType
  , RData(..)
  , Record(..) ) where

import DNS.Name
import qualified DNS.RecordType as T

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
  | DNSKEY  { _zoneKey :: Bool
            , _secureEntryPoint :: Bool
            , _algorithm :: Int
            , _publicKey :: String }  -- TODO: Make ByteString
  | RRSIG { _typeCovered :: T.Type
          , _algorithm :: Int
          , _labels :: Int
          , _originalTTL :: Int
          , _exp :: Int
          , _nbf :: Int
          , _keyTag :: Int
          , _signer :: Name String
          , _signature :: String   -- TODO: Make Bytestring
          }
  | NSEC  { _nextName :: Name String
          , _types :: [T.Type] }
  | DS  { _keyTag :: Int
        , _algorithm :: Int
        , _digestType :: Int
        , _digset :: String  -- TODO: Make ByteString
        }
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
showData (DNSKEY zk sep alg pk) = flags ++ " 3 " ++ show alg ++ " " ++ pk
  where
    flags :: String
    flags
      | sep = "257"
      | zk = "256"
      | otherwise = "0"
showData (RRSIG typ alg lbls orgTTL expire nbf tag signer sig) =
      show typ ++ " " ++ show alg ++ " " ++ show lbls ++ " " ++ show orgTTL ++ " "
  ++  show expire ++ " " ++ show nbf ++ " " ++ show tag ++ show signer ++ " " ++ show sig
showData (NSEC name types) = show name ++ " " ++ unwords (map show types)
showData (DS tag alg typ dig) = show tag ++ " " ++ show alg ++ " " ++ show typ ++ " " ++ dig

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

data Record a = Record
  { rname :: a
  , rrdata :: RData }
  deriving (Eq, Ord)

instance Show a => Show (Record a) where
  show (Record n (RData c ttl t)) = show n ++ " " ++ show ttl ++ " " ++ show c ++ " " ++ show t ++ " "
