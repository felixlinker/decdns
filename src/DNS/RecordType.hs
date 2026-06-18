module DNS.RecordType
  ( Type(..)
  , allTypes
  , typeToInt ) where

import Util.Bytes (Serialize(..), word16)

data Type = CNAME | DNAME | NS | A | AAAA | TXT | MX | SOA | DNSKEY | RRSIG | NSEC | DS
  deriving (Eq, Ord, Show)

instance Serialize Type where
  serialize = word16 . typeToInt

allTypes :: [Type]
allTypes = [ CNAME, DNAME, NS, A, AAAA, TXT, MX, SOA, DNSKEY, RRSIG, NSEC, DS ]

typeToInt :: Type -> Int
typeToInt A = 1
typeToInt NS = 2
typeToInt CNAME = 5
typeToInt SOA = 6
typeToInt MX = 15
typeToInt TXT = 16
typeToInt AAAA = 28
typeToInt DNAME = 39
typeToInt DS = 43
typeToInt RRSIG = 46
typeToInt NSEC = 47
typeToInt DNSKEY = 48
