module DNS.RecordType
  ( Type(..)
  , allTypes ) where

data Type = CNAME | DNAME | NS | A | AAAA | TXT | MX | SOA | DNSKEY | RRSIG | NSEC | DS
  deriving (Eq, Ord, Show)

allTypes :: [Type]
allTypes = [ CNAME, DNAME, NS, A, AAAA, TXT, MX, SOA, DNSKEY, RRSIG, NSEC, DS ]
