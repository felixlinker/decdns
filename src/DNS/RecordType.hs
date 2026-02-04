module DNS.RecordType
  ( Type(..) ) where

data Type = CNAME | DNAME | NS | A | AAAA | TXT | MX | SOA | DNSKEY | RRSIG | NSEC | DS
  deriving (Eq, Ord, Show)
