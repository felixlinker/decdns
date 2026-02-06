module DNS.RecordType
  ( Type(..) ) where

data Type = CNAME | DNAME | NS | A | AAAA | TXT | MX | SOA
  deriving (Eq, Ord, Show)
