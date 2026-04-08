module DNS.SEC.Crypto
  ( AlgorithmType(..)
  , DigestType(..) ) where

data AlgorithmType
  = RSAMD5
  | DH
  | DSA
  | RSASHA1
  | DSA_NSEC3_SHA1
  | RSASHA1_NSEC3_SHA1
  | RSASHA256
  | RSASHA512
  | ECC_GOST
  | ECDSAP256SHA256
  | ECDSAP384SHA384
  | ED25519
  | ED448
  | SM2SM3
  | ECC_GOST12
  | INDIRECT
  | PRIVATEDNS
  | PRIVATEOID
  deriving (Eq, Ord)

instance Show AlgorithmType where
  show RSAMD5 = "1"
  show DH = "2"
  show DSA = "3"
  show RSASHA1 = "5"
  show DSA_NSEC3_SHA1 = "6"
  show RSASHA1_NSEC3_SHA1 = "7"
  show RSASHA256 = "8"
  show RSASHA512 = "10"
  show ECC_GOST = "12"
  show ECDSAP256SHA256 = "13"
  show ECDSAP384SHA384 = "14"
  show ED25519 = "15"
  show ED448 = "16"
  show SM2SM3 = "17"
  show ECC_GOST12 = "23"
  show INDIRECT = "252"
  show PRIVATEDNS = "253"
  show PRIVATEOID = "254"

data DigestType = NULL | SHA1 | SHA256 | GOST_R_31_11_94 | SHA384
  deriving (Eq, Ord)

instance Show DigestType where
  show NULL = "0"
  show SHA1 = "1"
  show SHA256 = "2"
  show GOST_R_31_11_94 = "3"
  show SHA384 = "4"
