module DNS.SEC.Crypto
  ( AlgorithmType(..)
  , algorithmToInt
  , algorithmFromInt
  , algorithmFromMnemonic
  , ecdsaSig
  , ecdsaPk
  , DigestType(..)
  , digestToInt
  , digestFromInt
  ) where

import qualified Data.ByteString as BS
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Crypto.PubKey.ECC.Types as ECCTypes
import Data.Bifunctor (Bifunctor(bimap))
import Crypto.Number.Serialize (os2ip)

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

algorithmToInt :: AlgorithmType -> Int
algorithmToInt RSAMD5 = 1
algorithmToInt DH = 2
algorithmToInt DSA = 3
algorithmToInt RSASHA1 = 5
algorithmToInt DSA_NSEC3_SHA1 = 6
algorithmToInt RSASHA1_NSEC3_SHA1 = 7
algorithmToInt RSASHA256 = 8
algorithmToInt RSASHA512 = 10
algorithmToInt ECC_GOST = 12
algorithmToInt ECDSAP256SHA256 = 13
algorithmToInt ECDSAP384SHA384 = 14
algorithmToInt ED25519 = 15
algorithmToInt ED448 = 16
algorithmToInt SM2SM3 = 17
algorithmToInt ECC_GOST12 = 23
algorithmToInt INDIRECT = 252
algorithmToInt PRIVATEDNS = 253
algorithmToInt PRIVATEOID = 254

instance Show AlgorithmType where
  show = show . algorithmToInt

algorithmFromInt :: Int -> Maybe AlgorithmType
algorithmFromInt 1 = Just RSAMD5
algorithmFromInt 2 = Just DH
algorithmFromInt 3 = Just DSA
algorithmFromInt 5 = Just RSASHA1
algorithmFromInt 6 = Just DSA_NSEC3_SHA1
algorithmFromInt 7 = Just RSASHA1_NSEC3_SHA1
algorithmFromInt 8 = Just RSASHA256
algorithmFromInt 10 = Just RSASHA512
algorithmFromInt 12 = Just ECC_GOST
algorithmFromInt 13 = Just ECDSAP256SHA256
algorithmFromInt 14 = Just ECDSAP384SHA384
algorithmFromInt 15 = Just ED25519
algorithmFromInt 16 = Just ED448
algorithmFromInt 17 = Just SM2SM3
algorithmFromInt 23 = Just ECC_GOST12
algorithmFromInt 252 = Just INDIRECT
algorithmFromInt 253 = Just PRIVATEDNS
algorithmFromInt 254 = Just PRIVATEOID
algorithmFromInt _ = Nothing

algorithmFromMnemonic :: String -> Maybe AlgorithmType
algorithmFromMnemonic "RSAMD5" = Just RSAMD5
algorithmFromMnemonic "DH" = Just DH
algorithmFromMnemonic "DSA" = Just DSA
algorithmFromMnemonic "RSASHA1" = Just RSASHA1
algorithmFromMnemonic "INDIRECT" = Just INDIRECT
algorithmFromMnemonic "PRIVATEDNS" = Just PRIVATEDNS
algorithmFromMnemonic "PRIVATEOID" = Just PRIVATEOID
algorithmFromMnemonic _ = Nothing

-- https://datatracker.ietf.org/doc/html/rfc6605#section-4
ecdsaSig :: BS.ByteString -> Maybe ECDSA.Signature
ecdsaSig sig
  | BS.length sig /= 64 = Nothing
  | otherwise = Just $ uncurry ECDSA.Signature $ bimap os2ip os2ip $ BS.splitAt 32 sig

p256 :: ECCTypes.Curve
p256 = ECCTypes.getCurveByName ECCTypes.SEC_p256r1

-- https://datatracker.ietf.org/doc/html/rfc6605#section-4
ecdsaPk :: BS.ByteString -> Maybe ECDSA.PublicKey
ecdsaPk key
  | BS.length key /= 64 = Nothing
  | otherwise = Just $ ECDSA.PublicKey p256 $ uncurry ECCTypes.Point $ bimap os2ip os2ip $ BS.splitAt 32 key

data DigestType = NULL | SHA1 | SHA256 | GOST_R_31_11_94 | SHA384
  deriving (Eq, Ord)

digestToInt :: DigestType -> Int
digestToInt NULL = 0
digestToInt SHA1 = 1
digestToInt SHA256 = 2
digestToInt GOST_R_31_11_94 = 3
digestToInt SHA384 = 4

instance Show DigestType where
  show = show . digestToInt

digestFromInt :: Int -> Maybe DigestType
digestFromInt 0 = Just NULL
digestFromInt 1 = Just SHA1
digestFromInt 2 = Just SHA256
digestFromInt 3 = Just GOST_R_31_11_94
digestFromInt 4 = Just SHA384
digestFromInt _ = Nothing
