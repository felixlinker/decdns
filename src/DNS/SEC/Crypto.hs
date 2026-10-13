module DNS.SEC.Crypto
  ( AlgorithmType(..)
  , algorithmToInt
  , algorithmFromInt
  , algorithmFromMnemonic
  , ecdsaSig
  , ecdsaPk
  , rsaPk
  , DigestType(..)
  , digestToInt
  , digestFromInt
  , NSEC3DigestType(..)
  , nsec3DigestToInt
  , nsec3DigestFromInt
  , NSEC3Flags(..)
  , nsec3FlagsToInt
  , nsec3FlagsFromInt
  ) where

import Control.Monad (guard)
import Crypto.PubKey.ECC.Prim (isPointValid)
import qualified Crypto.PubKey.RSA.Types as RSA
import Data.Bits ((.|.))
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
algorithmFromMnemonic "DSA_NSEC3_SHA1" = Just DSA_NSEC3_SHA1
algorithmFromMnemonic "RSASHA1_NSEC3_SHA1" = Just RSASHA1_NSEC3_SHA1
algorithmFromMnemonic "RSASHA256" = Just RSASHA256
algorithmFromMnemonic "RSASHA512" = Just RSASHA512
algorithmFromMnemonic "ECC_GOST" = Just ECC_GOST
algorithmFromMnemonic "ECDSAP256SHA256" = Just ECDSAP256SHA256
algorithmFromMnemonic "ECDSAP384SHA384" = Just ECDSAP384SHA384
algorithmFromMnemonic "ED25519" = Just ED25519
algorithmFromMnemonic "ED448" = Just ED448
algorithmFromMnemonic "SM2SM3" = Just SM2SM3
algorithmFromMnemonic "ECC_GOST12" = Just ECC_GOST12
algorithmFromMnemonic "INDIRECT" = Just INDIRECT
algorithmFromMnemonic "PRIVATEDNS" = Just PRIVATEDNS
algorithmFromMnemonic "PRIVATEOID" = Just PRIVATEOID
algorithmFromMnemonic _ = Nothing

-- RFC 6605, Section 4: fixed-width r || s and x || y encodings.
-- Keep the original helpers as P-256 specializations.
ecdsaSig :: AlgorithmType -> BS.ByteString -> Maybe ECDSA.Signature
ecdsaSig alg sig = do
  (size, _) <- ecdsaParameters alg
  guard (BS.length sig == 2 * size)
  return $ uncurry ECDSA.Signature $ bimap os2ip os2ip $ BS.splitAt size sig

ecdsaPk :: AlgorithmType -> BS.ByteString -> Maybe ECDSA.PublicKey
ecdsaPk alg key = do
  (size, curve) <- ecdsaParameters alg
  guard (BS.length key == 2 * size)
  let point = uncurry ECCTypes.Point $ bimap os2ip os2ip $ BS.splitAt size key
  guard (isPointValid curve point)
  return $ ECDSA.PublicKey curve point

ecdsaParameters :: AlgorithmType -> Maybe (Int, ECCTypes.Curve)
ecdsaParameters ECDSAP256SHA256 = Just (32, ECCTypes.getCurveByName ECCTypes.SEC_p256r1)
ecdsaParameters ECDSAP384SHA384 = Just (48, ECCTypes.getCurveByName ECCTypes.SEC_p384r1)
ecdsaParameters _ = Nothing

-- RFC 3110, Section 2: exponent length (one or three octets), exponent,
-- and modulus. All RSA DNSSEC algorithms share this encoding.
rsaPk :: BS.ByteString -> Maybe RSA.PublicKey
rsaPk key = do
  (first, rest) <- BS.uncons key
  (size, body) <- if first == 0
    then do
      guard (BS.length rest >= 2)
      let size = fromIntegral $ os2ip $ BS.take 2 rest
      guard (size > 255)
      return (size, BS.drop 2 rest)
    else return (fromIntegral first, rest)
  guard (BS.length body > size)
  let (exponentBytes, modulus) = BS.splitAt size body
      e = os2ip exponentBytes
      n = os2ip modulus
  guard (BS.head exponentBytes /= 0 && BS.head modulus /= 0)
  guard (e > 1 && odd e && n > e && odd n)
  return $ RSA.PublicKey (BS.length modulus) n e

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

data NSEC3DigestType = NSEC3_SHA1
  deriving (Eq, Ord)

nsec3DigestToInt :: NSEC3DigestType -> Int
nsec3DigestToInt NSEC3_SHA1 = 1

instance Show NSEC3DigestType where
  show = show . nsec3DigestToInt

nsec3DigestFromInt :: Int -> Maybe NSEC3DigestType
nsec3DigestFromInt 1 = Just NSEC3_SHA1
nsec3DigestFromInt _ = Nothing

data NSEC3Flags = OPT_OUT
  deriving (Eq, Ord)

nsec3FlagsToInt :: [NSEC3Flags] -> Int
nsec3FlagsToInt = foldr ((.|.) . nsec3FlagToInt) 0

nsec3FlagToInt :: NSEC3Flags -> Int
nsec3FlagToInt OPT_OUT = 1

nsec3FlagsFromInt :: Int -> [NSEC3Flags]
nsec3FlagsFromInt 1 = [OPT_OUT]
nsec3FlagsFromInt _ = []
