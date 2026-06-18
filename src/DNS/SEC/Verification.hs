{-# LANGUAGE ViewPatterns #-}
module DNS.SEC.Verification
  ( verifySignature
  , keyTag
  , keyDigest
  , verifyDigest ) where

import Crypto.Hash (Digest, hash)
import qualified Crypto.Hash.Algorithms as Hash
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import Data.Bits (shiftR, shiftL)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.List (intersperse)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Data.Word (Word16, Word32)

import DNS.Name
import DNS.Record
import Util.Bytes
import DNS.SEC.Crypto
import Data.Maybe (fromMaybe)

verifySignature :: Name String -> [Type] -> RRSIGData BS.ByteString -> BS.ByteString -> Bool
verifySignature (canonicalName -> owner) (map canonicalType -> rrset) (canonicalRRSIGData -> sig) key = fromMaybe False $
  case _signatureAlgorithm sig of
    ECDSAP256SHA256 -> do
      cSig <- ecdsaSig (_signature sig)
      cPk <- ecdsaPk key
      return $ ECDSA.verify Hash.SHA256 cPk cSig (BS.concat (serialize sig : serializedRRSet))
    _ -> Nothing
  where
    lc :: Int
    lc = _labels sig

    originalOwner :: Name String
    originalOwner = case owner of
      n@(Name ls) ->
        let lc' = labelCount n
        in  if lc' > lc then Name ("*" NE.:| NE.drop (lc' - lc) ls) else n

    mkRecord :: Type -> StandardRecord
    mkRecord = Named originalOwner . RData IN (_originalTTL sig)

    -- Set construction for sorting and deduplication
    serializedRRSet :: [BS.ByteString]
    serializedRRSet = S.toAscList $ S.fromList
      $ map (serialize . mkRecord) rrset

keyTag :: DNSKEYData -> Word16
keyTag (serialize -> wireFormat) =
  let i32 = foldr ((+) . fromIntegral) (0 :: Word32) asDoubleOctets
      lower = fromIntegral i32 :: Word16
      upper = fromIntegral (i32 `shiftR` 16) :: Word16
  in  lower + upper
  where
    -- Alternatingly, shift Word8 left by 8 to make them the high 8 bits of a
    -- Word16
    asDoubleOctets :: [Word16]
    asDoubleOctets = zipWith (shiftL . fromIntegral) (BS.unpack wireFormat) (intersperse 0 (repeat 8))

keyDigest :: Name String -> DigestType -> DNSKEYData -> BS.ByteString
keyDigest (canonicalName -> ownerName) digType k = case digType of
  SHA1    -> BA.convert (digest :: Digest Hash.SHA1)
  SHA256  -> BA.convert (digest :: Digest Hash.SHA256)
  SHA384  -> BA.convert (digest :: Digest Hash.SHA384)
  _ -> undefined
  where
    digest :: Hash.HashAlgorithm a => Digest a
    digest = hash $ serialize ownerName <> serialize k

-- See: https://datatracker.ietf.org/doc/html/rfc4035#section-5.2
verifyDigest :: Name String -> DSData -> DNSKEYData -> Bool
verifyDigest ownerName (DSData tag alg typ dig) k =
      tag == fromIntegral (keyTag k)
  &&  alg == _keyAlgorithm k
  &&  keyDigest ownerName typ k == dig
