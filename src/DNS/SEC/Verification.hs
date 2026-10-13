{-# LANGUAGE ViewPatterns #-}
module DNS.SEC.Verification
  ( isDNSSECKey
  , verifySignature
  , keyTag
  , keyDigest
  , verifyDigest
  , nsec3Hash
  , verifyClosestEncloserProof
  , nsec3UnsignedSubzone
  , nsecUnsignedSubzone ) where

import Crypto.Error (maybeCryptoError)
import Crypto.Hash (Digest, hash)
import qualified Crypto.Hash.Algorithms as Hash
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Crypto.PubKey.Ed448 as Ed448
import Data.Bits (shiftR, shiftL)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.List (intersperse, sortOn)
import qualified Data.List.NonEmpty as NE
import Data.Word (Word16, Word32)
import Control.Monad (guard)

import DNS.Name
import qualified DNS.RecordType as T
import DNS.Record
import Util.Bytes
import DNS.SEC.Crypto
import Data.Maybe (fromMaybe)
import Data.Functor (void)
import Util.List (snub)

-- See: https://datatracker.ietf.org/doc/html/rfc4035#section-2.4
isDNSSECKey :: DNSKEYData -> Bool
isDNSSECKey (DNSKEYData { _flags = fs }) = ZoneKey `elem` fs

verifySignature :: Name String -> [Type] -> RRSIGData BS.ByteString -> DNSKEYData -> Bool
verifySignature (canonicalName -> owner) (map canonicalType -> rrset) (canonicalRRSIGData -> sig) pk@(DNSKEYData _ _ pkBytes) = fromMaybe False $ do
  guard (isDNSSECKey pk)
  guard (_signatureAlgorithm sig == _keyAlgorithm pk)
  case _signatureAlgorithm sig of
    RSASHA1 -> verifyRSA Hash.SHA1
    RSASHA1_NSEC3_SHA1 -> verifyRSA Hash.SHA1
    RSASHA256 -> verifyRSA Hash.SHA256
    RSASHA512 -> verifyRSA Hash.SHA512
    ECDSAP256SHA256 -> verifyECDSA Hash.SHA256
    ECDSAP384SHA384 -> verifyECDSA Hash.SHA384
    ED25519 -> do
      cSig <- maybeCryptoError $ Ed25519.signature (_signature sig)
      cPk <- maybeCryptoError $ Ed25519.publicKey pkBytes
      return $ Ed25519.verify cPk signedData cSig
    ED448 -> do
      cSig <- maybeCryptoError $ Ed448.signature (_signature sig)
      cPk <- maybeCryptoError $ Ed448.publicKey pkBytes
      return $ Ed448.verify cPk signedData cSig
    _ -> Nothing
  where
    signedData :: BS.ByteString
    signedData = BS.concat (serialize (void sig) : serializedRRSet)

    verifyRSA :: RSA.HashAlgorithmASN1 a => a -> Maybe Bool
    verifyRSA hashAlg = do
      cPk <- rsaPk pkBytes
      return $ RSA.verify (Just hashAlg) cPk signedData (_signature sig)

    verifyECDSA :: Hash.HashAlgorithm a => a -> Maybe Bool
    verifyECDSA hashAlg = do
      cSig <- ecdsaSig (_signatureAlgorithm sig) (_signature sig)
      cPk <- ecdsaPk (_signatureAlgorithm sig) pkBytes
      return $ ECDSA.verify hashAlg cPk cSig signedData

    lc :: Int
    lc = _labels sig

    originalOwner :: Name String
    originalOwner = case owner of
      n@(Name ls) ->
        let lc' = labelCount n
        in  if lc' > lc then Name ("*" NE.:| NE.drop (lc' - lc) ls) else n

    mkRecord :: Type -> StandardRecord
    mkRecord = Named originalOwner . RData IN (_originalTTL sig)

    serializedRRSet :: [BS.ByteString]
    serializedRRSet = snub $ map (serialize . mkRecord) $ sortOn serialize rrset

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
  &&  isDNSSECKey k

-- See: https://datatracker.ietf.org/doc/html/rfc5155#autoid-29
nsec3Hash :: NSEC3Data -> Name String -> BS.ByteString
nsec3Hash (NSEC3Data NSEC3_SHA1 _ iter salt _ _) (serialize . canonicalName -> x) = ih iter
  where
    ih ::  Int -> BS.ByteString
    ih 0 = BA.convert (hash $ x <> salt :: Digest Hash.SHA1)
    ih n =
      let rec = ih (n - 1)
      in  BA.convert (hash $ rec <> salt :: Digest Hash.SHA1)

-- TODO: Compare with RFC 7129, Sec. 5.5.
-- TODO: Compare with RFC 6840, Sec. 4.1.
-- See: https://datatracker.ietf.org/doc/html/rfc5155#section-8.3 and
-- https://datatracker.ietf.org/doc/html/rfc5155#section-8.9
verifyClosestEncloserProof :: Name String -> Name String -> NSEC3Data -> Name String -> NSEC3Data -> Bool
verifyClosestEncloserProof (canonicalName -> qname) hashedClosestEncloser closestEncloserData@(NSEC3Data { _nsec3TypeBitMaps = ts, _nsec3Flags = flags }) hashedOwner (NSEC3Data { _nextHashedOwner = next }) =
  fromMaybe False $ do
    guard (OPT_OUT `elem` flags)
    guard (T.DNAME `notElem` ts)
    -- T.NS `elem` ts ==> T.SOA `elem` ts
    guard (T.NS `notElem` ts || T.SOA `elem` ts)
    let Name (b32 NE.:| _) = hashedClosestEncloser
    closestEncloserHash <- fromB32 b32
    nonExHash <- findNonExHash Nothing qname closestEncloserHash
    let Name (b32' NE.:| _) = hashedOwner
    hashedOwnerHash <- fromB32 b32'
    return (hashedOwnerHash < nonExHash && nonExHash < next)
  where
    -- Find the original name of the closest encloser given the closest
    -- encloser's hashed owner name. Return the hash of the immediate child
    -- name.
    findNonExHash :: Maybe BS.ByteString -> Name String -> BS.ByteString -> Maybe BS.ByteString
    findNonExHash acc n hashed =
      let hn = nsec3Hash closestEncloserData n
      in if hn == hashed
        then acc
        else do
          n' <- Name <$> NE.nonEmpty (NE.tail $ labels n)
          findNonExHash (Just hn) n' hashed

checkUnsigedDelegationBitMap :: [T.Type] -> Bool
checkUnsigedDelegationBitMap ts =
        T.DS `notElem` ts
    &&  T.NS `elem` ts
    &&  T.SOA `notElem` ts

-- See: https://datatracker.ietf.org/doc/html/rfc5155#section-8.9
nsec3UnsignedSubzone :: Name String -> Name String -> NSEC3Data -> Bool
nsec3UnsignedSubzone originalOwner hashedOwner dat@(NSEC3Data { _nsec3TypeBitMaps = ts }) = fromMaybe False $ do
  let Name (b32 NE.:| _) = hashedOwner
  hashedOwnerHash <- fromB32 b32
  return $ nsec3Hash dat originalOwner == hashedOwnerHash
    && checkUnsigedDelegationBitMap ts

-- TODO: Check an attacker could use NSEC data from the child to lie about the
-- parent. This should be rejected due to an illegal signer's name. This also
-- goes the other way around. The signer name length check should catch this.
-- NOTE: I now use the check for T.SOA to enforce this. I should also check,
-- though, whether there are issues with signer's name.
nsecUnsignedSubzone :: Name String -> Name String -> NSECData -> Bool
nsecUnsignedSubzone owner nsecOwner (NSECData { _nsecTypeBitMaps = ts }) =
  owner == nsecOwner && checkUnsigedDelegationBitMap ts
