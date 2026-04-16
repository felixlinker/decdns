{-# LANGUAGE ViewPatterns #-}
module DNS.SEC.Keys
  ( DNSKeyFlag(..)
  , dnsKeyFlagsToInt
  , dnsKeyFlagToInt
  , knownDNSKeyFlagsMask
  , dnsKeyFlagsFromInt
  , nameWireFormat
  , keyTag
  , keyDigest ) where

import Data.Bits ((.|.), (.&.), complement, shiftR, shiftL)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import Data.List (intersperse)
import Data.Word (Word8, Word16, Word32)
import qualified Crypto.Hash.SHA256 as S256

import DNS.SEC.Crypto
import DNS.Name (Name(..))
import Data.Char (toLower)

data DNSKeyFlag = ZoneKey | Other | SecureEntryPoint
  deriving (Eq, Ord)

dnsKeyFlagsToInt :: [DNSKeyFlag] -> Int
dnsKeyFlagsToInt = foldr ((.|.) . dnsKeyFlagToInt) 0

dnsKeyFlagToInt :: DNSKeyFlag -> Int
dnsKeyFlagToInt ZoneKey = 256
dnsKeyFlagToInt Other = 128
dnsKeyFlagToInt SecureEntryPoint = 1

knownDNSKeyFlagsMask :: Int
knownDNSKeyFlagsMask = dnsKeyFlagsToInt [ZoneKey, Other, SecureEntryPoint]

dnsKeyFlagsFromInt :: Int -> [DNSKeyFlag]
dnsKeyFlagsFromInt flags =
  concat
    [ [ZoneKey | flags .&. dnsKeyFlagToInt ZoneKey /= 0]
    , [SecureEntryPoint | flags .&. dnsKeyFlagToInt SecureEntryPoint /= 0]
    , [Other | flags .&. otherDNSKeyFlagsMask /= 0]
    ]
  where
    otherDNSKeyFlagsMask = dnsKeyFlagToInt Other .|. complement knownDNSKeyFlagsMask

dnsKeyWireFormat :: [DNSKeyFlag] -> AlgorithmType -> BS.ByteString -> BS.ByteString
dnsKeyWireFormat (dnsKeyFlagsToInt -> flags) (algorithmToInt -> alg) key = foldr BS.cons key flagsProtoAlg
  where
    flagsProtoAlg :: [Word8]
    flagsProtoAlg =
      [ fromIntegral (shiftR flags 8)
      , fromIntegral flags
      , 3 -- Protocol
      , fromIntegral alg ]

keyTag :: [DNSKeyFlag] -> AlgorithmType -> BS.ByteString -> Word16
keyTag flags alg key =
  let i32 = foldr ((+) . fromIntegral) (0 :: Word32) asDoubleOctets
      lower = fromIntegral i32 :: Word16
      upper = fromIntegral (i32 `shiftR` 16) :: Word16
  in  lower + upper
  where
    wireFormat :: BS.ByteString
    wireFormat = dnsKeyWireFormat flags alg key

    -- Alternatingly, shift Word8 left by 8 to make them the high 8 bits of a
    -- Word16
    asDoubleOctets :: [Word16]
    asDoubleOctets = zipWith (shiftL . fromIntegral) (BS.unpack wireFormat) (intersperse 0 (repeat 8))

nameWireFormat :: Name String -> BS.ByteString
nameWireFormat (Name ls) = foldr ((<>) . encodeLabel) BS.empty ls
  where
    encodeLabel :: String -> BS.ByteString
    encodeLabel (CBS.pack -> label) = BS.cons (fromIntegral (CBS.length label)) label

keyDigest :: DigestType -> Name String -> [DNSKeyFlag] -> AlgorithmType -> BS.ByteString -> BS.ByteString
keyDigest SHA256 ownerName flags alg key = S256.hash $ canonicalName <> wireFormat
  where
    wireFormat :: BS.ByteString
    wireFormat = dnsKeyWireFormat flags alg key

    canonicalName :: BS.ByteString
    canonicalName = nameWireFormat (fmap (map toLower) ownerName)
keyDigest _ _ _ _ _ = undefined
