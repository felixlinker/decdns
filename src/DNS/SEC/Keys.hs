module DNS.SEC.Keys
  ( DNSKeyFlag(..)
  , dnsKeyFlagsToInt
  , dnsKeyFlagToInt
  , knownDNSKeyFlagsMask
  , dnsKeyFlagsFromInt
  , dnsKeyWireFormat
  , keyTag
  , keyDigest ) where

import Data.Bits ((.|.), (.&.), complement, shiftR, shiftL)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.List (intersperse)
import Data.Word (Word16, Word32)
import Crypto.Hash (Digest, hash)
import qualified Crypto.Hash.Algorithms as Hash

import DNS.SEC.Crypto
import DNS.Name (Name(..))
import Util.Bytes (Serialize(serialize), word16, word8)

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
dnsKeyWireFormat flags alg key =
  foldr (<>) key  [ word16 (dnsKeyFlagsToInt flags)
                  , word8 (3 :: Int) -- Protocol
                  , word8 (algorithmToInt alg) ]

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

keyDigest :: DigestType -> Name String -> [DNSKeyFlag] -> AlgorithmType -> BS.ByteString -> BS.ByteString
keyDigest digType ownerName flags alg key = case digType of
  SHA1    -> BA.convert (digest :: Digest Hash.SHA1)
  SHA256  -> BA.convert (digest :: Digest Hash.SHA256)
  SHA384  -> BA.convert (digest :: Digest Hash.SHA384)
  _ -> undefined
  where
    digest :: Hash.HashAlgorithm a => Digest a
    digest = hash $ serialize ownerName <> dnsKeyWireFormat flags alg key
