{-# LANGUAGE ViewPatterns #-}
module DNS.SEC.Keys
  ( DNSKeyFlag(..)
  , dnsKeyFlagsToInt
  , dnsKeyFlagToInt
  , knownDNSKeyFlagsMask
  , dnsKeyFlagsFromInt
  , keyTag ) where

import Data.Bits ((.|.), (.&.), complement, shiftR, shiftL)
import Data.Word (Word8, Word16, Word32)
import DNS.SEC.Crypto (AlgorithmType, algorithmToInt)
import qualified Data.ByteString as BS
import Data.List (intersperse)

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

keyTag :: [DNSKeyFlag] -> AlgorithmType -> BS.ByteString -> Word16
keyTag (dnsKeyFlagsToInt -> flags) (algorithmToInt -> alg) key =
  let i32 = foldr ((+) . fromIntegral) (0 :: Word32) asDoubleOctets
      lower = fromIntegral i32 :: Word16
      upper = fromIntegral (i32 `shiftR` 16) :: Word16
  in  lower + upper
  where
    flagsProtoAlg :: [Word8]
    flagsProtoAlg =
      [ fromIntegral (shiftR flags 8)
      , fromIntegral flags
      , 3 -- Protocol
      , fromIntegral alg ]

    wireFormat :: BS.ByteString
    wireFormat = foldr BS.cons key flagsProtoAlg

    -- Alternatingly, shift Word8 left by 8 to make them the high 8 bits of a
    -- Word16
    asDoubleOctets :: [Word16]
    asDoubleOctets = zipWith (shiftL . fromIntegral) (BS.unpack wireFormat) (intersperse 0 (repeat 8))
