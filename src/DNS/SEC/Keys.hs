module DNS.SEC.Keys
  ( DNSKeyFlag(..)
  , dnsKeyFlagsToInt
  , dnsKeyFlagToInt
  , knownDNSKeyFlagsMask
  , dnsKeyFlagsFromInt ) where

import Data.Bits ((.|.), (.&.), complement)

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
