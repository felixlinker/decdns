{-# LANGUAGE ViewPatterns #-}
module DNS.SEC.KeysSpec (tests) where

import Data.Word (Word16)
import DNS.Record (Type (DNSKEY), Record, record, Cat (IN), RData (RData, rtype))
import DNS.SEC.Keys (keyTag, dnsKeyFlagsFromInt, keyDigest)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import DNS.SEC.Crypto (algorithmFromInt, AlgorithmType, DigestType (SHA256))
import Parsing.Bytes (base64)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Text.Parsec (runParser)
import Util.Bytes (toHex, Serialize(serialize))
import DNS.NameSpec (mkName)
import DNS.Name (Named(Named))

thd :: (a, b, c) -> c
thd (_, _, c) = c

tests :: TestTree
tests =
  testGroup
    "DNS.SEC.Keys"
    [ testCase "canonical name wire format encodes lowercase labels and root" $
        serialize (mkName ["a", "com", ""]) @?= BS.pack [1, 97, 3, 99, 111, 109, 0]
    , testGroup "key tag" (zipWith (testKeyTag . show) [0 :: Int, 1..] dnskeyTestCases)
    , testGroup "key digest" (zipWith (testKeyDigest . show) [0 :: Int, 1..] (filter (not . null . thd) dnskeyTestCases)) ]

type KeyTestCase = (Record String, Word16, String)  -- DNSKEY, key tag, SHA256 digest

parseBase64 :: String -> BS.ByteString
parseBase64 = either undefined id . runParser base64 () "<tests>"

algFromInt :: Int -> AlgorithmType
algFromInt = fromMaybe undefined . algorithmFromInt

r :: [String] -> Type -> Record String
r n = record (mkName n) . RData IN 100

dnskeyTestCases :: [KeyTestCase]
dnskeyTestCases =
  [ ( r ["com", "", ""] $ DNSKEY (dnsKeyFlagsFromInt 256) (algFromInt 13) (parseBase64 "ZMpXruovycFOC7+ebNH5sG+Gir52Skfa7RkkaEYMi662o3XCMTdQE3NiPdADM4MPKx/FJxjpscuPPeA+cxpTOA==")
    , 23099
    , "")
  , ( r ["com", ""] $ DNSKEY (dnsKeyFlagsFromInt 257) (algFromInt 13) (parseBase64 "LlZrrBEO3dkRiacwuiG/b3uXuztOrWKwDqwHs+Pj0QVyD3ec1UMFhiwceoDeopWdlri6S+Sa932tV5hHx+rU2g==")
    , 24978
    , "")
  , ( r ["a", "com", ""] $ DNSKEY (dnsKeyFlagsFromInt 256) (algFromInt 13) (parseBase64 "9hQ+LE+VB2kZp9PqZp7X2JMf6ioKoqYqqPf9gs5cV1UVLay+DAKXle++Da8AAKIoeiQyKgMQC/JpdfLYirUbVg==")
    , 30814
    , "")
  , ( r ["a", "com", ""] $ DNSKEY (dnsKeyFlagsFromInt 257) (algFromInt 13) (parseBase64 "2iBXCSBo2Dvk4eEPxxdfogVY5J0Woop4tpS6RD27va9od/QOpb5dbBKlI4gMNljr47o9rRruikkUpyEFnJ1DiA==")
    , 59726
    , "BAFD7C691EB978754C2CA32C5697AC4B21C8DC6534DA5EFDFBE9A7B17882501B") ]

testKeyTag :: String -> KeyTestCase -> TestTree
testKeyTag label (Named _ (rtype -> DNSKEY flags algorithm publicKey), expectedTag, _) = testCase label $
  keyTag flags algorithm publicKey @?= expectedTag
testKeyTag _ _ = undefined

testKeyDigest :: String -> KeyTestCase -> TestTree
testKeyDigest label (Named owner (rtype -> DNSKEY flags algorithm publicKey), _, expectedDigest) = testCase label $
  toHex (keyDigest SHA256 owner flags algorithm publicKey) @?= expectedDigest
testKeyDigest _ _ = undefined
