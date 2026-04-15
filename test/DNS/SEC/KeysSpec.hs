module DNS.SEC.KeysSpec (tests) where

import Data.Word (Word16)
import DNS.Record (Type (DNSKEY))
import DNS.SEC.Keys (keyTag, dnsKeyFlagsFromInt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import DNS.SEC.Crypto (algorithmFromInt, AlgorithmType)
import Parsing.Bytes (base64)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Text.Parsec (runParser)

tests :: TestTree
tests =
  testGroup
    "DNS.SEC.Keys"
    [ testGroup "keyTag" (zipWith (testKeyTag . show) [0 :: Int, 1..] dnskeyKeyTagCases) ]

type KeyTagCase = (Type, Word16)

parseBase64 :: String -> BS.ByteString
parseBase64 = either undefined id . runParser base64 () "<tests>"

algFromInt :: Int -> AlgorithmType
algFromInt = fromMaybe undefined . algorithmFromInt

dnskeyKeyTagCases :: [KeyTagCase]
dnskeyKeyTagCases =
  [ ( DNSKEY (dnsKeyFlagsFromInt 256) (algFromInt 13) (parseBase64 "ZMpXruovycFOC7+ebNH5sG+Gir52Skfa7RkkaEYMi662o3XCMTdQE3NiPdADM4MPKx/FJxjpscuPPeA+cxpTOA==")
    , 23099)
  , ( DNSKEY (dnsKeyFlagsFromInt 257) (algFromInt 13) (parseBase64 "LlZrrBEO3dkRiacwuiG/b3uXuztOrWKwDqwHs+Pj0QVyD3ec1UMFhiwceoDeopWdlri6S+Sa932tV5hHx+rU2g==")
    , 24978)
  , ( DNSKEY (dnsKeyFlagsFromInt 256) (algFromInt 13) (parseBase64 "9hQ+LE+VB2kZp9PqZp7X2JMf6ioKoqYqqPf9gs5cV1UVLay+DAKXle++Da8AAKIoeiQyKgMQC/JpdfLYirUbVg==")
    , 30814)
  , ( DNSKEY (dnsKeyFlagsFromInt 257) (algFromInt 13) (parseBase64 "2iBXCSBo2Dvk4eEPxxdfogVY5J0Woop4tpS6RD27va9od/QOpb5dbBKlI4gMNljr47o9rRruikkUpyEFnJ1DiA==")
    , 59726) ]

testKeyTag :: String -> KeyTagCase -> TestTree
testKeyTag label (DNSKEY flags algorithm publicKey, expectedTag) = testCase label $
  keyTag flags algorithm publicKey @?= expectedTag
testKeyTag _ (_, _) = undefined
