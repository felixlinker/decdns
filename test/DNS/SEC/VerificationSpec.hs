{-# LANGUAGE ViewPatterns #-}
module DNS.SEC.VerificationSpec (tests) where

import Data.List (uncons)
import Data.Word (Word16)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, (@?=), testCase)
import Test.Tasty.QuickCheck
  ( Gen
  , Property
  , chooseInt
  , counterexample
  , elements
  , forAll
  , oneof
  , testProperty, chooseAny, suchThat
  )
import DNS.SEC.Crypto
  ( AlgorithmType (ECDSAP256SHA256, ECDSAP384SHA384)
  , DigestType (SHA256)
  , NSEC3DigestType (NSEC3_SHA1)
  )
import Parsing.Bytes (base32Hex, base64, hex)
import qualified Data.ByteString as BS
import Text.Parsec (runParser)
import Util.Bytes (toHex)

import DNS.Record
import DNS.SEC.Keys
import DNS.SEC.Verification
import DNS.Name (Name, Named (Named))
import DNS.NameSpec (mkName)
import qualified DNS.RecordType as RT
import Data.Maybe (fromMaybe)

thd :: (a, b, c) -> c
thd (_, _, c) = c

tests :: TestTree
tests =
  testGroup
    "DNS.SEC.Keys"
    [ testGroup "key tag" (zipWith (testKeyTag . show) [0 :: Int, 1..] dnskeyTestCases)
    , testGroup "key digest" (zipWith (testKeyDigest . show) [0 :: Int, 1..] (filter (not . null . thd) dnskeyTestCases))
    , testGroup "NSEC3 hash" (zipWith (testNSEC3Hash . show) [0 :: Int, 1..] nsec3Vectors)
    , testCase "verifies signatures from the static signed zones" testSignedZones
    , testProperty "rejects a modified RRSIG or incomplete RRset" propRejectsMutatedVector
    ]

type KeyTestCase = (Record String, Word16, String)  -- DNSKEY, key tag, SHA256 digest

parseBase64 :: String -> BS.ByteString
parseBase64 = either undefined id . runParser base64 () "<tests>"

parseHex :: String -> BS.ByteString
parseHex = either undefined id . runParser hex () "<tests>"

parseBase32Hex :: String -> BS.ByteString
parseBase32Hex = either undefined id . runParser base32Hex () "<tests>"

r :: [String] -> Type -> Record String
r n = record (mkName n) . RData IN 100

-- TODO: I see four test cases but the test suite only seems to run one
dnskeyTestCases :: [KeyTestCase]
dnskeyTestCases =
  [ ( r ["com", "", ""] $ DNSKEY comZsk
    , 23099
    , "")
  , ( r ["com", ""] $ DNSKEY comKsk
    , 24978
    , "")
  , ( r ["a", "com", ""] $ DNSKEY aComZsk
    , 30814
    , "")
  , ( r ["a", "com", ""] $ DNSKEY aComKsk
    , 59726
    , "BAFD7C691EB978754C2CA32C5697AC4B21C8DC6534DA5EFDFBE9A7B17882501B") ]

testKeyTag :: String -> KeyTestCase -> TestTree
testKeyTag label (Named _ (rtype -> DNSKEY k), expectedTag, _) = testCase label $
  keyTag k @?= expectedTag
testKeyTag _ _ = undefined

testKeyDigest :: String -> KeyTestCase -> TestTree
testKeyDigest label (Named owner (rtype -> DNSKEY k), _, expectedDigest) = testCase label $
  toHex (keyDigest owner SHA256 k) @?= expectedDigest
testKeyDigest _ _ = undefined

type SignatureVector = (Name String, [Type], RRSIGData BS.ByteString, DNSKEYData)

testSignedZones :: IO ()
testSignedZones = mapM_ verifyVector signatureVectors
  where
    verifyVector (owner, rrset, sig, key) =
      assertBool
        ("signature verification failed for " ++ show owner ++ " " ++ show (_typeCovered sig))
        (verifySignature owner rrset sig key)

propRejectsMutatedVector :: Property
propRejectsMutatedVector =
  forAll (elements signatureVectors) $ \original ->
    forAll (mutateSignatureVector original) $ \(owner, rrset, sig, key) ->
      counterexample ("mutated vector still verified: " ++ show (owner, rrset, sig, key)) $
        not (verifySignature owner rrset sig key)

mutateSignatureVector :: SignatureVector -> Gen SignatureVector
mutateSignatureVector (owner, rrset, sig, key) =
  oneof
    [ (\sig' -> (owner, rrset, sig', key)) <$> mutateRRSIG sig
    , do
      rrset' <- dropOne rrset
      return (owner, rrset', sig, key)
    ]

mutateRRSIG :: RRSIGData BS.ByteString -> Gen (RRSIGData BS.ByteString)
mutateRRSIG sig =
  oneof
    [ (\x -> sig { _typeCovered = x }) <$> differentRecordType (_typeCovered sig)
    , pure sig { _signatureAlgorithm = ECDSAP384SHA384 }
    , pure sig { _labels = _labels sig + 1 }
    , pure sig { _originalTTL = _originalTTL sig + 1 }
    , pure sig { _signatureExpiration = _signatureExpiration sig + 1 }
    , pure sig { _signatureInception = _signatureInception sig + 1 }
    , pure sig { _verificationKeyTag = _verificationKeyTag sig + 1 }
    , pure sig { _signersName = differentName (_signersName sig) }
    , (\x -> sig { _signature = x }) <$> byteFlip (_signature sig)
    ]

differentName :: Name String -> Name String
differentName name
  | name == comName = aComName
  | otherwise = comName

differentRecordType :: RT.Type -> Gen RT.Type
differentRecordType typ = elements (filter (/= typ) RT.allTypes)

dropOne :: [a] -> Gen [a]
dropOne l = do
  i <- chooseInt (0, length l - 1)
  let (prefix, suffix) = splitAt i l
  let (_, suffix') = fromMaybe undefined $ uncons suffix
  return $ prefix ++ suffix'

byteFlip :: BS.ByteString -> Gen BS.ByteString
byteFlip bytes = do
  byteIndex <- chooseInt (0, BS.length bytes - 1)
  let (prefix, suffix) = BS.splitAt byteIndex bytes
  -- choice of byteIndex ensures that suffix is non-empty
  let (h, t) = fromMaybe undefined $ BS.uncons suffix
  byte' <- suchThat chooseAny (/= h)
  return $ prefix <> BS.singleton byte' <> t

signatureVectors :: [SignatureVector]
signatureVectors =
  [ vector comName [soa ns1Name] RT.SOA 1 1200 23099 comName comZsk
      "Zudwp50+lG2UoUgg+dQwrEdbYBPu/6JvmO9dJE58XE8seqNAQvWl4JL0d7il/YOQsrV4lAyNfHlZvzTZ7rDvZg=="
  , vector comName [NSEC (NSECData aComName [RT.SOA, RT.RRSIG, RT.NSEC, RT.DNSKEY])] RT.NSEC 1 0 23099 comName comZsk
      "nswb0Ix/dCBI+y31MoZhoWoxJYGVW5t22c+/Fi+UnhtF5yQamKHm3KOJXiYkVxOyZQS8lSr0J9cgGILbXfIf3g=="
  , vector comName [DNSKEY comZsk, DNSKEY comKsk] RT.DNSKEY 1 1200 23099 comName comZsk
      "5GEccqRu3p/gf2VuPGr7tN40S+vDppqbyldfP5EaDPalhCo9SWVHy+50HvwEXMrqMI4EBOwqSDnFJc2hr8YnSw=="
  , vector comName [DNSKEY comZsk, DNSKEY comKsk] RT.DNSKEY 1 1200 24978 comName comKsk
      "orNm/I5iRFBBNHLTSPG712KhV4gh6/VjQFBdxHM/lqwfA81asy+lC1AznHsc2WWlZ1h2bXrexrKfP2Ngj7xqeg=="
  , vector aComName [DS (DSData 59726 ECDSAP256SHA256 SHA256 aComDigest)] RT.DS 2 1200 23099 comName comZsk
      "TZEsjl0wTWXTJ8On50DV1WmsOsje/5Gx1dP89xmt7vgq73q1XnMeF9tae6ocIUsnu1RSas1XXaqIhRpyiK1uRg=="
  , vector aComName [NSEC (NSECData ns1Name [RT.NS, RT.DS, RT.RRSIG, RT.NSEC])] RT.NSEC 2 0 23099 comName comZsk
      "bK1rYhCk9zXCm7GunxGmodpu3X+DBP0sHDmQYotNhvlpoL6eZMYw7ymp8IQ3mZ8pTbZmuzR82K5lLQpOZV+Kaw=="
  , vector ns1Name [A (read "1.2.3.4")] RT.A 2 1200 23099 comName comZsk
      "3XdYvOXShYuZLCZ5wHEY/faPPJBAOt0UCNiLtPNCHqi9XegqQmJcz/rES40y+KYA9hrwfXJ2fCUIHgNcCPoKfA=="
  , vector ns1Name [NSEC (NSECData ns2Name [RT.A, RT.RRSIG, RT.NSEC])] RT.NSEC 2 0 23099 comName comZsk
      "fXUPVnTSGY5m+lJ+ENLTmfOHpBhq0Y/VvazJ6dC7scZ+iDpzwdL61Isl6wCbF5EqIrnpRejI50FiS4W6cxH00g=="
  , vector ns2Name [A (read "4.5.6.7")] RT.A 2 1200 23099 comName comZsk
      "byZ0YAt1dgdHxVUPoMGCpj/1So7bR2PCakaT/Ll97DP44bvkExW2Z1kq9nVXDh5Qfcpz65lP9mAUrVQiQ4juSQ=="
  , vector ns2Name [NSEC (NSECData comName [RT.A, RT.RRSIG, RT.NSEC])] RT.NSEC 2 0 23099 comName comZsk
      "Pdm2XQ2CFdvop2kkxcsnCPh9T2ouCO5cLcFPmQMOuTavHP5T1O28075HT3W77k6GUsVh2MNDnYZ7e9BHaKnlAA=="
  , vector aComName [soa ns2Name] RT.SOA 2 1200 30814 aComName aComZsk
      "Ne2VNOMCBogf6920EM6k0gt5t/PJA/BufL/5P0vxhvlYe1qPB1p1JDYJeI8QoSVtt3nBnGfcXU13ZuaVwvT7+g=="
  , vector aComName [A (read "8.9.10.11")] RT.A 2 1200 30814 aComName aComZsk
      "lRat5+VG8t5hH7tTdFa6kbnmH1ZhYW0sxLn5RespS39gNw19KBaRpLVas6gunRUlpj09of//FARF/zJU+Znwyw=="
  , vector aComName [NSEC (NSECData aComName [RT.A, RT.SOA, RT.RRSIG, RT.NSEC, RT.DNSKEY])] RT.NSEC 2 0 30814 aComName aComZsk
      "4JOKAiVGKxHbOk9YRJhIZbb0C1ND7dBkQQfhPULhFZUGplKCxKu5uNTRRzo41HqwiXIJ6wz8AbITCVNm7NjLyA=="
  , vector aComName [DNSKEY aComZsk, DNSKEY aComKsk] RT.DNSKEY 2 1200 30814 aComName aComZsk
      "waRQ7SaQ+Qp6MBE5dtQ1BJ4etYuH1tDalYLHXc7G/BMCHdopW00bZL0BRE+8M8XvPAzUvDWALC0c/qGwimTwYA=="
  , vector aComName [DNSKEY aComZsk, DNSKEY aComKsk] RT.DNSKEY 2 1200 59726 aComName aComKsk
      "5Q30z9xuwO9inJvXuLh5xqXFBr6zTCSEaMhJ5v3fxVb2cAnXDHWiUHsw9ADLTTkuo7+nTMjoT83Hxaa2lun0kw=="
  ]

vector :: Name String -> [Type] -> RT.Type -> Int -> Int -> Int -> Name String -> DNSKEYData -> String -> SignatureVector
vector owner rrset covered labels ttl tag signer key signature =
  ( owner
  , rrset
  , RRSIGData covered ECDSAP256SHA256 labels ttl signatureExpiration signatureInception tag signer (parseBase64 signature)
  , key
  )

soa :: Name String -> Type
soa mname = SOA mname exampleName 0 0 0 0 0

signatureExpiration, signatureInception :: Int
signatureExpiration = 1783775562
signatureInception = 1781183562

comName, aComName, ns1Name, ns2Name, exampleName :: Name String
comName = mkName ["com", ""]
aComName = mkName ["a", "com", ""]
ns1Name = mkName ["ns1", "com", ""]
ns2Name = mkName ["ns2", "com", ""]
exampleName = mkName ["example", "com", ""]

comZsk, comKsk, aComZsk, aComKsk :: DNSKEYData
comZsk = dnskey 256 "ZMpXruovycFOC7+ebNH5sG+Gir52Skfa7RkkaEYMi662o3XCMTdQE3NiPdADM4MPKx/FJxjpscuPPeA+cxpTOA=="
comKsk = dnskey 257 "LlZrrBEO3dkRiacwuiG/b3uXuztOrWKwDqwHs+Pj0QVyD3ec1UMFhiwceoDeopWdlri6S+Sa932tV5hHx+rU2g=="
aComZsk = dnskey 256 "9hQ+LE+VB2kZp9PqZp7X2JMf6ioKoqYqqPf9gs5cV1UVLay+DAKXle++Da8AAKIoeiQyKgMQC/JpdfLYirUbVg=="
aComKsk = dnskey 257 "2iBXCSBo2Dvk4eEPxxdfogVY5J0Woop4tpS6RD27va9od/QOpb5dbBKlI4gMNljr47o9rRruikkUpyEFnJ1DiA=="

dnskey :: Int -> String -> DNSKEYData
dnskey flags = DNSKEYData (dnsKeyFlagsFromInt flags) ECDSAP256SHA256 . parseBase64

aComDigest :: BS.ByteString
aComDigest = parseHex "BAFD7C691EB978754C2CA32C5697AC4B21C8DC6534DA5EFDFBE9A7B17882501B"

type NSEC3HashVector = (Name String, BS.ByteString)

testNSEC3Hash :: String -> NSEC3HashVector -> TestTree
testNSEC3Hash label (name, expectedHash) = testCase label $
  nsec3Hash nsec3Data name @?= expectedHash

nsec3Data :: NSEC3Data
nsec3Data = NSEC3Data NSEC3_SHA1 [] 12 (parseHex "aabbccdd") BS.empty []

-- Source: https://datatracker.ietf.org/doc/html/rfc5155#appendix-A
nsec3Vectors :: [NSEC3HashVector]
nsec3Vectors =
  [ (mkName ["example", ""], parseBase32Hex "0p9mhaveqvm6t7vbl5lop2u3t2rp3tom")
  , (mkName ["a", "example", ""], parseBase32Hex "35mthgpgcu1qg68fab165klnsnk3dpvl")
  , (mkName ["ai", "example", ""], parseBase32Hex "gjeqe526plbf1g8mklp59enfd789njgi")
  , (mkName ["ns1", "example", ""], parseBase32Hex "2t7b4g4vsa5smi47k61mv5bv1a22bojr")
  , (mkName ["ns2", "example", ""], parseBase32Hex "q04jkcevqvmu85r014c7dkba38o0ji5r")
  , (mkName ["w", "example", ""], parseBase32Hex "k8udemvp1j2f7eg6jebps17vp3n8i58h")
  , (mkName ["*", "w", "example", ""], parseBase32Hex "r53bq7cc2uvmubfu5ocmm6pers9tk9en")
  , (mkName ["x", "w", "example", ""], parseBase32Hex "b4um86eghhds6nea196smvmlo4ors995")
  , (mkName ["y", "w", "example", ""], parseBase32Hex "ji6neoaepv8b5o6k4ev33abha8ht9fgc")
  , (mkName ["x", "y", "w", "example", ""], parseBase32Hex "2vptu5timamqttgl4luu9kg21e0aor3s")
  , (mkName ["xx", "example", ""], parseBase32Hex "t644ebqk9bibcna874givr6joj62mlhv")
  , (mkName ["2t7b4g4vsa5smi47k61mv5bv1a22bojr", "example", ""], parseBase32Hex "kohar7mbb8dc2ce8a9qvl8hon4k53uhi")
  ]
