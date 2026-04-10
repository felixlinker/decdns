module Parsing.ZoneFileSpec (tests) where

import qualified Data.ByteString as BS
import Data.Functor.Identity (runIdentity)

import DNS.Name (Named (name, value))
import DNS.Record (RData (rttl), Type (DNSKEY, DS))
import DNS.SEC.Crypto (AlgorithmType (RSASHA256), DigestType (SHA256))
import DNS.ZoneFile (allData, fromRecordList)
import Parsing.Data (recordTypeData)
import Parsing.State (emptyState)
import Parsing.ZoneFile (parseZoneFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Parsec (runParserT)
import Control.Monad (foldM)
import Data.List (unsnoc)

tests :: TestTree
tests =
  testGroup
    "Parsing.ZoneFile"
    [ testCase "can parse a record for the root label" parsesRootRecord
    , testCase "parses $TTL and applies it to records without explicit TTL" parsesTTLDirective
    , testCase "parses omitted owner when line starts with IN" parsesOmittedOwnerStartingWithClass
    , testCase "backtracks to explicit owner when owner is IN" parsesExplicitOwnerNamedIN
    , testCase "parses multiline records followed by inline comments" parsesMultilineRecordWithInlineComment
    , testProperty "parses multiline DNSKEY base64 blobs" propParsesMultilineDNSKEYBlob
    , testProperty "parses multiline DS hex blobs" propParsesMultilineDSBlob
    ]

parsesRootRecord :: Assertion
parsesRootRecord = do
  let zone = ". IN 1200 A 192.0.2.1\n"
  rs <- either (fail . show) return =<< parseZoneFile "<test>" zone
  length rs @?= 1

parsesTTLDirective :: Assertion
parsesTTLDirective = do
  let input = unlines
        [ "$ORIGIN example.com."
        , "$TTL 3600"
        , "@ IN A 192.0.2.1"
        , "www IN A 192.0.2.2"
        ]

  records <- either (fail . show) pure =<< parseZoneFile "<test>" input
  length records @?= 2
  mapM_ ((3600 @=?) . rttl . value) records

parsesOmittedOwnerStartingWithClass :: Assertion
parsesOmittedOwnerStartingWithClass = do
  let input = unlines
        [ "$ORIGIN example.com."
        , "@ 200 IN SOA ns.example.com. hostmaster.example.com. 1 2 3 4 5"
        , "www IN NS ns.example.com."
        , "IN AAAA 2001:db8::1"
        ]

  zones <- either (fail . show) fromRecordList <$> parseZoneFile "<test>" input
  case zones of
    [z] -> do
      let recs = allData z
      length recs @?= 3
      name (recs !! 1) @?= name (recs !! 2)
    zs -> assertFailure ("expected 1 zone, got " ++ show (length zs))

parsesExplicitOwnerNamedIN :: Assertion
parsesExplicitOwnerNamedIN = do
  let input = unlines
        [ "$ORIGIN example.com."
        , "@ 200 IN SOA ns.example.com. hostmaster.example.com. 1 2 3 4 5"
        , "IN 300 IN A 192.0.2.10"
        ]

  zones <- either (fail . show) fromRecordList <$> parseZoneFile "<test>" input
  case zones of
    [z] -> length (allData z) @?= 2
    zs -> assertFailure ("expected 1 zone, got " ++ show (length zs))

parsesMultilineRecordWithInlineComment :: Assertion
parsesMultilineRecordWithInlineComment = do
  let input = unlines
        [ ". 1200 IN TXT ("
        , "\"abc\""
        , ") ; comment"
        , ". 300 IN A 192.0.2.10"
        ]

  records <- either (fail . show) pure =<< parseZoneFile "<test>" input
  length records @?= 2

propParsesMultilineDNSKEYBlob :: Property
propParsesMultilineDNSKEYBlob =
  forAll (genSplitBytesRec dnskeyRec) (uncurry parsesRecordType)

propParsesMultilineDSBlob :: Property
propParsesMultilineDSBlob =
  forAll (genSplitBytesRec dsRec) (uncurry parsesRecordType)

parsesRecordType :: DNS.Record.Type -> String -> Property
parsesRecordType expected input =
  counterexample input $
      case runIdentity (runParserT recordTypeData emptyState "<quickcheck>" input) of
        Left err ->
          counterexample (show err) False
        Right actual ->
          actual === expected

genSplitBytesRec :: Gen Type -> Gen (Type, String)
genSplitBytesRec recGen = do
  r <- recGen
  (prefix, blob) <- maybe (error "could not unsnoc") return $ unsnoc $ words $ show r
  serialized <- genSplitBlob blob
  return (r, unwords $ prefix ++ ["(", serialized, ")"])

genSplitBlob :: String -> Gen String
genSplitBlob blob = do
  chunked <- split blob
  initSep <- genSep
  foldM sepF initSep chunked
  where
    sepF :: String -> String -> Gen String
    sepF acc chunk = do
      seperator <- genSep
      return $ acc ++ chunk ++ seperator

split :: String -> Gen [String]
split blob = go blob =<< chooseInt (1, length blob)
  where
    go :: String -> Int -> Gen [String]
    go "" _ = return []
    go s maxLen = do
      i <- chooseInt (1, maxLen)
      let (h, t) = splitAt i s
      (h:) <$> go t maxLen

genSep :: Gen String
genSep = listOf1 (elements [' ', '\t', '\n'])

genBytes :: Gen BS.ByteString
genBytes = BS.pack <$> listOf1 arbitrary

dnskeyRec :: Gen Type
dnskeyRec = DNSKEY [] RSASHA256 <$> genBytes

dsRec :: Gen Type
dsRec = DS 60485 RSASHA256 SHA256 <$> genBytes
