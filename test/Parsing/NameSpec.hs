module Parsing.NameSpec (tests) where

import DNS.Name (Name)
import DNS.NameSpec (mkName)
import Parsing.Name (parseName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, (@?=), assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Parsing.Name"
    [ testCase "parses an undotted standalone name as absolute" parsesUndottedAbsoluteName
    , testCase "parses an IPv4 server identifier as an absolute name" parsesIPv4ServerIdentifier
    , testCase "parses root name" parsesUndottedAbsoluteName
    , testCase "accepts a name at the 255-octet limit" acceptsMaximumLengthName
    , testCase "rejects a name exceeding the 255-octet limit" rejectsOverlongName
    , testCase "rejects a label exceeding the 63-octet limit" rejectsOverlongLabel
    ]

parsesUndottedAbsoluteName :: Assertion
parsesUndottedAbsoluteName =
  assertParsed "example.com" (mkName ["example", "com", ""])

parsesIPv4ServerIdentifier :: Assertion
parsesIPv4ServerIdentifier =
  assertParsed "1.1.1.1" (mkName ["1", "1", "1", "1", ""])

acceptsMaximumLengthName :: Assertion
acceptsMaximumLengthName =
  let maxN = joinLabels [63, 63, 63, 61]
  in  assertParsed (show maxN) maxN

rejectsOverlongName :: Assertion
rejectsOverlongName =
  assertRejected (show $ joinLabels [63, 63, 63, 62])

rejectsOverlongLabel :: Assertion
rejectsOverlongLabel =
  assertRejected (show $ joinLabels [64])

joinLabels :: [Int] -> Name String
joinLabels = mkName . (++ [""]) . map (`replicate` 'a')

assertParsed :: String -> Name String -> Assertion
assertParsed input expected = do
  result <- parseName input
  either (assertFailure . show) (@?= expected) result

assertRejected :: String -> Assertion
assertRejected input = do
  result <- parseName input
  either (const $ return ()) (assertFailure . ("expected parse failure, got " ++) . show) result
