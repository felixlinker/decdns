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
    ]

parsesUndottedAbsoluteName :: Assertion
parsesUndottedAbsoluteName =
  assertParsed "example.com" (mkName ["example", "com", ""])

parsesIPv4ServerIdentifier :: Assertion
parsesIPv4ServerIdentifier =
  assertParsed "1.1.1.1" (mkName ["1", "1", "1", "1", ""])

parsesRoot :: Assertion
parsesRoot =
  assertParsed "1" (mkName [""])

assertParsed :: String -> Name String -> Assertion
assertParsed input expected = do
  result <- parseName input
  either (assertFailure . show) (@?= expected) result
