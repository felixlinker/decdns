module Parsing.ZoneFileSpec (tests) where

import DNS.Name (Named (name, value))
import DNS.Record (RData (rttl))
import DNS.ZoneFile (allData)
import Parsing.ZoneFile (parseZoneFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Parsing.ZoneFile"
    [ testCase "parses $TTL and applies it to records without explicit TTL" parsesTTLDirective
    , testCase "parses omitted owner when line starts with IN" parsesOmittedOwnerStartingWithClass
    , testCase "backtracks to explicit owner when owner is IN" parsesExplicitOwnerNamedIN
    ]

parsesTTLDirective :: Assertion
parsesTTLDirective = do
  let input = unlines
        [ "$ORIGIN example.com."
        , "$TTL 3600"
        , "@ IN A 192.0.2.1"
        , "www IN A 192.0.2.2"
        ]

  zones <-  either (fail . show) id <$> parseZoneFile "<test>" input
  case zones of
    [z] -> do
      let d = allData z
      length d @?= 2
      mapM_ ((3600 @=?) . rttl . value) d
    zs -> assertFailure ("expected 1 zone, got " ++ show (length zs))

parsesOmittedOwnerStartingWithClass :: Assertion
parsesOmittedOwnerStartingWithClass = do
  let input = unlines
        [ "$ORIGIN example.com."
        , "@ 200 IN SOA ns.example.com. hostmaster.example.com. 1 2 3 4 5"
        , "www IN NS ns.example.com."
        , "IN AAAA 2001:db8::1"
        ]

  zones <- either (fail . show) id <$> parseZoneFile "<test>" input
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

  zones <- either (fail . show) id <$> parseZoneFile "<test>" input
  case zones of
    [z] -> length (allData z) @?= 2
    zs -> assertFailure ("expected 1 zone, got " ++ show (length zs))
