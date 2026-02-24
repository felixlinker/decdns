module Parsing.ZoneFileSpec (tests) where

import DNS.Name (Named (value))
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
