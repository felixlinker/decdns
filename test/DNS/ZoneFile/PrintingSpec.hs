module DNS.ZoneFile.PrintingSpec
  ( tests
  ) where

import Data.Functor.Identity (runIdentity)
import Data.List (sort)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import DNS.Name (Name)
import DNS.Record (StandardRecord)
import qualified DNS.ZoneFileSpec as ZS
import DNS.ZoneFile (Zone, allData, zoneName, fromRecordList)
import Parsing.ZoneFile (parseZoneFile)

tests :: TestTree
tests =
  testGroup
    "ZoneFile.Printing"
    [ testProperty "parsing a printed zone yields the same zone" propPrintParseRoundTrip
    , testProperty "parsing a printed zone with DNSSEC records yields the same zone" propPrintParseRoundTripDNSSEC
    ]

propPrintParseRoundTrip :: ZS.ArbZone -> Property
propPrintParseRoundTrip (ZS.ArbZone z printed) =
  let parsed = fromRecordList <$> runIdentity (parseZoneFile "<quickcheck>" printed)
  in case parsed of
      Left err ->
        counterexample (show err) False
      Right [z'] ->
        normalizeZone z' === normalizeZone z
      Right zs ->
        counterexample ("expected 1 zone, got " ++ show (length zs)) False

propPrintParseRoundTripDNSSEC :: Property
propPrintParseRoundTripDNSSEC =
  forAll ZS.genArbZoneWithDNSSEC propPrintParseRoundTrip

normalizeZone :: Zone String -> (Name String, [StandardRecord])
normalizeZone z = (zoneName z, sort (allData z))
