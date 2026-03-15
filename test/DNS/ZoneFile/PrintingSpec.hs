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
import DNS.ZoneFile (Zone, allData, zoneName)
import Parsing.ZoneFile (parseZoneFile)

tests :: TestTree
tests =
  testGroup
    "ZoneFile.Printing"
    [ testProperty "parsing a printed zone yields the same zone" propPrintParseRoundTrip
    ]

propPrintParseRoundTrip :: ZS.ArbZone -> Property
propPrintParseRoundTrip (ZS.ArbZone z printed) =
  let parsed = runIdentity (parseZoneFile "<quickcheck>" printed)
  in case parsed of
      Left err ->
        counterexample (show err) False
      Right [z'] ->
        normalizeZone z' === normalizeZone z
      Right zs ->
        counterexample ("expected 1 zone, got " ++ show (length zs)) False

normalizeZone :: Zone String -> (Name String, [StandardRecord])
normalizeZone z = (zoneName z, sort (allData z))
