module DNS.ZoneFile.PrintingSpec
  ( tests
  ) where

import Data.Functor.Identity (runIdentity)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import qualified DNS.ZoneFileSpec as ZS
import DNS.ZoneFile (fromRecordList)
import Parsing.ZoneFile (parseZoneFile)

tests :: TestTree
tests =
  testGroup
    "ZoneFile.Printing"
    [ testProperty "parsing a printed zone yields the same zone" propPrintParseRoundTrip ]

propPrintParseRoundTrip :: ZS.ArbZone -> Property
propPrintParseRoundTrip (ZS.ArbZone z printed) =
  let parsed = fromRecordList <$> runIdentity (parseZoneFile "<quickcheck>" printed)
  in case parsed of
      Left err ->
        counterexample (show err) False
      Right [z'] ->
        ZS.normalizeZone z' === ZS.normalizeZone z
      Right zs ->
        counterexample ("expected 1 zone, got " ++ show (length zs)) False
