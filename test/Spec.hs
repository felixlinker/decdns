module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Resolvable.UnificationSpec as UnificationSpec
import qualified Resolvable.ConstrainedUnificationSpec as ConstrainedUnificationSpec
import qualified Resolvable.DependenciesSpec as DependenciesSpec
import qualified Resolvable.Update.RolloutSpec as RolloutSpec
import qualified DNS.NameSpec as NameSpec
import qualified DNS.RecordSpec as RecordSpec
import qualified DNS.SEC.KeysSpec as KeysSpec
import qualified DNS.DiffSpec as DiffSpec
import qualified DNS.ZoneFileSpec as DNSZoneFileSpec
import qualified DNS.ZoneFile.PrintingSpec as ZoneFilePrintingSpec
import qualified Parsing.WellformednessSpec as ParsingWellformednessSpec
import qualified Parsing.ZoneFileSpec as ParsingZoneFileSpec
import qualified Util.ListSpec as ListSpec
import qualified Util.PartialOrderSpec as PartialOrderSpec
import qualified Util.TreeSpec as TreeSpec
import qualified Util.FreshSpec as FreshSpec

main :: IO ()
main =
  defaultMain $
    testGroup
      "dns-regressor"
      [ UnificationSpec.tests
      , ConstrainedUnificationSpec.tests
      , DependenciesSpec.tests
      , RolloutSpec.tests
      , NameSpec.tests
      , RecordSpec.tests
      , KeysSpec.tests
      , DiffSpec.tests
      , DNSZoneFileSpec.tests
      , ParsingWellformednessSpec.tests
      , ParsingZoneFileSpec.tests
      , ZoneFilePrintingSpec.tests
      , ListSpec.tests
      , PartialOrderSpec.tests
      , TreeSpec.tests
      , FreshSpec.tests
      ]
