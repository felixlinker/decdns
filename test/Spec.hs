module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Resolvable.UnificationSpec as UnificationSpec
import qualified Resolvable.Prolog.CoreSpec as PrologCoreSpec
import qualified Resolvable.ConstrainedUnificationSpec as ConstrainedUnificationSpec
import qualified Resolvable.ResolvabilitySpec as ResolvabilitySpec
import qualified Resolvable.Update.DisunificationSpec as DisunificationSpec
import qualified Resolvable.Update.RolloutSpec as RolloutSpec
import qualified NameConstraintSpec
import qualified DNS.NameSpec as NameSpec
import qualified DNS.RecordSpec as RecordSpec
import qualified DNS.SEC.VerificationSpec as VerificationSpec
import qualified DNS.DiffSpec as DiffSpec
import qualified DNS.ZoneFileSpec as DNSZoneFileSpec
import qualified DNS.ZoneFile.PrintingSpec as ZoneFilePrintingSpec
import qualified Integration
import qualified DNS.WellformednessSpec as WellformednessSpec
import qualified Parsing.NameSpec as ParsingNameSpec
import qualified Parsing.ZoneFileSpec as ParsingZoneFileSpec
import qualified Util.ListSpec as ListSpec
import qualified Util.BoolSpec as BoolSpec
import qualified Util.GraphSpec as GraphSpec
import qualified Util.TreeSpec as TreeSpec
import qualified Util.FreshSpec as FreshSpec
import qualified Util.FixpointSpec as FixpointSpec

main :: IO ()
main = do
  -- rolloutTests <- RolloutSpec.tests
  defaultMain $
    testGroup
      "dns-regressor"
      [ UnificationSpec.tests
      , PrologCoreSpec.tests
      , ConstrainedUnificationSpec.tests
      , ResolvabilitySpec.tests
      , DisunificationSpec.tests
      -- , rolloutTests
      , NameConstraintSpec.tests
      , NameSpec.tests
      , RecordSpec.tests
      , VerificationSpec.tests
      , DiffSpec.tests
      , DNSZoneFileSpec.tests
      , Integration.tests
      , WellformednessSpec.tests
      , ParsingNameSpec.tests
      , ParsingZoneFileSpec.tests
      , ZoneFilePrintingSpec.tests
      , BoolSpec.tests
      , ListSpec.tests
      , GraphSpec.tests
      , TreeSpec.tests
      , FreshSpec.tests
      , FixpointSpec.tests
      ]
