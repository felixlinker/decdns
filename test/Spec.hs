module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Resolvable.UnificationSpec as UnificationSpec
import qualified Resolvable.ConstrainedUnificationSpec as ConstrainedUnificationSpec
import qualified Resolvable.DependenciesSpec as DependenciesSpec
import qualified Resolvable.Update.RolloutSpec as RolloutSpec
import qualified DNS.NameSpec as NameSpec
import qualified DNS.DiffSpec as DiffSpec
import qualified DNS.ZoneFile.PrintingSpec as ZoneFilePrintingSpec
import qualified Parsing.ZoneFileSpec as ZoneFileSpec
import qualified Util.ListSpec as ListSpec
import qualified Util.TreeSpec as TreeSpec
import qualified Util.FreshSpec as FreshSpec
import qualified NonDetSpec

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
      , DiffSpec.tests
      , ZoneFileSpec.tests
      , ZoneFilePrintingSpec.tests
      , ListSpec.tests
      , TreeSpec.tests
      , FreshSpec.tests
      , NonDetSpec.tests
      ]
