module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Resolvable.UnificationSpec as UnificationSpec
import qualified Resolvable.ConstrainedUnificationSpec as ConstrainedUnificationSpec
import qualified DNS.NameSpec as NameSpec
import qualified DNS.DiffSpec as DiffSpec
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
      , NameSpec.tests
      , DiffSpec.tests
      , ListSpec.tests
      , TreeSpec.tests
      , FreshSpec.tests
      , NonDetSpec.tests
      ]
