module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Resolvable.UnificationSpec as UnificationSpec
import qualified Resolvable.ConstrainedUnificationSpec as ConstrainedUnificationSpec
import qualified ZoneFileSpec

main :: IO ()
main =
  defaultMain $
    testGroup
      "dns-regressor"
      [UnificationSpec.tests, ConstrainedUnificationSpec.tests, ZoneFileSpec.tests]
