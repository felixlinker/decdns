module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Resolvable.UnificationSpec as UnificationSpec
import qualified Resolvable.ConstrainedUnificationSpec as ConstrainedUnificationSpec

main :: IO ()
main =
  defaultMain $
    testGroup
      "dns-regressor"
      [UnificationSpec.tests, ConstrainedUnificationSpec.tests]
