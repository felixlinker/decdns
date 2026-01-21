module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Resolvable.UnificationSpec as UnificationSpec

main :: IO ()
main =
  defaultMain $
    testGroup
      "dns-regressor"
      [UnificationSpec.tests]
