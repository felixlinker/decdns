module Util.FreshSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.QuickCheck
import qualified Data.List as L
import qualified Data.Set as S

import Util.Fresh (avoiding, fresh, runFresh)

tests :: TestTree
tests =
  testGroup
    "Util.Fresh"
    [ testProperty "fresh produces unique names for repeated calls" prop_freshUnique
    , testCase "avoiding picks maximum numeric suffix across names" $
        runFresh (avoiding ["a9", "foo12bar", "z2", "abc"] >> fresh "a") @?= "a10"
    ]

prop_freshUnique :: Positive Int -> Property
prop_freshUnique (Positive n) =
  n <= 50 ==>
    let xs = runFresh (mapM (const (fresh "n")) [1 .. n])
    in S.size (S.fromList xs) === n
