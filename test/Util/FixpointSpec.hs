module Util.FixpointSpec (tests) where

import qualified Data.Set as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Util.Fixpoint

tests :: TestTree
tests =
  testGroup
    "Util.Fixpoint"
    [ testCase "terminates after adding an element" $
        fixpointSet (const [1 :: Int]) S.empty @?= S.singleton 1
    , testCase "repeats until no new elements are added" $
        fixpointSet successors (S.singleton 0) @?= S.fromList [0 .. 3]
    ]
  where
    successors values = [value + 1 | value <- S.toList values, value < (3 :: Int)]
