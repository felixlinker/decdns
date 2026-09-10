module NameConstraintSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import NameConstraint (LabelConstraint(..))

tests :: TestTree
tests =
  testGroup
    "NameConstraint"
    [ testCase "wildcard equality ignores constraints" $
        Wildcard ["a"] == Wildcard ["b"] @?= True
    , testCase "wildcard ordering ignores constraints" $
        compare (Wildcard ["a"]) (Wildcard ["b"]) @?= EQ
    ]
