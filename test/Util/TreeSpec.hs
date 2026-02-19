module Util.TreeSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, (@?=), testCase)
import Test.Tasty.QuickCheck

import qualified Util.Tree as T

tests :: TestTree
tests =
  testGroup
    "Util.Tree"
    [ testProperty "insert/get roundtrip at path" prop_insertGetRoundtrip
    , testCase "get returns Nothing for missing path" $
        T.get ["a", "b"] (T.emptyNode :: T.Node String Int) @?= Nothing
    , testCase "insertNode replaces existing subtree at path" replacesSubtree
    , testCase "merge keeps values from both trees" mergesValues
    ]

prop_insertGetRoundtrip :: [String] -> Int -> Property
prop_insertGetRoundtrip p v =
  let n = T.insert p v (T.emptyNode :: T.Node String Int)
  in T.get p n === Just [v]

replacesSubtree :: Assertion
replacesSubtree = do
  let base = T.insert ["x", "y"] (1 :: Int) T.emptyNode
      replacement = T.leaf [2 :: Int]
      out = T.insertNode ["x"] replacement base
  T.get ["x"] out @?= Just [2]
  T.get ["x", "y"] out @?= Nothing

mergesValues :: Assertion
mergesValues = do
  let left = T.insert ["a"] (1 :: Int) T.emptyNode
      right = T.insert ["a"] (2 :: Int) T.emptyNode
      merged = T.merge left right
  T.get ["a"] merged @?= Just [1, 2]
