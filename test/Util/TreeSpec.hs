module Util.TreeSpec (tests) where

import qualified Data.Map as M
import qualified Data.Set as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, (@?=), testCase)
import Test.Tasty.QuickCheck

import qualified Util.Tree as T
import Util.Tree (Node(items))

tests :: TestTree
tests =
  testGroup
    "Util.Tree"
    [ testProperty "insert/get roundtrip at path" prop_insertGetRoundtrip
    , testCase "get returns Nothing for missing path" $
        T.get ["a", "b"] (T.emptyNode :: T.Node String Int) @?= Nothing
    , testCase "alter applies function at root for empty path" altersRoot
    , testCase "alter creates missing path when given default node" altersMissingPathWithDefault
    , testCase "alter is no-op on missing path without default" alterMissingPathWithoutDefault
    , testCase "insertNode replaces existing subtree at path" replacesSubtree
    , testProperty "insert is commutative" prop_insertCommutativeDistinctPaths
    , testCase "merge keeps values from both trees" mergesValues
    ]

prop_insertGetRoundtrip :: [String] -> Int -> Property
prop_insertGetRoundtrip p v =
  let n = T.insert p v (T.emptyNode :: T.Node String Int)
  in T.get p n === Just [v]

prop_insertCommutativeDistinctPaths :: [String] -> Int -> [String] -> Int -> Property
prop_insertCommutativeDistinctPaths p1 v1 p2 v2 =
  let left = T.insert p1 v1 (T.insert p2 v2 T.emptyNode)
      right = T.insert p2 v2 (T.insert p1 v1 T.emptyNode)
  in toCanon left === toCanon right

altersRoot :: Assertion
altersRoot = do
  let base = T.Node [1 :: Int] (M.fromList [("x", T.leaf [2 :: Int])])
      out = T.alter (\n@(T.Node is _) -> n { items = 0 : is }) Nothing [] base
  T.get [] out @?= Just [0, 1]
  T.get ["x"] out @?= Just [2]

altersMissingPathWithDefault :: Assertion
altersMissingPathWithDefault = do
  let out = T.alter (\n@(T.Node is _) -> n { items = 1 : is }) (Just T.emptyNode) ["a", "b"] (T.emptyNode :: T.Node String Int)
  T.get ["a"] out @?= Just []
  T.get ["a", "b"] out @?= Just [1]

alterMissingPathWithoutDefault :: Assertion
alterMissingPathWithoutDefault = do
  let base = T.insert ["keep"] (7 :: Int) (T.emptyNode :: T.Node String Int)
      out = T.alter (const (T.leaf [1 :: Int])) Nothing ["missing"] base
  toCanon out @?= toCanon base

data CanonTree k v = CanonTree (S.Set v) [(k, CanonTree k v)]
  deriving (Eq, Show)

toCanon :: Ord v => T.Node k v -> CanonTree k v
toCanon (T.Node is cs) = CanonTree (S.fromList is) [(k, toCanon n) | (k, n) <- M.toAscList cs]

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
