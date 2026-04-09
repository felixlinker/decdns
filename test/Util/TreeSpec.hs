{-# LANGUAGE ViewPatterns #-}
module Util.TreeSpec (tests) where

import qualified Data.Map as M
import qualified Data.Set as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, (@?=), testCase)
import Test.Tasty.QuickCheck

import qualified Util.Tree as T

tests :: TestTree
tests =
  testGroup
    "Util.Tree"
    [ testProperty "leafAt/get roundtrip at path" prop_leafAtGetRoundtrip
    , testProperty "insert/get roundtrip at path" prop_insertGetRoundtrip
    , testProperty "pruning a tree without elements yields null" prop_pruneEmptyTreeIsNull
    , testProperty "merging a partition is the identity" prop_mergePartionedIdentity
    , testProperty "partition correctly partitions" prop_mergeCorrectness
    , testCase "null detects empty and non-empty nodes" nullDetectsEmptyAndNonEmptyNodes
    , testCase "get returns Nothing for missing path" $
        T.get ["a", "b"] (T.emptyNode :: T.Node String Int) @?= Nothing
    , testCase "alter applies function at root for empty path" insertRoot
    , testCase "alter creates missing path when given default node" insertMissingPath
    , testCase "insertNode replaces existing subtree at path" replacesSubtree
    , testProperty "insert is commutative" prop_insertCommutativeDistinctPaths
    , testCase "merge keeps values from both trees" mergesValues
    ]

newtype TNode k v = TNode { tt :: T.Node k v }

onTree :: (T.Node k1 v1 -> T.Node k2 v2) -> TNode k1 v1 -> TNode k2 v2
onTree f (tt -> t) = TNode (f t)

instance (Ord v, Eq k, Eq v) => Eq (TNode k v) where
  (tt -> T.Node vs (M.map TNode -> cs)) == (tt -> T.Node vs' (M.map TNode -> cs')) =
    (S.fromList vs == S.fromList vs') && (cs == cs')

instance (Show k, Show v) => Show (TNode k v) where
  show = show . tt

genTree :: Gen (TNode String Int)
genTree = (fmap TNode . go) =<< chooseInt (0, 2)
  where
    go 0 = pure T.emptyNode
    go n = do
      n' <- chooseInt (0, n - 1)
      vs <- listOf arbitrary :: Gen [Int]
      width <- chooseInt (0, 3)
      keys <- vectorOf width (arbitrary :: Gen String)
      children <- vectorOf width (go n')
      pure $ T.Node vs (M.fromList (zip keys children))

genEmptyTree :: Gen (TNode String Int)
genEmptyTree = onTree (T.mapItems (const [])) <$> genTree

prop_leafAtGetRoundtrip :: [String] -> Int -> Property
prop_leafAtGetRoundtrip p v =
  let n = T.leafAt p [v]
  in T.get p n === Just [v]

prop_insertGetRoundtrip :: [String] -> Int -> Property
prop_insertGetRoundtrip p v =
  let n = T.insert p v (T.emptyNode :: T.Node String Int)
  in T.get p n === Just [v]

prop_insertCommutativeDistinctPaths :: [String] -> Int -> [String] -> Int -> Property
prop_insertCommutativeDistinctPaths p1 v1 p2 v2 =
  let left = T.insert p1 v1 (T.insert p2 v2 T.emptyNode)
      right = T.insert p2 v2 (T.insert p1 v1 T.emptyNode)
  in toCanon left === toCanon right

prop_pruneEmptyTreeIsNull :: Property
prop_pruneEmptyTreeIsNull =
  forAll genEmptyTree $ \tree ->
    property $ T.null (T.prune (tt (tree :: TNode String Int)))

prop_mergePartionedIdentity :: Property
prop_mergePartionedIdentity =
  forAll genTree $ \tree ->
    onTree (uncurry T.merge . T.partition (const even)) tree === tree

prop_mergeCorrectness :: Property
prop_mergeCorrectness =
  forAll genTree $ \tree ->
    let (top, bot) = T.partition (const even) (tt tree)
    in property $ all even top && not (any even bot)

nullDetectsEmptyAndNonEmptyNodes :: Assertion
nullDetectsEmptyAndNonEmptyNodes = do
  T.null (T.emptyNode :: T.Node String Int) @?= True
  T.null (T.leaf [1 :: Int]) @?= False

insertRoot :: Assertion
insertRoot = do
  let base = T.Node [1 :: Int] (M.fromList [("x", T.leaf [2 :: Int])])
      out = T.insert [] 0 base
  T.get [] out @?= Just [0, 1]
  T.get ["x"] out @?= Just [2]

insertMissingPath :: Assertion
insertMissingPath = do
  let out = T.insert ["a", "b"] 1 (T.emptyNode :: T.Node String Int)
  T.get ["a"] out @?= Just []
  T.get ["a", "b"] out @?= Just [1]

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
