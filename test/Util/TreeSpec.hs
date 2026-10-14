module Util.TreeSpec (tests) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, (@?=), testCase)
import Test.Tasty.QuickCheck

import DNS.Name ((<|))
import DNS.NameSpec (mkName)
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
        T.get ["a", "b"] (T.emptyNode :: T.Node String [Int]) @?= Nothing
    , testCase "insert works with empty path" insertRoot
    , testCase "insert works on empty tree" insertMissingPath
    , testCase "insertNode replaces existing subtree at path" replacesSubtree
    , testProperty "insert is commutative" prop_insertCommutativeDistinctPaths
    , testCase "merge keeps values from both trees" mergesValues
    , testCase "allNamed supplies the name of every node" allNamedSuppliesNames
    , testCase "allNamed rejects a predicate failure below the root" allNamedRejectsFailure
    , rootsTests
    ]

genTree :: Gen (T.Node String [Int])
genTree = go =<< chooseInt (0, 2)
  where
    go 0 = pure T.emptyNode
    go n = do
      n' <- chooseInt (0, n - 1)
      vs <- listOf arbitrary :: Gen [Int]
      width <- chooseInt (0, 3)
      keys <- vectorOf width (arbitrary :: Gen String)
      children <- vectorOf width (go n')
      pure $ T.Node vs (M.fromList (zip keys children))

genEmptyTree :: Gen (T.Node String [Int])
genEmptyTree = T.mapItems (const []) <$> genTree

prop_leafAtGetRoundtrip :: [String] -> Int -> Property
prop_leafAtGetRoundtrip p v =
  let n = T.leafAt p [v]
  in T.get p n === Just [v]

prop_insertGetRoundtrip :: [String] -> Int -> Property
prop_insertGetRoundtrip p v =
  let n = T.insert p [v] (T.emptyNode :: T.Node String [Int])
  in T.get p n === Just [v]

prop_insertCommutativeDistinctPaths :: [String] -> Int -> [String] -> Int -> Property
prop_insertCommutativeDistinctPaths p1 v1 p2 v2 =
  let left = T.insert p1 [v1] (T.insert p2 [v2] T.emptyNode)
      right = T.insert p2 [v2] (T.insert p1 [v1] T.emptyNode)
  in toCanon left === toCanon right

prop_pruneEmptyTreeIsNull :: Property
prop_pruneEmptyTreeIsNull =
  forAll genEmptyTree $ \tree ->
    property $ T.null (T.prune (tree :: T.Node String [Int]))

prop_mergePartionedIdentity :: Property
prop_mergePartionedIdentity =
  forAll genTree $ \tree ->
    toCanon (uncurry T.merge (T.partition (const $ L.partition even) tree)) === toCanon tree

prop_mergeCorrectness :: Property
prop_mergeCorrectness =
  forAll genTree $ \tree ->
    let (top, bot) = T.partition (const $ L.partition even) tree
    in property $ all (all even) top && not (any (any even) bot)

nullDetectsEmptyAndNonEmptyNodes :: Assertion
nullDetectsEmptyAndNonEmptyNodes = do
  T.null (T.emptyNode :: T.Node String [Int]) @?= True
  T.null (T.leaf [1 :: Int]) @?= False

insertRoot :: Assertion
insertRoot = do
  let base = T.Node [1 :: Int] (M.fromList [("x", T.leaf [2 :: Int])])
      out = T.insert [] [0] base
  S.fromList <$> T.get [] out @?= Just (S.fromList [0, 1])
  T.get ["x"] out @?= Just [2]

insertMissingPath :: Assertion
insertMissingPath = do
  let out = T.insert ["a", "b"] [1] (T.emptyNode :: T.Node String [Int])
  T.get ["a"] out @?= Just []
  T.get ["a", "b"] out @?= Just [1]

toCanon :: Ord v => T.Node k [v] -> T.Node k (S.Set v)
toCanon (T.Node is cs) = T.Node (S.fromList is) (M.map toCanon cs)

replacesSubtree :: Assertion
replacesSubtree = do
  let base = T.insert ["x", "y"] [1 :: Int] T.emptyNode
      replacement = T.leaf [2 :: Int]
      out = T.insertNode ["x"] replacement base
  T.get ["x"] out @?= Just [2]
  T.get ["x", "y"] out @?= Nothing

mergesValues :: Assertion
mergesValues = do
  let left = T.insert ["a"] [1 :: Int] T.emptyNode
      right = T.insert ["a"] [2 :: Int] T.emptyNode
      merged = T.merge left right
  S.fromList <$> T.get ["a"] merged @?= Just (S.fromList [1, 2])

allNamedSuppliesNames :: Assertion
allNamedSuppliesNames =
  T.allNamed (==) org tree @?= True
  where
    org = mkName ["example", "com", ""]
    tree = T.Node org (M.fromList
      [ ("www", T.leaf ("www" <| org))
      , ("child", T.Node ("child" <| org) (M.singleton "ns" (T.leaf ("ns" <| ("child" <| org)))))
      ])

allNamedRejectsFailure :: Assertion
allNamedRejectsFailure =
  T.allNamed (==) org tree @?= False
  where
    org = mkName ["example", "com", ""]
    tree = T.Node org (M.singleton "www" (T.leaf org))

trees :: [(T.Node String [Int], S.Set [Int])]
trees =
  [ (T.leaf [], S.empty)
  , (T.leaf [0], S.singleton [0])
  , (T.leafAt ["a", "b"] [0], S.singleton [0])
  , (T.insertNode ["c", "d"] (T.leaf [1]) (T.leafAt ["a", "b"] [0]), S.fromList [[1], [0]])
  , (T.insertNode ["a"] (T.leaf [1]) (T.leaf [0]), S.singleton [0])
  ]

rootsTests :: TestTree
rootsTests = testGroup "nonEmptyRoots tests"
  $ zipWith mkTest [(0 :: Int)..] trees
  where
    mkTest :: Int -> (T.Node String [Int], S.Set [Int]) -> TestTree
    mkTest no (tree, expected) = testCase (show no) $
      let
      in  S.fromList (T.nonEmptyRoots tree) @?= expected
