module Util.GraphSpec (tests) where

import qualified Data.Set as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import qualified Util.Graph as G

tests :: TestTree
tests =
  testGroup
    "Util.PartialOrder"
    [ testCase "toList exposes all relations" $
        S.fromList (G.toList (relations [('b', "a")]))
          @?= S.fromList [('a', S.empty), ('b', S.singleton 'a')]
    , testCase "toAscList orders relations by their right element" $
        G.toAscList (relations [('a', ""), ('c', "a"), ('b', "a")])
          @?= [('a', S.empty), ('b', S.singleton 'a'), ('c', S.singleton 'a')]
    , testCase "fromList unit test" $
        S.fromList (G.toList (G.fromList [('a', 'b'), ('c', 'b'), ('a', 'd')]))
          @?= S.fromList [('a', S.fromList "bd"), ('b', S.empty), ('c', S.singleton 'b'), ('d', S.empty)]
    , testCase "next returns only known relations" $ do
        let order = relations [('b', "ac")]
        G.next order 'b' @?= S.fromList "ac"
        G.next order 'x' @?= S.empty
    , testCase "leaves returns present elements with no incoming edges" $
        G.leaves (relations [('c', "b"), ('a', ""), ('b', "a")]) @?= S.fromList "a"
    , testCase "transitiveClosure adds all correct elements" $
        fmap (S.fromList . G.toList) (G.transitiveClosure acyclicOrder)
          @?= Just (S.fromList
            [ ('a', S.empty)
            , ('b', S.fromList "a")
            , ('c', S.fromList "ab")
            , ('d', S.fromList "abcx")
            , ('x', S.empty)
            ])
    , testCase "transitiveClosure rejects a self-cycle" $
        fmap G.toList (G.transitiveClosure (relations [('a', "a")])) @?= Nothing
    , testCase "transitiveClosure rejects a multi-element cycle" $
        fmap G.toList (G.transitiveClosure (relations [('a', "b"), ('b', "c"), ('c', "a")]))
          @?= Nothing
    , testCase "invert reverses every relation" $
        S.fromList (G.toList (G.invert (relations [('a', ""), ('b', "a"), ('c', "ab")])))
          @?= S.fromList [('a', S.fromList "bc"), ('b', S.singleton 'c'), ('c', S.empty)]
    , testCase "insert unions new and existing relations" $
        S.fromList (G.toList (G.insert 'b' (S.fromList "ac") (relations [('b', "ad")])))
          @?= S.fromList [('a', S.empty), ('b', S.fromList "acd"), ('c', S.empty), ('d', S.empty)]
    , testCase "remove deletes an element and all references to it" $
        S.fromList (G.toList (G.remove 'b' (relations [('a', ""), ('b', "a"), ('c', "ab")])))
          @?= S.fromList [('a', S.empty), ('c', S.singleton 'a')]
    , testCase "filter" $
        (S.fromList . G.toList . G.filter (/= 'b') <$> G.transitiveClosure (relations [('a', ""), ('b', "a"), ('c', "ab")]))
          @?= Just (S.fromList [('a', S.empty), ('c', S.singleton 'a')])
    , testCase "triangle SCC" (G.tarjan triangle @?= [G.elems triangle])
    , testCase "twoTriangle SCC" (G.tarjan twoTriangle @?= [S.fromList "abc", S.fromList "def"])
    , testCase "acyclic order SCCs" (G.tarjan acyclicOrder @?= map S.singleton (S.toList $ G.elems acyclicOrder))
    ]

triangle :: G.Graph Char
triangle = relations [('b', "a"), ('c', "b"), ('a', "c")]

twoTriangle :: G.Graph Char
twoTriangle = relations [('b', "a"), ('c', "b"), ('a', "c"), ('e', "d"), ('f', "e"), ('d', "fa")]

acyclicOrder :: G.Graph Char
acyclicOrder = relations [('a', ""), ('b', "a"), ('c', "b"), ('d', "bcx")]

relations :: Ord a => [(a, [a])] -> G.Graph a
relations = foldr (\(froms, tos) -> G.insert froms (S.fromList tos)) G.empty
