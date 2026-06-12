module Util.PartialOrderSpec (tests) where

import qualified Data.Set as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import qualified Util.PartialOrder as PO

tests :: TestTree
tests =
  testGroup
    "Util.PartialOrder"
    [ testCase "toList exposes all relations" $
        PO.toList (relations [('b', "a"), ('a', "")])
          @?= [('a', S.empty), ('b', S.singleton 'a')]
    , testCase "toAscList orders relations by their left element" $
        PO.toAscList (relations [('c', "a"), ('a', ""), ('b', "a")])
          @?= [('a', S.empty), ('b', S.singleton 'a'), ('c', S.singleton 'a')]
    , testCase "fromList combines pairs with the same left element" $
        PO.toAscList (PO.fromList [('b', 'a'), ('b', 'c'), ('d', 'a')])
          @?= [('b', S.fromList "ac"), ('d', S.singleton 'a')]
    , testCase "rel returns known relations and defaults to empty" $ do
        let order = relations [('b', "ac")]
        PO.rel 'b' order @?= S.fromList "ac"
        PO.rel 'x' order @?= S.empty
    , testCase "minima returns present elements with no smaller elements" $
        PO.minima (relations [('c', "b"), ('a', ""), ('b', "a")]) @?= "a"
    , testCase "transitiveClosure adds all transitively smaller elements" $
        fmap PO.toAscList (PO.transitiveClosure acyclicOrder)
          @?= Just
            [ ('a', S.empty)
            , ('b', S.fromList "a")
            , ('c', S.fromList "ab")
            , ('d', S.fromList "abcx")
            ]
    , testCase "transitiveClosure rejects a self-cycle" $
        fmap PO.toAscList (PO.transitiveClosure (relations [('a', "a")])) @?= Nothing
    , testCase "transitiveClosure rejects a multi-element cycle" $
        fmap PO.toAscList (PO.transitiveClosure (relations [('a', "b"), ('b', "c"), ('c', "a")]))
          @?= Nothing
    , testCase "invert reverses every relation" $
        PO.toAscList (PO.invert (relations [('a', ""), ('b', "a"), ('c', "ab")]))
          @?= [('a', S.fromList "bc"), ('b', S.singleton 'c')]
    , testCase "insert unions new and existing relations" $
        PO.toAscList (PO.insert 'b' (S.fromList "ac") (relations [('b', "ad")]))
          @?= [('b', S.fromList "acd")]
    , testCase "remove deletes an element and all references to it" $
        PO.toAscList (PO.remove 'b' (relations [('a', ""), ('b', "a"), ('c', "ab")]))
          @?= [('a', S.empty), ('c', S.singleton 'a')]
    , testCase "filterLefts filters only left elements" $
        PO.toAscList (PO.filterLefts (/= 'b') (relations [('a', ""), ('b', "a"), ('c', "ab")]))
          @?= [('a', S.empty), ('c', S.fromList "ab")]
    , testCase "filterRights filters only right elements" $
        PO.toAscList (PO.filterRights (/= 'b') (relations [('a', ""), ('b', "a"), ('c', "ab")]))
          @?= [('a', S.empty), ('b', S.singleton 'a'), ('c', S.singleton 'a')]
    ]

acyclicOrder :: PO.PartialOrder Char
acyclicOrder = relations [('a', ""), ('b', "a"), ('c', "b"), ('d', "bcx")]

relations :: Ord a => [(a, [a])] -> PO.PartialOrder a
relations = foldr (uncurry PO.insert . fmap S.fromList) PO.empty
