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
        PO.toList (relations [("", 'a'), ("a", 'b')])
          @?= [(S.empty, 'a'), (S.singleton 'a', 'b')]
    , testCase "toAscList orders relations by their right element" $
        PO.toAscList (relations [("", 'a'), ("a", 'c'), ("a", 'b')])
          @?= [(S.empty, 'a'), (S.singleton 'a', 'b'), (S.singleton 'a', 'c')]
    , testCase "fromList combines pairs with the same right element" $
        PO.toAscList (PO.fromList [('a', 'b'), ('c', 'b'), ('a', 'd')])
          @?= [(S.fromList "ac", 'b'), (S.singleton 'a', 'd')]
    , testCase "rel returns known relations and defaults to empty" $ do
        let order = relations [("ac", 'b')]
        PO.rel 'b' order @?= S.fromList "ac"
        PO.rel 'x' order @?= S.empty
    , testCase "minima returns present elements with no smaller elements" $
        PO.minima (relations [("b", 'c'), ("", 'a'), ("a", 'b')]) @?= S.fromList "a"
    , testCase "transitiveClosure adds all transitively smaller elements" $
        fmap PO.toAscList (PO.transitiveClosure acyclicOrder)
          @?= Just
            [ (S.empty, 'a')
            , (S.fromList "a", 'b')
            , (S.fromList "ab", 'c')
            , (S.fromList "abcx", 'd')
            ]
    , testCase "transitiveClosure rejects a self-cycle" $
        fmap PO.toAscList (PO.transitiveClosure (relations [("a", 'a')])) @?= Nothing
    , testCase "transitiveClosure rejects a multi-element cycle" $
        fmap PO.toAscList (PO.transitiveClosure (relations [("b", 'a'), ("c", 'b'), ("a", 'c')]))
          @?= Nothing
    , testCase "invert reverses every relation" $
        PO.toAscList (PO.invert (relations [("", 'a'), ("a", 'b'), ("ab", 'c')]))
          @?= [(S.fromList "bc", 'a'), (S.singleton 'c', 'b')]
    , testCase "insert unions new and existing relations" $
        PO.toAscList (PO.insert (S.fromList "ac") 'b' (relations [("ad", 'b')]))
          @?= [(S.fromList "acd", 'b')]
    , testCase "remove deletes an element and all references to it" $
        PO.toAscList (PO.remove 'b' (relations [("", 'a'), ("a", 'b'), ("ab", 'c')]))
          @?= [(S.empty, 'a'), (S.singleton 'a', 'c')]
    , testCase "filterLefts filters only left elements" $
        PO.toAscList (PO.filterLefts (/= 'b') (relations [("", 'a'), ("a", 'b'), ("ab", 'c')]))
          @?= [(S.empty, 'a'), (S.singleton 'a', 'b'), (S.singleton 'a', 'c')]
    , testCase "filterRights filters only right elements" $
        PO.toAscList (PO.filterRights (/= 'b') (relations [("", 'a'), ("a", 'b'), ("ab", 'c')]))
          @?= [(S.empty, 'a'), (S.fromList "ab", 'c')]
    ]

acyclicOrder :: PO.PartialOrder Char
acyclicOrder = relations [("", 'a'), ("a", 'b'), ("b", 'c'), ("bcx", 'd')]

relations :: Ord a => [([a], a)] -> PO.PartialOrder a
relations = foldr (\(smaller, larger) -> PO.insert (S.fromList smaller) larger) PO.empty
