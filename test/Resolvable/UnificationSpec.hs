module Resolvable.UnificationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Resolvable.Prolog (Term(..))
import Resolvable.Unification (apply, unify)
import Util.Fresh (runFresh, fresh)

tests :: TestTree
tests =
  testGroup
    "Resolvable.Unification"
    [ testProperty "apply unifier implies terms become equal" prop_unify_apply_agrees ]

prop_unify_apply_agrees :: Property
prop_unify_apply_agrees =
  forAll (genTermNoWildcard names) $ \t1 ->
    forAll (genTermNoWildcard names) $ \t2 ->
      maybe (property True) (\u -> apply u t1 === apply u t2) (unify t1 t2)
  where
    names :: Gen String
    names = elements (runFresh $ mapM (const $ fresh "v") ([0..4] :: [Int]))

genTermNoWildcard :: Gen String -> Gen Term
genTermNoWildcard names = do
  n <- chooseInt (0, 6)
  go n
  where
    go :: Int -> Gen Term
    go n
      | n <= 0 = oneof  [ TVar <$> names
                        , TAtom <$> names
                        , TAtomQuoted <$> names
                        , TString <$> names
                        ]
      | otherwise =
        let rec = go (n - 1)
        in  frequency
            [ (4, go 0)
            , (2, TOp <$> names <*> genList rec)
            , (2, TPred <$> names <*> genList rec)
            , (2, TList <$> genList rec <*> oneof [pure Nothing, Just <$> rec]) ]

    genList :: Gen Term -> Gen [Term]
    genList g = do
      k <- chooseInt (0, 3)
      vectorOf k g
