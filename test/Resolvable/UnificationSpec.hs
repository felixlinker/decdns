module Resolvable.UnificationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck
import qualified Data.Map.Strict as M
import Data.Functor.Identity (Identity(..), runIdentity)

import Resolvable.Prolog.Core
import Resolvable.Unification
import Resolvable.Util (genTerm)
import Util.Fresh (runFresh, Fresh)

tests :: TestTree
tests =
  testGroup
    "Resolvable.Unification"
    [ testCase "away preserves variable sharing" $
        let t = TFunctor "pair" [TVar "X", TVar "X", TVar "Y"]
        in  equivModuloRenaming (runFresh $ awayTerm t) t @?= True
    , testProperty "apply unifier implies terms become equal" prop_unify_apply_agrees
    , testProperty "unification is symmetric in success" prop_unify_symmetric
    , testProperty "unification yields normal unifiers" prop_unify_normal
    , testGroup
        "unit"
        [ testCase "occurs check rejects cyclic binding" occursCheckRejectsCycle
        , testCase "propagates substitutions through bindings" propagatesBindings
        , testCase "list tail unifies against longer list" listTailUnifies
        , testCase "fails on different arity" mismatchedArityFails
        , testCase "equivalence modulo renaming preserves variable sharing" $
            equivModuloRenaming
              (TFunctor "f" [TVar "X", TVar "Y"])
              (TFunctor "f" [TVar "Z", TVar "Z"])
              @?= False
        , testCase "equivalence modulo renaming allows different variable names" $
            equivModuloRenaming
              (TFunctor "f" [TVar "X", TVar "Y"])
              (TFunctor "f" [TVar "A", TVar "B"])
              @?= True
        , testCase "equivalence modulo renaming preserves repeated variables" $
            equivModuloRenaming
              (TFunctor "f" [TVar "X", TVar "X"])
              (TFunctor "f" [TVar "Y", TVar "Y"])
              @?= True
        , testCase "equivalence modulo renaming rejects variables and atoms" $
            equivModuloRenaming
              (TFunctor "f" [TVar "X", TVar "X"])
              (TFunctor "f" [TVar "Y", TAtom "a"])
              @?= False
        ]
    ]

awayTerm :: Term -> Fresh Term
awayTerm = fmap (runIdentity . fst) . away . Identity

occursCheckRejectsCycle :: Assertion
occursCheckRejectsCycle =
  unify (TVar "X") (TFunctor "f" [TVar "X"]) @?= Nothing

propagatesBindings :: Assertion
propagatesBindings =
  case unify (TFunctor "p" [TVar "X", TVar "Y"]) (TFunctor "p" [TVar "Y", TAtom "a"]) of
    Nothing -> assertFailure "expected a unifier"
    Just u -> do
      apply u (TVar "X") @?= TAtom "a"
      apply u (TVar "Y") @?= TAtom "a"

listTailUnifies :: Assertion
listTailUnifies =
  case unify (listWithTail [TVar "X"] (TVar "T")) (list [TAtom "a", TAtom "b"]) of
    Nothing -> assertFailure "expected a unifier"
    Just u -> do
      apply u (TVar "X") @?= TAtom "a"
      apply u (TVar "T") @?= list [TAtom "b"]

mismatchedArityFails :: Assertion
mismatchedArityFails =
  unify (TFunctor "p" [TAtom "a"]) (TFunctor "p" [TAtom "a", TAtom "b"]) @?= Nothing

prop_unify_apply_agrees :: Property
prop_unify_apply_agrees =
  forAll genTerm $ \t1 ->
    forAll genTerm $ \t2 ->
      maybe (property True) (\u -> apply u t1 === apply u t2) (unify t1 t2)

prop_unify_symmetric :: Property
prop_unify_symmetric =
  forAll genTerm $ \t1 ->
    forAll genTerm $ \t2 ->
      unifies t1 t2 === unifies t2 t1

prop_unify_normal :: Property
prop_unify_normal =
  forAll genTerm $ \t1 ->
    forAll genTerm $ \t2 ->
      maybe (property True) (\u -> counterexample (show u) (isNormalUnifier u)) (unify t1 t2)

isNormalUnifier :: Unifier -> Bool
isNormalUnifier (Unifier m) = all noVarInOtherTerms (M.keys m) && all isVar (M.keys m)
  where
    -- Substituted term does not occur in image of unifier
    noVarInOtherTerms :: Term -> Bool
    noVarInOtherTerms v = not $ any (subterm v) (M.elems m)
