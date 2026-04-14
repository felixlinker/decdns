module Resolvable.UnificationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck
import qualified Data.Map.Strict as M

import Resolvable.Prolog
import Resolvable.Unification (Unifier(..), apply, unify, unifies)
import Resolvable.Util (genTermNoWildcard)

tests :: TestTree
tests =
  testGroup
    "Resolvable.Unification"
    [ testProperty "apply unifier implies terms become equal" prop_unify_apply_agrees
    , testProperty "unification is symmetric in success" prop_unify_symmetric
    , testProperty "unification yields normal unifiers" prop_unify_normal
    , testGroup
        "unit"
        [ testCase "occurs check rejects cyclic binding" occursCheckRejectsCycle
        , testCase "propagates substitutions through bindings" propagatesBindings
        , testCase "list tail unifies against longer list" listTailUnifies
        , testCase "fails on different arity" mismatchedArityFails
        , testCase "wildcard unifies without binding" wildcardKeepsIdentity
        , testCase "wildcard unification is symmetric" wildcardUnificationSymmetric
        ]
    ]

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

wildcardKeepsIdentity :: Assertion
wildcardKeepsIdentity =
  case unify TWildcard (TAtom "a") of
    Nothing -> assertFailure "expected a unifier"
    Just u -> apply u (TVar "X") @?= TVar "X"

wildcardUnificationSymmetric :: Assertion
wildcardUnificationSymmetric = do
  let t = TFunctor "p" [TVar "X", TAtom "a"]
  let assertIdentity u = apply u (TVar "X")
  case unify TWildcard t of
    Nothing -> assertFailure "expected wildcard on lhs to unify"
    Just u -> assertIdentity u @?= TVar "X"
  case unify t TWildcard of
    Nothing -> assertFailure "expected wildcard on rhs to unify"
    Just u -> assertIdentity u @?= TVar "X"

prop_unify_apply_agrees :: Property
prop_unify_apply_agrees =
  forAll genTermNoWildcard $ \t1 ->
    forAll genTermNoWildcard $ \t2 ->
      maybe (property True) (\u -> apply u t1 === apply u t2) (unify t1 t2)

prop_unify_symmetric :: Property
prop_unify_symmetric =
  forAll genTermNoWildcard $ \t1 ->
    forAll genTermNoWildcard $ \t2 ->
      unifies t1 t2 === unifies t2 t1

prop_unify_normal :: Property
prop_unify_normal =
  forAll genTermNoWildcard $ \t1 ->
    forAll genTermNoWildcard $ \t2 ->
      maybe (property True) (\u -> counterexample (show u) (isNormalUnifier u)) (unify t1 t2)

isNormalUnifier :: Unifier -> Bool
isNormalUnifier (Unifier m) = all noVarInOtherTerms (M.keys m)
  where
    noVarInOtherTerms :: Term -> Bool
    noVarInOtherTerms v = not $ any (subterm v) (M.elems m)
