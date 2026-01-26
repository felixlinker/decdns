module Resolvable.UnificationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck

import Resolvable.Prolog (Term(..))
import Resolvable.Unification (apply, unify, unifies)
import Resolvable.Util (genTermNoWildcard)

tests :: TestTree
tests =
  testGroup
    "Resolvable.Unification"
    [ testProperty "apply unifier implies terms become equal" prop_unify_apply_agrees
    , testProperty "unification is symmetric in success" prop_unify_symmetric
    , testGroup
        "unit"
        [ testCase "occurs check rejects cyclic binding" occursCheckRejectsCycle
        , testCase "propagates substitutions through bindings" propagatesBindings
        , testCase "list tail unifies against longer list" listTailUnifies
        , testCase "fails on different arity" mismatchedArityFails
        , testCase "wildcard unifies without binding" wildcardKeepsIdentity
        ]
    ]

occursCheckRejectsCycle :: Assertion
occursCheckRejectsCycle =
  unify (TVar "X") (TOp "f" [TVar "X"]) @?= Nothing

propagatesBindings :: Assertion
propagatesBindings =
  case unify (TPred "p" [TVar "X", TVar "Y"]) (TPred "p" [TVar "Y", TAtom "a"]) of
    Nothing -> assertFailure "expected a unifier"
    Just u -> do
      apply u (TVar "X") @?= TAtom "a"
      apply u (TVar "Y") @?= TAtom "a"

listTailUnifies :: Assertion
listTailUnifies =
  case unify (TList [TVar "X"] (Just (TVar "T"))) (TList [TAtom "a", TAtom "b"] Nothing) of
    Nothing -> assertFailure "expected a unifier"
    Just u -> do
      apply u (TVar "X") @?= TAtom "a"
      apply u (TVar "T") @?= TList [TAtom "b"] Nothing

mismatchedArityFails :: Assertion
mismatchedArityFails =
  unify (TPred "p" [TAtom "a"]) (TPred "p" [TAtom "a", TAtom "b"]) @?= Nothing

wildcardKeepsIdentity :: Assertion
wildcardKeepsIdentity =
  case unify TWildcard (TAtom "a") of
    Nothing -> assertFailure "expected a unifier"
    Just u -> apply u (TVar "X") @?= TVar "X"

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

