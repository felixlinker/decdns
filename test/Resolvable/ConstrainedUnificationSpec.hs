module Resolvable.ConstrainedUnificationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

import Resolvable.Prolog (Term(..))
import Resolvable.ConstrainedUnification
  ( addConstraint
  , apply
  , extendUnifier
  , identity
  )

tests :: TestTree
tests =
  testGroup
    "Resolvable.ConstrainedUnification"
    [ testGroup
        "unit"
        [ testCase "rejects constraint already equal under substitution" rejectsEqualConstraint
        , testCase "rejects unification that violates a stored constraint" rejectsConstraintViolation
        , testCase "rejects when constraint becomes equal after chained bindings" rejectsChainedViolation
        , testCase "keeps constraints after unrelated bindings" keepsConstraintsAfterBinding
        ]
    ]

expectJust :: String -> Maybe a -> (a -> Assertion) -> Assertion
expectJust msg m k = case m of
  Nothing -> assertFailure msg
  Just v -> k v

rejectsEqualConstraint :: Assertion
rejectsEqualConstraint =
  expectJust "expected a unifier" (extendUnifier (TVar "X") (TAtom "a") identity) $ \u ->
    case addConstraint (TVar "X") (TAtom "a") u of
      Nothing -> pure ()
      Just _ -> assertFailure "expected constraint to be rejected"

rejectsConstraintViolation :: Assertion
rejectsConstraintViolation =
  expectJust "expected constraint to be added" (addConstraint (TVar "X") (TVar "Y") identity) $ \u ->
    case extendUnifier (TVar "X") (TVar "Y") u of
      Nothing -> pure ()
      Just _ -> assertFailure "expected constraint violation"

rejectsChainedViolation :: Assertion
rejectsChainedViolation =
  expectJust "expected constraint to be added" (addConstraint (TVar "X") (TOp "f" [TVar "Y"]) identity) $ \u1 ->
    expectJust "expected unifier after binding Y" (extendUnifier (TVar "Y") (TAtom "a") u1) $ \u2 ->
      case extendUnifier (TVar "X") (TOp "f" [TAtom "a"]) u2 of
        Nothing -> pure ()
        Just _ -> assertFailure "expected constraint violation after chaining"

keepsConstraintsAfterBinding :: Assertion
keepsConstraintsAfterBinding =
  expectJust "expected constraint to be added" (addConstraint (TVar "X") (TVar "Y") identity) $ \u1 ->
    expectJust "expected unifier after binding X" (extendUnifier (TVar "X") (TAtom "a") u1) $ \u2 -> do
      apply u2 (TVar "X") @?= TAtom "a"
      case extendUnifier (TVar "Y") (TAtom "a") u2 of
        Nothing -> pure ()
        Just _ -> assertFailure "expected constraint to still block equality"
