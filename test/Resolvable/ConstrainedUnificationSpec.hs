module Resolvable.ConstrainedUnificationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

import Resolvable.Prolog.Core (Term(..))
import Resolvable.ConstrainedUnification
  ( Neqs
  , addConstraint
  , apply
  , extendUnifier
  , fromList
  , identity
  , simplify
  )
import Data.Maybe (fromMaybe)

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
        , testCase "simplify rejects equal ground terms" simplifyRejectsEqualGroundTerms
        , testCase "simplify keeps variable constraints" simplifyKeepsVariableConstraints
        , testCase "simplify decomposes matching functors" simplifyDecomposesMatchingFunctors
        ]
    ]

expectJust :: String -> Maybe a -> (a -> Assertion) -> Assertion
expectJust msg m k = case m of
  Nothing -> assertFailure msg
  Just v -> k v

mkNeqs :: [(Term, Term)] -> Neqs
mkNeqs = fromMaybe (error "invalid test constraints") . fromList

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
  expectJust "expected constraint to be added" (addConstraint (TVar "X") (TFunctor "f" [TVar "Y"]) identity) $ \u1 ->
    expectJust "expected unifier after binding Y" (extendUnifier (TVar "Y") (TAtom "a") u1) $ \u2 ->
      case extendUnifier (TVar "X") (TFunctor "f" [TAtom "a"]) u2 of
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

simplifyRejectsEqualGroundTerms :: Assertion
simplifyRejectsEqualGroundTerms =
  fromList [(TAtom "a", TAtom "a")] @?= Nothing

simplifyKeepsVariableConstraints :: Assertion
simplifyKeepsVariableConstraints =
  simplify (mkNeqs [(TVar "X", TVar "Y"), (TAtom "a", TVar "Z")])
    @?= Just (mkNeqs [(TVar "X", TVar "Y"), (TVar "Z", TAtom "a")])

simplifyDecomposesMatchingFunctors :: Assertion
simplifyDecomposesMatchingFunctors =
  simplify
    ( mkNeqs
      [
        ( TFunctor "f" [TVar "X", TVar "Y"]
        , TFunctor "f" [TAtom "a", TAtom "b"]
        )
      ]
    )
    @?= Just (mkNeqs [(TVar "X", TAtom "a"), (TVar "Y", TAtom "b")])
