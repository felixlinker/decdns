module Resolvable.ConstrainedUnificationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck
import Data.List (foldl')

import Resolvable.Prolog.Core (Term(..))
import Resolvable.ConstrainedUnification
  ( ConstrainedUnifier(..)
  , Neqs
  , addConstraint
  , apply
  , extendUnifier
  , fromList
  , identity
  , mergeUnifiers
  , simplify
  , toList
  )
import Resolvable.Util (genTermNoWildcard)
import Data.Maybe (fromMaybe)

tests :: TestTree
tests =
  testGroup
    "Resolvable.ConstrainedUnification"
    [ testProperty "mergeUnifiers keeps left unifier when merging identity" prop_mergeIdentityRight
    , testProperty "mergeUnifiers keeps right unifier when merging into identity" prop_mergeIdentityLeft
    , testProperty "mergeUnifiers preserves constraints from both inputs" prop_mergePreservesConstraints
    , testGroup
        "unit"
        [ testCase "rejects constraint already equal under substitution" rejectsEqualConstraint
        , testCase "rejects unification that violates a stored constraint" rejectsConstraintViolation
        , testCase "rejects when constraint becomes equal after chained bindings" rejectsChainedViolation
        , testCase "keeps constraints after unrelated bindings" keepsConstraintsAfterBinding
        , testCase "simplify drops constraints involving wildcards" simplifyDropsWildcardConstraints
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

simplifyDropsWildcardConstraints :: Assertion
simplifyDropsWildcardConstraints =
  simplify (mkNeqs [(TWildcard, TAtom "a"), (TVar "X", TWildcard)])
    @?= Just (mkNeqs [])

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

data Op
  = OpExtend Term Term
  | OpConstraint Term Term

prop_mergeIdentityRight :: Property
prop_mergeIdentityRight =
  forAll genConstrainedUnifier $ \u ->
    case mergeUnifiers u identity of
      Nothing -> counterexample "merge with identity failed" False
      Just u' -> constrainedEq u u'

prop_mergeIdentityLeft :: Property
prop_mergeIdentityLeft =
  forAll genConstrainedUnifier $ \u ->
    case mergeUnifiers identity u of
      Nothing -> counterexample "merge into identity failed" False
      Just u' -> constrainedEq u u'

prop_mergePreservesConstraints :: Property
prop_mergePreservesConstraints =
  forAll genConstrainedUnifier $ \u1 ->
    forAll genConstrainedUnifier $ \u2 ->
      case mergeUnifiers u1 u2 of
        Nothing -> property True
        Just u ->
          conjoin
            [ checkConstraints u (constraints u1)
            , checkConstraints u (constraints u2)
            ]

constrainedEq :: ConstrainedUnifier -> ConstrainedUnifier -> Property
constrainedEq (CUnifier u1 c1) (CUnifier u2 c2) =
  counterexample "unifier mismatch" (u1 == u2)
    .&&. counterexample "constraint mismatch" (c1 == c2)

checkConstraints :: ConstrainedUnifier -> Neqs -> Property
checkConstraints u neqs =
  conjoin [ counterexample ("constraint violated: " ++ show a ++ " == " ++ show b) (apply u a /= apply u b)
          | (a, b) <- toList neqs
          ]

genConstrainedUnifier :: Gen ConstrainedUnifier
genConstrainedUnifier = sized $ \n -> do
  let kMax = min 6 (n + 1)
  k <- chooseInt (0, kMax)
  ops <- vectorOf k genOp
  pure $ foldl' applyOp identity ops
  where
    applyOp :: ConstrainedUnifier -> Op -> ConstrainedUnifier
    applyOp u op = case op of
      OpExtend a b -> fromMaybe u (extendUnifier a b u)
      OpConstraint a b -> fromMaybe u (addConstraint a b u)

genOp :: Gen Op
genOp =
  oneof
    [ OpExtend <$> genTermNoWildcard <*> genTermNoWildcard
    , OpConstraint <$> genTermNoWildcard <*> genTermNoWildcard
    ]
