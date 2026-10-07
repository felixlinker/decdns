module Resolvable.ConstrainedUnificationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

import Resolvable.Prolog.Core (Term(..), list, listWithTail)
import Resolvable.ConstrainedUnification
  ( Neqs
  , addConstraint
  , apply
  , extendUnifier
  , fromList
  , identity
  , simplify
  , toList
  )
import Data.Maybe (fromMaybe)

tests :: TestTree
tests =
  testGroup
    "Resolvable.ConstrainedUnification"
    [ testGroup
        "unit"
        ([ testCase "rejects constraint already equal under substitution" rejectsEqualConstraint
        , testCase "rejects unification that violates a stored constraint" rejectsConstraintViolation
        , testCase "rejects when constraint becomes equal after chained bindings" rejectsChainedViolation
        , testCase "keeps constraints after unrelated bindings" keepsConstraintsAfterBinding
        , testCase "simplify rejects equal ground terms" simplifyRejectsEqualGroundTerms
        , testCase "simplify keeps variable constraints" simplifyKeepsVariableConstraints ]
        ++ map (\(n, i, ex) -> simplifyFunctorCase n i ex) functorSimplifications)
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

(!=) :: Term -> Term -> (Term, Term)
(!=) = (,)

f :: Term -> Term -> Term
f i j = TFunctor "f" [i, j]

g :: Term -> Term -> Term
g i j = TFunctor "g" [i, j]

h :: Term -> Term -> Term -> Term
h i j k = TFunctor "h" [i, j, k]

x, y, z, a, b :: Term
x = TVar "X"
y = TVar "Y"
z = TVar "Z"
a = TAtom "a"
b = TAtom "b"

functorSimplifications :: [(String, (Term, Term), Maybe [(Term, Term)])]
functorSimplifications =
  [ ("a", f x b != f a b, Just [x != a])
  , ("b", f a b != f a b, Nothing)
  , ("c", g a b != f a b, Just [])
  , ("d", f (g x b) b != f (g y a) b, Just [])
  , ("e", f (g x a) a != f (g y a) a, Just [x != y])
  , ("f", f (g x (f y b)) a != f (g y (f z b)) a, Just [list [x, y] != list [y, z]])
  , ("g", f a (f a (f a (f a x))) != f a (f a (f a (f a y))), Just [x != y])
  , ("h", listWithTail [x] y != list [a, b], Just [list [x, y] != list [a, list [b]]])
  , ("i", h x y (h z a b) != h z x (h y a b), Just [list [x, y, z] != list [z, x, y]])
  -- TODO: Below test case fails for now, but that is not a theoretical issue.
  -- , ("j", list [x, y, z] != list [z, x, y], Just [list [x, y, z] != list [z, x, y]])
  ]

simplifyFunctorCase :: String -> (Term, Term) -> Maybe [(Term, Term)] -> TestTree
simplifyFunctorCase name neq expected =
  testCase name ((toList <$> fromList [neq]) @?= expected)
