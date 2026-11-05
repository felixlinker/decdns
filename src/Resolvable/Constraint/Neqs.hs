module Resolvable.Constraint.Neqs
  ( Neqs
  , Resolvable.Constraint.Neqs.empty
  , asPredicates
  , addConstraint
  , simplify
  , simplifyNeq ) where

import Resolvable.Prolog.Core
import Resolvable.Constraint.Solving

type Neqs = [(Term, Term)]

empty :: Neqs
empty = []

asPredicates :: Neqs -> [Term]
asPredicates = map (uncurry (\=))

addConstraint :: Term -> Term -> Neqs -> Neqs
addConstraint k v = ((k, v):)

-- Returns a DNF
simplify :: Neqs -> [Neqs]
simplify = foldr (aux . uncurry simplifyNeq) [[]]
  where
    aux :: [Conjunction (Maybe (Term, Term))] -> [Neqs] -> [Neqs]
    aux disj acc = disjunction acc (concatMap (maybe acc (\d -> map (d:) acc))) (simplifyDisj disj)

simplifyArgs :: [Term] -> [Term] -> [Conjunction (Maybe (Term, Term))]
-- Arguments are equal, i.e., contradictory
simplifyArgs [] [] = []
-- Arguments are unequal, i.e., constraint is redundant
simplifyArgs [] (_:_) = [Conjunction Nothing]
simplifyArgs (_:_) [] = [Conjunction Nothing]
simplifyArgs (a:ta) (b:tb) = simplifyNeq a b ++ simplifyArgs ta tb

simplifyNeq :: Term -> Term -> [Conjunction (Maybe (Term, Term))]
simplifyNeq a@(TVar _) b
  | a /= b = [Conjunction (Just (a, b))]
  | otherwise = []
-- NOTE: a below cannot be a variable
simplifyNeq a b@(TVar _) = [Conjunction (Just (a, b))]
-- Do not simplify universal quantifiers
simplifyNeq a@(TForAll _ _) b = [Conjunction (Just (a, b))]
-- NOTE: b below cannot be a variable
simplifyNeq a b = case a of
  -- We can simplify all below cases to either dropping the inequality, as the
  -- terms are unequal, or deriving a contradiction.
  TEmptyList      -> decideNonVar
  (TAtom _)       -> decideNonVar
  (TString _)     -> decideNonVar
  (TInt _)        -> decideNonVar
  (TComplex _)    -> decideNonVar
  TFunctor na argsa -> case b of
    TFunctor nb argsb
      -- If the functor name is the same, we can reduce inequality of the
      -- functor to the inequality of its list of arguments.
      | na == nb -> simplifyArgs argsa argsb
    _ -> [Conjunction Nothing]
  where
    decideNonVar :: [Conjunction (Maybe a)]
    decideNonVar
      | a == b    = []
      | otherwise = [Conjunction Nothing]
