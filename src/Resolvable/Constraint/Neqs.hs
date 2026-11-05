{-# LANGUAGE ViewPatterns #-}
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
    aux :: [Solved (Maybe (Term, Term))] -> [Neqs] -> [Neqs]
    aux (simplifyDisj -> disj) acc = concatMap (maybe acc (\d -> map (d:) acc)) disj

simplifyArgs :: [Term] -> [Term] -> [Solved (Maybe (Term, Term))]
-- Arguments are equal, i.e., contradictory
simplifyArgs [] [] = []
-- Arguments are unequal, i.e., constraint is redundant
simplifyArgs [] (_:_) = [Satisfiable Nothing]
simplifyArgs (_:_) [] = [Satisfiable Nothing]
simplifyArgs (a:ta) (b:tb) = simplifyNeq a b ++ simplifyArgs ta tb

simplifyNeq :: Term -> Term -> [Solved (Maybe (Term, Term))]
simplifyNeq a@(TVar _) b
  | a /= b = [Satisfiable (Just (a, b))]
  | otherwise = []
-- NOTE: a below cannot be a variable
simplifyNeq a b@(TVar _) = [Satisfiable (Just (a, b))]
-- Do not simplify universal quantifiers
simplifyNeq a@(TForAll _ _) b = [Satisfiable (Just (a, b))]
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
    _ -> [Satisfiable Nothing]
  where
    decideNonVar :: [Solved (Maybe a)]
    decideNonVar
      | a == b    = []
      | otherwise = [Satisfiable Nothing]
