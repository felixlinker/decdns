module Resolvable.Constraint.Neqs
  ( Neqs
  , Resolvable.Constraint.Neqs.empty
  , asPredicates
  , addConstraint
  , simplify ) where

import Control.Monad (guard)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Maybe (maybeToList, catMaybes)

import Resolvable.Prolog.Core
import Resolvable.Constraint.Solving

type Neqs = [(Term, Term)]

empty :: Neqs
empty = []

asPredicates :: Neqs -> [Term]
asPredicates = map (uncurry (\=))

addConstraint :: Term -> Term -> Neqs -> Neqs
addConstraint k v = ((k, v):)

simplify :: Neqs -> Solved Neqs
simplify = fmap catMaybes . traverse (uncurry simplifyNeq)

simplifyArgs :: [Term] -> [Term] -> Solved [(Term, Term)]
-- Arguments are equal, i.e., contradictory
simplifyArgs [] [] = Unsatisfiable
-- Arguments are unequal, i.e., constraint is redundant
simplifyArgs [] (_:_) = Satisfiable []
simplifyArgs (_:_) [] = Satisfiable []
simplifyArgs (a:ta) (b:tb) = cup (maybeToList <$> simplifyNeq a b) (simplifyArgs ta tb)

listOrSingleton :: [Term] -> Term
listOrSingleton [a] = a
listOrSingleton l = list l

simplifyNeq :: Term -> Term -> Solved (Maybe (Term, Term))
simplifyNeq a@(TVar _) b
  | a /= b = Satisfiable (Just (a, b))
  | otherwise = Unsatisfiable
-- NOTE: a below cannot be a variable
simplifyNeq a b@(TVar _) = simplifyNeq b a
-- NOTE: b below cannot be a variable
simplifyNeq a b = case a of
  -- We can simplify all below cases to either dropping the inequality, as the
  -- terms are unequal, or deriving a contradiction.
  TEmptyList      -> decideNonVar
  (TAtomQuoted _) -> decideNonVar
  (TAtom _)       -> decideNonVar
  (TString _)     -> decideNonVar
  (TInt _)        -> decideNonVar
  (TComplex _)    -> decideNonVar
  TFunctor na argsa -> case b of
    TFunctor nb argsb
      -- If the functor name is the same, we can reduce inequality of the
      -- functor to the inequality of its list of arguments.
      | na == nb -> asList <$> simplifyArgs argsa argsb
    _ -> Satisfiable Nothing
  where
    asList :: [(Term, Term)] -> Maybe (Term, Term)
    asList neqs = do
      guard (not $ null neqs)
      return $ bimap listOrSingleton listOrSingleton $ unzip neqs

    decideNonVar :: Solved (Maybe a)
    decideNonVar
      | a == b    = Unsatisfiable
      | otherwise = Satisfiable Nothing
