{-# LANGUAGE ViewPatterns #-}
module Resolvable.Update.Disunification
  ( withouts ) where

import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Map as M

import qualified Resolvable.Constraint.Store as Store
import Resolvable.Constraint.Solving (getSolved)
import Resolvable.Dependencies
import qualified Resolvable.Unification as U
import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView

toNeq :: Term -> Maybe (Term, Term)
toNeq (toView -> TVNeq a b) = Just (a, b)
toNeq _ = Nothing  -- TODO: This case may silently drop non-neq constraints; address that

negateFact :: Fact -> Maybe [Term]
negateFact (Fact t (mapMaybe toNeq . Store.toBody -> neqs))
  | null neqs = Nothing
  -- Treat failing unification as undefined because we know that CU.toList will
  -- return variables as left term, which always succeeds.
  | otherwise = Just $ map (maybe undefined (`U.apply` t) . uncurry U.unify) neqs

-- @unified a b@ "intersects" a with b.
unified :: Fact -> Term -> Maybe Fact
unified (Fact t st) t' = do
  u <- U.unify t t'
  st' <- getSolved $ Store.simplify $ U.apply u st
  return $ Fact (U.apply u t) st'

-- @disunified a b@ "subtracts" b from a. Assumes that intersection of b and a
-- is non-empty.
disunified :: Fact -> Term -> [Fact]
disunified (Fact t st) t' = fromMaybe [] $ do
  (U.Unifier (M.toList -> u)) <- U.filterRenamings . U.restrict (vars t) <$> U.unify t t'
  -- The above unifier is the intersection of @t@ and @t'@. As we want to
  -- subtract @t'@, we treat the the unifier as a conjuction of assignments, the
  -- negation of which is a disjunction of inequalites, which we add to the
  -- constraints below.
  return $ mapMaybe (fmap (Fact t) . addNeq) u
  where
    addNeq :: (Term, Term) -> Maybe Store.ConstraintStore
    addNeq (a, b) = getSolved . Store.simplify =<< Store.add st (a \= b)

without :: Fact -> Fact -> [Fact]
without pos@(Fact t _) neg@(Fact t' _)
  | U.unifies t t' =
            -- If @neg@ does not contain inequalities, we subtract @t'@ from
            -- @pos@ by disunification.
      maybe (disunified pos t')
            -- If @neg@ contains inequalities, we can easliy create its inverse
            -- by treating the inequalites as disjunction of assignments. In
            -- that case, we only need to unify @pos@ with each disjunct.
            (mapMaybe (unified pos))
            (negateFact neg)
  -- If the intersection of t and t' is empty, we need not consider t' for
  -- subtraction.
  | otherwise = [pos]

withouts :: Fact -> [Fact] -> [Fact]
withouts q = foldr (\nq -> concatMap (`without` nq)) [q]
