module Resolvable.Resolution
  ( Solver
  , Goal(..)
  , resolution ) where

import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift, MonadTrans)
import Data.Maybe (mapMaybe, maybeToList)

import qualified Resolvable.Constraint.Store as Store
import Resolvable.Constraint.Solving
import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import Resolvable.Unification
import Util.Fresh

type Solver t m a = t (FreshT m) a

data Goal a = Goal a Store.ConstraintStore
  deriving (Eq, Ord)

instance Functor Goal where
  fmap f (Goal as st) = Goal (f as) st

awayG :: Monad m => Goal [Term] -> FreshT m (Goal [Term])
awayG (Goal ts st) = do
  (ts', u) <- awayT ts
  return $ Goal ts' (apply u st)

resolution :: (Foldable f, MonadTrans t) => f (Rule a) -> Goal [Term] -> Solver t Maybe [Goal [Term]]
resolution _ g@(Goal [] _) = return [g]
resolution rs g@(Goal (p:t) st) = case toView p of
  TVMember x l -> return $ do
    l' <- maybeToList $ collectList l
    u <- mapMaybe (unify x) l'
    return (Goal (apply u t) (apply u st))
  TVResolves {} -> do
    g' <- lift $ awayG g
    foldM (\acc -> fmap (:acc) . resolveRule g') [] rs
  _ -> error "unsupportd predicate"
  where
    resolveRule :: MonadTrans t => Goal [Term] -> Rule a -> Solver t Maybe (Goal [Term])
    resolveRule g'@(Goal [] _) _ = return g'
    resolveRule (Goal (p':t') st') r = do
      (bR, stR, u) <- solveHead p' r
      st'' <- lift $ getSolved $ Store.simplify (apply u st' <> stR)
      return $ Goal (bR ++ apply u t') st''

    solveHead :: MonadTrans t => Term -> Rule a -> Solver t Maybe ([Term], Store.ConstraintStore, Unifier)
    solveHead gH (Rule _ h b) = do
      let (stR, b') = Store.partitionConstraints b
      u <- lift $ lift $ unify gH h
      return (apply u b', apply u stR, u)
