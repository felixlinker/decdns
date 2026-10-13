{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Resolvable.Resolution
  ( Solver
  , getFailed
  , Goal(..)
  , goal
  , sld
  , runSLD ) where

import Control.Monad (foldM, when, guard)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as St
import Data.Maybe (mapMaybe, maybeToList, catMaybes)
import qualified Data.Map as M

import qualified Resolvable.Constraint.Store as Store
import Resolvable.Constraint.Solving
import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import Resolvable.Unification hiding (toList)
import Util.Fresh

type Failures m = St.StateT [(Goal [Term], Unifier)] m

type LoopBreaker k m = St.StateT (M.Map k Int) m

type Solver k m a = Store.SimplifyCache (FreshT (LoopBreaker k (Failures m))) a

liftFailures :: Monad m => Failures m a -> Solver k m a
liftFailures = lift . lift . lift

failed :: Monad m => Goal [Term] -> Unifier -> Solver k m ()
failed t = liftFailures . St.modify . (:) . (t,)

getFailed :: Monad m => Solver k m [(Goal [Term], Unifier)]
getFailed = liftFailures St.get

liftBreaker :: Monad m => LoopBreaker k (Failures m) a -> Solver k m a
liftBreaker = lift . lift

checkLoopBreaker :: (Monad m, Ord a) => Rule a -> Solver a m Bool
checkLoopBreaker (rannotation -> a) = liftBreaker $ do
  St.modify (M.alter (fmap (\limit -> limit - 1)) a)
  St.gets (maybe True (>= 0) . M.lookup a)

data Goal a = Goal a Store.ConstraintStore
  deriving (Eq, Ord)

instance Show a => Show (Goal a) where
  show (Goal a _) = "Goal " ++ show a

instance Functor Goal where
  fmap f (Goal as st) = Goal (f as) st

goal :: Term -> (Goal [Term], Unifier)
goal g0 = (Goal [g0] Store.empty, termIdentity g0)

awayG :: Monad m => Goal [Term] -> FreshT m (Goal [Term], Unifier)
awayG (Goal ts st) = do
  (ts', u) <- awayT ts
  return (Goal ts' (apply u st), u)

resolution :: (Foldable f, Monad m, Ord a) => f (Rule a) -> Goal [Term] -> Unifier -> Solver a m [(Goal [Term], Unifier)]
resolution _ g@(Goal [] _) u = return [(g, u)]
resolution rs g@(Goal (p:_) _) u = do
  gs  <- resolutionStep rs g
  gs' <- catMaybes <$> mapM (uncurry simplify) gs
  when (null gs' && isResolves p) (failed g u)
  return gs'
  where
    simplify :: Monad m => Goal [Term] -> Unifier -> Solver a m (Maybe (Goal [Term], Unifier))
    simplify (Goal ts st) u' = do
      st' <- getSolved <$> Store.simplifyCached st
      return $ (, u ~> u') . Goal ts <$> st'

resolutionStep :: (Foldable f, Monad m, Ord a) => f (Rule a) -> Goal [Term] -> Solver a m [(Goal [Term], Unifier)]
resolutionStep _ g@(Goal [] _) = return [(g, identity)]
resolutionStep rs (Goal (p:t) st) = case toView p of
  TVMember x l -> return $ do
    l' <- maybeToList $ collectList l
    u <- mapMaybe (unify x) l'
    return (Goal (apply u t) (apply u st), u)
  _ -> foldM (\acc -> fmap (maybe acc (:acc)) . unifyRule) [] rs
  where
    unifyRule :: (Monad m, Ord a) => Rule a -> Solver a m (Maybe (Goal [Term], Unifier))
    unifyRule r = do
      b <- checkLoopBreaker r
      unified <- unifyHead r
      return $ guard b >> unified

    unifyHead :: Monad m => Rule a -> Solver a m (Maybe (Goal [Term], Unifier))
    unifyHead (Rule _ h b) = return $ do
      let (stR, b') = Store.partitionConstraints b
      u <- unify p h
      return (Goal (apply u b' ++ apply u t) (apply u stR <> apply u st), u)

-- | Solve or resume a goal, preserving its remaining terms and constraint store.
sld :: (Foldable f, Monad m, Ord a) => f (Rule a) -> Goal [Term] -> Unifier -> Solver a m [Unifier]
sld _ (Goal [] _) u = return [u]
sld rs g u = do
  (g', u') <- lift $ awayG g
  gs <- resolution rs g' (u ~> u')
  concat <$> mapM (uncurry (sld rs)) gs

runSLD :: Monad m => M.Map k Int -> Solver k m a -> m (a, [(Goal [Term], Unifier)])
runSLD limits = (`St.runStateT` []) . (`St.evalStateT` limits) . (`evalFreshT` 0) .  Store.evalCached
