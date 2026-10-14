{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Resolvable.Resolution
  ( Solver
  , Goal(..)
  , goal
  , sld
  , sldAll
  , sldSome
  , clearFailures
  , runSLD ) where

import Control.Monad (when, guard)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as St
import Data.Maybe (mapMaybe, maybeToList, catMaybes, isNothing)
import Data.List (uncons, partition)
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Resolvable.Constraint.Store as Store
import Resolvable.Constraint.Solving
import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import Resolvable.Unification hiding (toList)
import Util.Fresh

type Failures k m = St.StateT (S.Set (Goal k [Term])) m

type Solver k m a = Store.SimplifyCache (FreshT (Failures k m)) a

liftFailures :: Monad m => Failures k m a -> Solver k m a
liftFailures = lift . lift

clearFailures :: Monad m => Solver k m (S.Set (Goal k [Term]))
clearFailures = liftFailures $ do
  failures <- St.get
  St.put S.empty
  return failures

whenNotFailed :: (Monad m, Ord k) => Goal k [Term] -> Solver k m [a] -> Solver k m [a]
whenNotFailed g m = do
  hasFailed <- liftFailures $ St.gets (S.member (canonicalGoal g))
  if hasFailed
    then return []
    else m

failed :: (Monad m, Ord k) => Goal k [Term] -> Solver k m ()
failed = liftFailures . St.modify . S.insert . canonicalGoal

type Limits k = M.Map k Int

-- | The remaining rule budgets belong to this search branch.
data Goal k a = Goal a (Limits k) Store.ConstraintStore
  deriving (Eq, Ord)

finished :: Foldable f => Goal k (f a) -> Bool
finished (Goal gs _ _) = null gs

checkLimits :: Ord k => k -> Limits k -> Maybe (Limits k)
checkLimits ann ls = do
  guard $ maybe True (> 0) $ M.lookup ann ls
  return $ M.adjust (subtract 1) ann ls

instance Apply a => Apply (Goal k a) where
  apply u (Goal ts limits st) = Goal (apply u ts) limits (apply u st)

instance Show a => Show (Goal k a) where
  show (Goal a _ _) = "Goal " ++ show a

instance Functor (Goal k) where
  fmap f (Goal as st limits) = Goal (f as) st limits

goal :: Limits k -> Term -> (Goal k [Term], Unifier)
goal limits g0 = (Goal [g0] limits Store.empty, termIdentity g0)

awayG :: Monad m => Goal k [Term] -> FreshT m (Goal k [Term], Unifier)
awayG (Goal ts limits st) = do
  (ts', u) <- awayT ts
  return (Goal ts' limits (apply u st), u)

canonicalGoal :: Goal k [Term] -> Goal k [Term]
canonicalGoal g = fst $ runFresh $ awayG g

resolution :: (Monad m, Ord a) => [Rule a] -> Goal a [Term] -> Unifier -> Solver a m [(Goal a [Term], Unifier)]
resolution _ g@(Goal [] _ _) u = return [(g, u)]
resolution rs g@(Goal (p:_) _ _) u = do
  gs <- catMaybes <$> mapM (uncurry simplify) (resolutionStep rs g)
  when (null gs && isResolves p) (failed g)
  return gs
  where
    simplify :: Monad m => Goal a [Term] -> Unifier -> Solver a m (Maybe (Goal a [Term], Unifier))
    simplify (Goal ts limits st) u' = do
      st' <- getSolved <$> Store.simplifyCached st
      return $ (, u ~> u') . Goal ts limits <$> st'

resolutionStep :: Ord k => [Rule k] -> Goal k [Term] -> [(Goal k [Term], Unifier)]
resolutionStep _ g@(Goal [] _ _) = [(g, identity)]
resolutionStep rs (Goal (p:t) limits st) = case toView p of
  TVMember x l -> do
    l' <- maybeToList $ collectList l
    u <- mapMaybe (unify x) l'
    return (apply u (Goal t limits st), u)
  _ -> mapMaybe unifyRule rs
  where
    unifyRule (Rule annotation h b) = do
      let (stR, b') = Store.partitionConstraints b
      u <- unify p h
      limits' <- checkLimits annotation limits
      return (apply u (Goal (b' ++ t) limits' (stR <> st)), u)

sld :: (Monad m, Ord k) => [Rule k] -> Goal k [Term] -> Unifier -> Solver k m ([Unifier], [(Goal k [Term], Unifier)])
sld _ (Goal [] _ _) u = return ([u], [])
sld rs g u = do
  (g', u') <- lift $ awayG g
  gs <- whenNotFailed g $ resolution rs g' (u ~> u')
  let (done, rec) = partition (finished . fst) gs
  return (map snd done, rec)

sldAll :: (Monad m, Ord k) => [Rule k] -> Goal k [Term] -> Unifier -> Solver k m [Unifier]
sldAll rs g u = do
  (done, rec) <- sld rs g u
  recGs <- concat <$> mapM (uncurry $ sldAll rs) rec
  when (null done && null recGs) (failed g)
  return $ done ++ recGs

sldSome :: forall k m. (Monad m, Ord k) => [Rule k] -> Goal k [Term] -> Unifier -> Solver k m (Maybe Unifier)
sldSome rs g0 u0 = aux [(g0, u0)]
  where
    aux :: [(Goal k [Term], Unifier)] -> Solver k m (Maybe Unifier)
    aux [] = return Nothing
    aux ((g, u) : gs) = do
      (done, rec) <- sld rs g u
      maybe (recAux rec)
            (return . Just . fst)
            (uncons done)
      where
        recAux :: [(Goal k [Term], Unifier)] -> Solver k m (Maybe Unifier)
        recAux rec = do
          r1 <- maybeFailed =<< aux rec
          maybe (aux gs) (return . Just) r1

        maybeFailed :: Maybe Unifier -> Solver k m (Maybe Unifier)
        maybeFailed mu = when (isNothing mu) (failed g) >> return mu

runSLD :: Monad m => Solver k m a -> m (a, S.Set (Goal k [Term]))
runSLD = (`St.runStateT` S.empty) . (`evalFreshT` 0) . Store.evalCached
