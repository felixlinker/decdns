{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
module Resolvable.Resolution
  ( Failures
  , Solver
  , RSolver
  , Goal(..)
  , SLDGoal
  , Limits
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
import Data.List (uncons, partition, intercalate)
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Resolvable.Constraint.Store as Store
import Resolvable.Constraint.Solving
import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import Resolvable.Unification hiding (toList)
import Util.Fresh

type Failures k m = St.StateT (S.Set (SLDGoal k)) m

type Solver m a = Store.SimplifyCache (FreshT m) a

type RSolver k m a = Solver (Failures k m) a

liftFailures :: Monad m => Failures k m a -> RSolver k m a
liftFailures = lift . lift

clearFailures :: Monad m => RSolver k m (S.Set (SLDGoal k))
clearFailures = liftFailures $ do
  failures <- St.get
  St.put S.empty
  return failures

whenNotFailed :: (Monad m, Ord k) => SLDGoal k -> RSolver k m [a] -> RSolver k m [a]
whenNotFailed g m = do
  hasFailed <- liftFailures $ St.gets (S.member (canonicalGoal g))
  if hasFailed
    then return []
    else m

failed :: (Monad m, Ord k) => SLDGoal k -> RSolver k m ()
failed = liftFailures . St.modify . S.insert . canonicalGoal

type Limits k = M.Map k Int

data Goal v a = Goal v a Store.ConstraintStore
  deriving (Eq, Ord)

type SLDGoal k = Goal (Limits k) [Term]

finished :: Foldable f => Goal k (f a) -> Bool
finished (Goal _ gs _) = null gs

checkLimits :: Ord k => k -> Limits k -> Maybe (Limits k)
checkLimits ann ls = do
  guard $ maybe True (> 0) $ M.lookup ann ls
  return $ M.adjust (subtract 1) ann ls

instance Apply a => Apply (Goal k a) where
  apply u (Goal limits ts st) = Goal limits (apply u ts) (apply u st)

instance Show (SLDGoal k) where
  show (Goal _ a st) = ":- " ++ intercalate ", " (map show $ a ++ Store.toBody st) ++ "."

instance Functor (Goal k) where
  fmap f (Goal limits as st) = Goal limits (f as) st

goal :: Limits k -> Term -> (SLDGoal k, Unifier)
goal limits g0 = (Goal limits [g0] Store.empty, termIdentity g0)

awayG :: Monad m => SLDGoal k -> FreshT m (SLDGoal k, Unifier)
awayG (Goal limits ts st) = do
  (ts', u) <- awayT ts
  return (Goal limits ts' (apply u st), u)

canonicalGoal :: SLDGoal k -> SLDGoal k
canonicalGoal g = fst $ runFresh $ awayG g

resolution :: (Monad m, Ord a) => [Rule a] -> SLDGoal a -> Unifier -> RSolver a m [(SLDGoal a, Unifier)]
resolution _ g@(Goal _ [] _) u = return [(g, u)]
resolution rs g@(Goal _ (p:_) _) u = do
  gs <- catMaybes <$> mapM (uncurry simplify) (resolutionStep rs g)
  when (null gs && isResolves p) (failed g)
  return gs
  where
    simplify :: Monad m => SLDGoal a -> Unifier -> RSolver a m (Maybe (SLDGoal a, Unifier))
    simplify (Goal ts limits st) u' = do
      st' <- getSolved <$> Store.simplifyCached st
      return $ (, u ~> u') . Goal ts limits <$> st'

resolutionStep :: forall k. Ord k => [Rule k] -> SLDGoal k -> [(SLDGoal k, Unifier)]
resolutionStep _ g@(Goal _ [] _) = [(g, identity)]
resolutionStep rs (Goal limits (p:t) st) = case toView p of
  TVMember x l -> do
    l' <- maybeToList $ collectList l
    u <- mapMaybe (unify x) l'
    return (apply u (Goal limits t st), u)
  _ -> mapMaybe unifyRule rs
  where
    unifyRule :: Rule k -> Maybe (SLDGoal k, Unifier)
    unifyRule (Rule annotation h b) = do
      let (stR, b') = Store.partitionConstraints b
      u <- unify p h
      limits' <- checkLimits annotation limits
      return (apply u (Goal limits' (b' ++ t) (stR <> st)), u)

sld :: (Monad m, Ord k) => [Rule k] -> SLDGoal k -> Unifier -> RSolver k m ([Unifier], [(SLDGoal k, Unifier)])
sld _ (Goal _ [] _) u = return ([u], [])
sld rs g u = do
  (g', u') <- lift $ awayG g
  gs <- whenNotFailed g $ resolution rs g' (u ~> u')
  let (done, rec) = partition (finished . fst) gs
  return (map snd done, rec)

factsFirst :: [Rule k] -> [Rule k]
factsFirst = uncurry (++) . partition (null . snd . Store.partitionConstraints . rbody)

sldAll :: forall m k. (Monad m, Ord k) => [Rule k] -> SLDGoal k -> Unifier -> RSolver k m [Unifier]
sldAll (factsFirst -> rs) = aux
  where
    aux :: SLDGoal k -> Unifier -> RSolver k m [Unifier]
    aux g u = do
      (done, rec) <- sld rs g u
      recGs <- concat <$> mapM (uncurry $ sldAll rs) rec
      when (null done && null recGs) (failed g)
      return $ done ++ recGs

sldSome :: forall k m. (Monad m, Ord k) => [Rule k] -> SLDGoal k -> Unifier -> RSolver k m (Maybe Unifier)
sldSome (factsFirst -> rs) g0 u0 = aux [(g0, u0)]
  where
    aux :: [(SLDGoal k, Unifier)] -> RSolver k m (Maybe Unifier)
    aux [] = return Nothing
    aux ((g, u) : gs) = do
      (done, rec) <- sld rs g u
      maybe (recAux rec)
            (return . Just . fst)
            (uncons done)
      where
        recAux :: [(SLDGoal k, Unifier)] -> RSolver k m (Maybe Unifier)
        recAux rec = do
          r1 <- maybeFailed =<< aux rec
          maybe (aux gs) (return . Just) r1

        maybeFailed :: Maybe Unifier -> RSolver k m (Maybe Unifier)
        maybeFailed mu = when (isNothing mu) (failed g) >> return mu

runSLD :: Monad m => RSolver k m a -> m (a, S.Set (SLDGoal k))
runSLD = (`St.runStateT` S.empty) . (`evalFreshT` 0) . Store.evalCached
