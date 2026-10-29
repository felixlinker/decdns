{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
module Resolvable.Resolution
  ( FailureMap
  , putFailure
  , hasFailed
  , limitsSmallerEq
  , Failures
  , Solver
  , RSolver
  , Goal(..)
  , BoundedGoal
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
import Data.Maybe (mapMaybe, maybeToList, isNothing)
import Data.List (uncons, partition, intercalate)
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Resolvable.Constraint.Store as Store
import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import Resolvable.Unification hiding (toList)
import Util.Fresh

type Limits k = M.Map k Int

checkLimits :: Ord k => k -> Limits k -> Maybe (Limits k)
checkLimits ann ls = do
  guard $ maybe True (> 0) $ M.lookup ann ls
  return $ M.adjust (subtract 1) ann ls

limitsSmallerEq :: forall k. Ord k => Limits k -> Limits k -> Bool
limitsSmallerEq m1 = M.foldrWithKey aux True
  where
    aux :: k -> Int -> Bool -> Bool
    aux k v acc = acc && maybe False (<= v) (M.lookup k m1)

type FailureMap k = M.Map (SLDGoal ()) [Limits k]

goalKey :: BoundedGoal k -> SLDGoal ()
goalKey (canonicalGoal -> Goal _ ts st) = Goal () ts st

putFailure :: forall k. Ord k => BoundedGoal k -> FailureMap k -> FailureMap k
putFailure g@(Goal ls _ _) = M.alter aux (goalKey g)
  where
    insert :: [Limits k] -> [Limits k]
    insert lls = if any (limitsSmallerEq ls) lls
      then lls
      else ls : filter (\ls' -> not (limitsSmallerEq ls' ls)) lls

    aux :: Maybe [Limits k] -> Maybe [Limits k]
    aux = Just . maybe [ls] insert

hasFailed :: Ord k => BoundedGoal k -> FailureMap k -> Bool
hasFailed g@(Goal ls _ _) = maybe False (any (limitsSmallerEq ls)) . M.lookup (goalKey g)

type Failures k m = St.StateT (FailureMap k) m

type Solver m a = Store.SimplifyCache (FreshT m) a

type RSolver k m a = Solver (Failures k m) a

liftFailures :: Monad m => Failures k m a -> RSolver k m a
liftFailures = lift . lift

clearFailures :: Monad m => RSolver k m (S.Set (BoundedGoal k))
clearFailures = liftFailures $ do
  failures <- St.get
  St.put M.empty
  return $ S.mapMonotonic fromSLDGoal $ M.keysSet failures

whenNotFailed :: (Monad m, Ord k) => BoundedGoal k -> RSolver k m [a] -> RSolver k m [a]
whenNotFailed g m = do
  b <- liftFailures $ St.gets (hasFailed g)
  if b then return [] else m

failed :: (Monad m, Ord k) => BoundedGoal k -> RSolver k m ()
failed = liftFailures . St.modify . putFailure

data Goal v a = Goal v a Store.ConstraintStore
  deriving (Eq, Ord)

type SLDGoal a = Goal a [Term]

type BoundedGoal k = SLDGoal (Limits k)

fromSLDGoal :: SLDGoal a -> BoundedGoal k
fromSLDGoal (Goal _ ts st) = Goal M.empty ts st

finished :: Foldable f => Goal k (f a) -> Bool
finished (Goal _ gs _) = null gs

instance Apply a => Apply (Goal k a) where
  apply u (Goal limits ts st) = Goal limits (apply u ts) (apply u st)

instance Show (SLDGoal k) where
  show (Goal _ a st) = ":- " ++ intercalate ", " (map show $ a ++ Store.toBody st) ++ "."

instance Functor (Goal k) where
  fmap f (Goal limits as st) = Goal limits (f as) st

goal :: Limits k -> Term -> (BoundedGoal k, Unifier)
goal limits g0 = (Goal limits [g0] Store.empty, termIdentity g0)

awayG :: Monad m => BoundedGoal k -> FreshT m (BoundedGoal k, Unifier)
awayG (Goal limits ts st) = do
  (ts', u) <- awayT ts
  return (Goal limits ts' (apply u st), u)

canonicalGoal :: BoundedGoal k -> BoundedGoal k
canonicalGoal g = fst $ runFresh $ awayG g

resolution :: (Monad m, Ord a) => [Rule a] -> BoundedGoal a -> Unifier -> RSolver a m [(BoundedGoal a, Unifier)]
resolution _ g@(Goal _ [] _) u = return [(g, u)]
resolution rs g@(Goal _ (p:_) _) u = do
  gs <- concat <$> mapM (uncurry simplify) (resolutionStep rs g)
  when (null gs && isResolves p) (failed g)
  return gs
  where
    simplify :: Monad m => BoundedGoal a -> Unifier -> RSolver a m [(BoundedGoal a, Unifier)]
    simplify (Goal ts limits st) u' = do
      st' <- Store.simplifyCached st
      return $ (, u ~> u') . Goal ts limits <$> st'

resolutionStep :: forall k. Ord k => [Rule k] -> BoundedGoal k -> [(BoundedGoal k, Unifier)]
resolutionStep _ g@(Goal _ [] _) = [(g, identity)]
resolutionStep rs (Goal limits (p:t) st) = case toView p of
  TVMin {} -> resolutionStep rs (Goal limits (member p (list minFacts) : t) st)
  TVMember x l -> do
    l' <- maybeToList $ collectList l
    u <- mapMaybe (unify x) l'
    return (apply u (Goal limits t st), u)
  _ -> mapMaybe unifyRule rs
  where
    unifyRule :: Rule k -> Maybe (BoundedGoal k, Unifier)
    unifyRule (Rule annotation h b) = do
      let (stR, b') = Store.partitionConstraints b
      u <- unify p h
      limits' <- checkLimits annotation limits
      return (apply u (Goal limits' (b' ++ t) (stR <> st)), u)

sld :: (Monad m, Ord k) => [Rule k] -> BoundedGoal k -> Unifier -> RSolver k m ([Unifier], [(BoundedGoal k, Unifier)])
sld _ (Goal _ [] _) u = return ([u], [])
sld rs g u = do
  (g', u') <- lift $ awayG g
  gs <- whenNotFailed g $ resolution rs g' (u ~> u')
  let (done, rec) = partition (finished . fst) gs
  return (map snd done, rec)

factsFirst :: [Rule k] -> [Rule k]
factsFirst = uncurry (++) . partition (null . snd . Store.partitionConstraints . rbody)

sldAll :: forall m k. (Monad m, Ord k) => [Rule k] -> BoundedGoal k -> Unifier -> RSolver k m [Unifier]
sldAll (factsFirst -> rs) = aux
  where
    aux :: BoundedGoal k -> Unifier -> RSolver k m [Unifier]
    aux g u = do
      (done, rec) <- sld rs g u
      recGs <- concat <$> mapM (uncurry $ sldAll rs) rec
      when (null done && null recGs) (failed g)
      return $ done ++ recGs

sldSome :: forall k m. (Monad m, Ord k) => [Rule k] -> BoundedGoal k -> Unifier -> RSolver k m (Maybe Unifier)
sldSome (factsFirst -> rs) g0 u0 = aux [(g0, u0)]
  where
    aux :: [(BoundedGoal k, Unifier)] -> RSolver k m (Maybe Unifier)
    aux [] = return Nothing
    aux ((g, u) : gs) = do
      (done, rec) <- sld rs g u
      maybe (recAux rec)
            (return . Just . fst)
            (uncons done)
      where
        recAux :: [(BoundedGoal k, Unifier)] -> RSolver k m (Maybe Unifier)
        recAux rec = do
          r1 <- maybeFailed =<< aux rec
          maybe (aux gs) (return . Just) r1

        maybeFailed :: Maybe Unifier -> RSolver k m (Maybe Unifier)
        maybeFailed mu = when (isNothing mu) (failed g) >> return mu

runSLD :: Monad m => RSolver k m a -> m (a, FailureMap k)
runSLD = (`St.runStateT` M.empty) . (`evalFreshT` 0) . Store.evalCached
