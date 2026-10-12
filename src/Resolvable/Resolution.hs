{-# LANGUAGE TupleSections #-}
module Resolvable.Resolution
  ( Solver
  , Goal(..)
  , resolution
  , sld
  , runSLD ) where

import Control.Monad (foldM, when)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as St
import Data.Maybe (mapMaybe, maybeToList)

import qualified Resolvable.Constraint.Store as Store
import Resolvable.Constraint.Solving
import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import Resolvable.Unification hiding (toList)
import Util.Fresh

type Failures = St.State [Term]

type Solver a = Store.SimplifyCache (FreshT Failures) a

failed :: Term -> Solver ()
failed t = lift $ lift (St.modify (t:) :: Failures ())

data Goal a = Goal a Store.ConstraintStore
  deriving (Eq, Ord)

instance Show a => Show (Goal a) where
  show (Goal a _) = "Goal " ++ show a

instance Functor Goal where
  fmap f (Goal as st) = Goal (f as) st

awayG :: Monad m => Goal [Term] -> FreshT m (Goal [Term], Unifier)
awayG (Goal ts st) = do
  (ts', u) <- awayT ts
  return (Goal ts' (apply u st), u)

resolution :: Foldable f => f (Rule a) -> Goal [Term] -> Solver [(Goal [Term], Unifier)]
resolution _ g@(Goal [] _) = return [(g, identity)]
resolution rs (Goal (p:t) st) = case toView p of
  TVMember x l -> return $ do
    l' <- maybeToList $ collectList l
    u <- mapMaybe (unify x) l'
    -- TODO: Simplify constraints
    return (Goal (apply u t) (apply u st), u)
  _ -> do
    gs' <- foldM (\acc -> fmap (maybe acc (:acc)) . resolveRule) [] rs
    when (null gs') (failed p)
    return gs'
  where
    resolveRule :: Rule a -> Solver (Maybe (Goal [Term], Unifier))
    resolveRule r = maybe
      (return Nothing)
      (\(bR, stR, u) -> do
        st'' <- getSolved <$> Store.simplifyCached (apply u st <> stR)
        return $ (,u) . Goal (bR ++ apply u t) <$> st'')
      (solveHead r)

    solveHead :: Rule a -> Maybe ([Term], Store.ConstraintStore, Unifier)
    solveHead (Rule _ h b) = do
      let (stR, b') = Store.partitionConstraints b
      u <- unify p h
      return (apply u b', apply u stR, u)

sld :: Foldable f => f (Rule a) -> Term -> Solver [Unifier]
sld rs g0 = aux (Goal [g0] Store.empty) (termIdentity g0)
  where
    aux :: Goal [Term] -> Unifier -> Solver [Unifier]
    aux g u = do
      gs <- resolution rs g
      concat <$> mapM (uncurry rec) gs
      where
        rec :: Goal [Term] -> Unifier -> Solver [Unifier]
        rec (Goal [] _) u' = return [u ~> u']
        rec g' u' = do
          (g'', u'') <- lift $ awayG g'
          aux g'' (u ~> u' ~> u'')

runSLD :: Solver [Unifier] -> ([Unifier], [Term])
runSLD = (`St.runState` []) . (`evalFreshT` 0) . Store.evalCached
