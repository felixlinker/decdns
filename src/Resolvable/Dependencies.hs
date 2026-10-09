{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Resolvable.Dependencies
  ( ResolutionOrder
  , resolutionOrder
  , DependencyMatch(..)
  , dependencyMatch
  , dependencyMatches
  , Fact(..)
  , toRule
  , computeResolvable ) where

import Control.Monad (foldM, guard)
import Control.Monad.Trans.Except (runExcept)
import Data.Either (fromRight)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (listToMaybe, mapMaybe, isJust, fromMaybe)

import qualified Resolvable.Constraint.Store as Store
import Resolvable.Constraint.Solving (getSolved)
import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import Resolvable.Resolvability (RComp(rid))
import Resolvable.Unification
import qualified Util.Graph as G
import qualified Util.CondensationGraph as CG
import Util.Monad (exceptMaybe)
import Util.Fixpoint
import Util.Fresh

data DependencyMatch = DependencyMatch
  { dependentRule :: Rule RComp
  , dependencyRule :: Rule RComp
  , dependencyPredicate :: Term
  , dependencyUnifier :: Unifier
  }
  deriving Eq

dependencyMatch :: Rule RComp -> Rule RComp -> Maybe DependencyMatch
dependencyMatch lrg@(Rule _ (toView -> TVResolves {}) b) sml@(Rule _ h@(toView -> TVResolves {}) _) = do
  let (st, b') = partitionConstraints b
  (predicate, u) <- listToMaybe $ mapMaybe unifyBodyPredicate b'
  maybe (return ()) guard $ Store.solve $ Store.simplify $ apply u st
  return $ DependencyMatch lrg sml predicate u
  where
    unifyBodyPredicate :: Term -> Maybe (Term, Unifier)
    unifyBodyPredicate p = do
      guard (isResolves p)
      u <- unify p h
      return (p, u)
dependencyMatch _ _ = Nothing

match :: Rule RComp -> Rule RComp -> Bool
match r = isJust . dependencyMatch r

dependencyMatches :: S.Set (Rule RComp) -> [DependencyMatch]
dependencyMatches rs = do
  dependent <- S.toList rs
  dependency <- S.toList rs
  maybe [] return $ dependencyMatch dependent dependency

type ResolutionOrder = G.Graph Int

resolutionOrder :: S.Set (Rule RComp) -> Fresh ResolutionOrder
resolutionOrder rs =
  let (facts, rs') = S.partition (not. any isResolves . rbody) rs
      gInit = foldr ((`G.insert` S.empty) . getId) G.empty facts
  in  foldM go gInit rs'
  where
    getId :: Rule RComp -> Int
    getId = rid . rannotation

    go :: ResolutionOrder -> Rule RComp -> Fresh ResolutionOrder
    go ord r = do
      r' <- away r
      return $ G.insert (getId r) (S.map getId $ S.filter (match r') rs) ord

partitionConstraints :: [Term] -> (Store.ConstraintStore, [Term])
partitionConstraints = foldr addConstraint (Store.empty, [])
  where
    addConstraint :: Term -> (Store.ConstraintStore, [Term]) -> (Store.ConstraintStore, [Term])
    addConstraint t (st, b) = maybe (st, t:b) (, b) (Store.add st t)

data Fact = Fact Term Store.ConstraintStore

equivFacts :: Fact -> Fact -> Bool
equivFacts (Fact f1 c1) (Fact f2 c2) = fromMaybe False $ do
  u <- unify f1 f2
  return $ isRenaming u && apply u c1 == apply u c2

instance Eq Fact where
  (==) = equivFacts

instance Ord Fact where
  compare f1@(Fact t1 c1) f2@(Fact t2 c2)
    | f1 == f2 = EQ
    | otherwise = case compare t1 t2 of
      EQ -> compare c1 c2
      comp -> comp

instance Unify Fact where
  extendUnifier (Fact f _) (Fact f' _) = extendUnifier f f'

instance Apply Fact where
  apply u (Fact t st) = Fact (apply u t) (apply u st)

instance Show Fact where
  show = show . toRule

toRule :: Fact -> Rule ()
toRule (Fact f cs) = Rule () f (Store.toBody cs)

solveBody :: Bool -> S.Set Fact -> Rule a -> Fresh (S.Set Fact)
solveBody keepGround fs r = do
  Rule _ h b <- away r
  let (st, b') = partitionConstraints b
  let us = foldl solvePredicate [(identity, st)] b'
  return $ S.fromList $ map (\(u, st') -> Fact (apply u h) (apply u st')) us
  where
    solvePredicate :: [(Unifier, Store.ConstraintStore)] -> Term -> [(Unifier, Store.ConstraintStore)]
    solvePredicate choices p = case toView p of
      TVResolves {} -> concatMap
        (\(u, accSt) -> foldr
          (\(Fact h st) acc -> maybe acc (:acc) $ do
            u' <- extendUnifier p h u
            accSt' <- getSolved $ Store.simplify $ apply u' accSt <> apply u' st
            return (u', accSt'))
          []
          fs)
        choices
      TVMember x l -> concatMap
        (\(u, accSt) -> fromRight [] $ runExcept $ do
          l' <- exceptMaybe "member" (collectList $ apply u l)
          return $ mapMaybe (\el -> do
            u' <- extendUnifier x el u
            return (u', apply u' accSt))
            l')
        choices
      _ -> error "unsupported atom encountered"

solveFixpoint :: forall a t. Traversable t => Bool -> S.Set Fact -> t (Rule a) -> Fresh (S.Set Fact)
solveFixpoint keepGround prog addFrom = fixpointSetM step prog
  where
    step :: S.Set Fact -> Fresh (S.Set Fact)
    step acc = do
      solved <- mapM (solveBody keepGround acc) addFrom
      return $ S.unions solved

solve :: forall a t. Traversable t => Bool -> S.Set Fact -> t (Rule a) -> Fresh (S.Set Fact)
solve keepGround kb addFrom = do
  solved <- mapM (solveBody keepGround kb) addFrom
  return $ foldr S.union kb solved

computeResolvable :: Bool -> S.Set (Rule RComp) -> Fresh (S.Set Fact)
computeResolvable keepGround (S.filter (isResolves . rhead) -> rs) = do
  ord <- resolutionOrder rs
  let recursive = S.filter (\i -> i `S.member` G.next ord i) (G.elems ord)
  rec recursive (CG.fromGraph ord) S.empty
  where
    rules :: M.Map Int (Rule RComp)
    rules = M.fromList $ map (\r -> (rid $ rannotation r, r)) $ S.toList rs

    getRules :: S.Set (S.Set Int) -> [Rule RComp]
    getRules = concatMap (mapMaybe (`M.lookup` rules) . S.toList) . S.toList

    rec :: S.Set Int -> CG.CondensationGraph Int -> S.Set Fact -> Fresh (S.Set Fact)
    rec recursive ord acc
      | G.null ord = pure acc
      | otherwise = do
          let leaves = G.leaves ord
          let leafRules = getRules leaves
          acc' <- if any ((> 1) . S.size) leaves || any (`S.member` recursive) (S.unions leaves)
            then solveFixpoint keepGround acc leafRules
            else solve keepGround acc leafRules
          rec recursive (foldr G.remove ord leaves) acc'
