{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Resolvable.Dependencies
  ( ResolutionOrder
  , resolutionOrder
  , Fact(..)
  , toRule
  , computeResolvable ) where

import Control.Monad (foldM, guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExcept)
import Data.Either (fromRight)
import Data.Foldable (Foldable(foldl'))
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (listToMaybe, mapMaybe, catMaybes, isJust, fromMaybe)

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

type Solver a = Store.SimplifyCache Fresh a

awayR :: Rule a -> Fresh (Rule a)
awayR (Rule a h b) = do
  fr <- fst <$> awayT (h:b)
  case fr of
    (h':b') -> return $ Rule a h' b'
    _ -> undefined

match :: Rule RComp -> Rule RComp -> Bool
match (Rule _ (toView -> TVResolves {}) b) (Rule _ h@(toView -> TVResolves {}) _) = isJust $ do
  let (st, b') = Store.partitionConstraints b
  (_, u) <- listToMaybe $ mapMaybe unifyBodyPredicate b'
  maybe (return ()) guard $ Store.solve $ Store.simplify $ apply u st
  where
    unifyBodyPredicate :: Term -> Maybe (Term, Unifier)
    unifyBodyPredicate p = do
      guard (isResolves p)
      u <- unify p h
      return (p, u)
match _ _ = False

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
      r' <- awayR r
      return $ G.insert (getId r) (S.map getId $ S.filter (match r') rs) ord

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

solveBody :: S.Set Fact -> Rule a -> Solver (S.Set Fact)
solveBody fs r = do
  Rule _ h b <- lift $ awayR r
  let (st, b') = Store.partitionConstraints b
  let us = foldl' solvePredicate [(identity, st)] b'
  facts <- catMaybes <$> mapM (uncurry $ toFact h) us
  return $ S.fromList facts
  where
    toFact :: Term -> Unifier -> Store.ConstraintStore -> Solver (Maybe Fact)
    toFact h u st = do
      reduced <- getSolved <$> Store.simplifyCached (apply u st)
      return $ Fact (apply u h) <$> reduced

    solvePredicate :: [(Unifier, Store.ConstraintStore)] -> Term -> [(Unifier, Store.ConstraintStore)]
    solvePredicate choices p = case toView p of
      TVResolves {} -> concatMap
        (\(u, accSt) -> foldr
          (\(Fact h st) acc ->
            maybe acc (\u' -> (u', apply u' (accSt <> st)) : acc) (extendUnifier p h u))
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

solveFixpoint :: forall a t. Traversable t => S.Set Fact -> t (Rule a) -> Solver (S.Set Fact)
solveFixpoint prog addFrom = fixpointSetM step prog
  where
    step :: S.Set Fact -> Solver (S.Set Fact)
    step acc = do
      solved <- mapM (solveBody acc) addFrom
      return $ S.unions solved

solve :: forall a t. Traversable t => S.Set Fact -> t (Rule a) -> Solver (S.Set Fact)
solve kb addFrom = do
  solved <- mapM (solveBody kb) addFrom
  return $ foldr S.union kb solved

-- TODO: Rename to immediate consequences
computeResolvable :: S.Set (Rule RComp) -> Fresh (S.Set Fact)
computeResolvable (S.filter (isResolves . rhead) -> rs) = Store.evalCached $ do
  ord <- lift $ resolutionOrder rs
  let recursive = S.filter (\i -> i `S.member` G.next ord i) (G.elems ord)
  rec recursive (CG.fromGraph ord) S.empty
  where
    rules :: M.Map Int (Rule RComp)
    rules = M.fromList $ map (\r -> (rid $ rannotation r, r)) $ S.toList rs

    getRules :: S.Set (S.Set Int) -> [Rule RComp]
    getRules = concatMap (mapMaybe (`M.lookup` rules) . S.toList) . S.toList

    rec :: S.Set Int -> CG.CondensationGraph Int -> S.Set Fact -> Solver (S.Set Fact)
    rec recursive ord acc
      | G.null ord = pure acc
      | otherwise = do
          let leaves = G.leaves ord
          let leafRules = getRules leaves
          acc' <- if any ((> 1) . S.size) leaves || any (`S.member` recursive) (S.unions leaves)
            then solveFixpoint acc leafRules
            else solve acc leafRules
          rec recursive (foldr G.remove ord leaves) acc'
