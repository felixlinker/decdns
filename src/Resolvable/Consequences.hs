{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Resolvable.Consequences
  ( ResolutionOrder
  , resolutionOrder
  , Fact(..)
  , applySimplify
  , awayF
  , toRule
  , immediateConsequences
  , allResolvable ) where

import Control.Monad (foldM, guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExcept)
import Data.Either (fromRight)
import Data.Foldable (Foldable(foldl'))
import Data.Functor.Identity (Identity)
import qualified Data.Set as S
import Data.Maybe (listToMaybe, mapMaybe, catMaybes, isJust)

import qualified Resolvable.Constraint.Store as Store
import Resolvable.Constraint.Solving (getSolved)
import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import Resolvable.Resolution
import Resolvable.Unification
import qualified Util.Graph as G
import qualified Util.CondensationGraph as CG
import Util.Monad (exceptMaybe)
import Util.Fixpoint
import Util.Fresh
import DNS.Name (Name)
import qualified DNS.RecordType as T

type CSolver a = Solver Identity a

awayR :: Monad m => Rule a -> FreshT m (Rule a)
awayR (Rule a h b) = do
  fr <- fst <$> awayT (h:b)
  case fr of
    (h':b') -> return $ Rule a h' b'
    _ -> undefined

match :: Rule a -> Rule a -> Bool
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

type ResolutionOrder a = G.Graph (Rule a)

resolutionOrder :: forall m a. Monad m => S.Set (Rule a) -> FreshT m (ResolutionOrder a)
resolutionOrder rs =
  let (facts, rs') = S.partition (not. any isResolves . rbody) rs
      gInit = foldr (`G.insert` S.empty) G.empty facts
  in  foldM go gInit rs'
  where
    go :: ResolutionOrder a -> Rule a -> FreshT m (ResolutionOrder a)
    go ord r = do
      r' <- awayR r
      return $ G.insert r (S.filter (match r') rs) ord

data Fact = Fact Term Store.ConstraintStore
  deriving (Eq, Ord)

awayF :: Monad m => Fact -> FreshT m Fact
awayF (Fact h st) = do
  (h', u) <- away h
  return $ Fact h' (apply u st)

canonFact :: Fact -> Fact
canonFact = runFresh . awayF

factSet :: Foldable f => f Fact -> S.Set Fact
factSet = foldr (S.insert . canonFact) S.empty

instance Unify Fact where
  extendUnifier (Fact f _) (Fact f' _) = extendUnifier f f'

instance Apply Fact where
  apply u (Fact t st) = Fact (apply u t) (apply u st)

applySimplify :: Unifier -> Fact -> Maybe Fact
applySimplify u f =
  let Fact t st = apply u f
  in  Fact t <$> getSolved (Store.simplify st)

instance Show Fact where
  show = show . toRule

toRule :: Fact -> Rule ()
toRule (Fact f cs) = Rule () f (Store.toBody cs)

solveBody :: S.Set Fact -> Rule a -> CSolver (S.Set Fact)
solveBody fs r = do
  Rule _ h b <- lift $ awayR r
  let (st, b') = Store.partitionConstraints b
  let us = foldl' solvePredicate [(identity, st)] b'
  facts <- catMaybes <$> mapM (uncurry $ toFact h) us
  return $ factSet facts
  where
    toFact :: Term -> Unifier -> Store.ConstraintStore -> CSolver (Maybe Fact)
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

solveFixpoint :: forall a t. Traversable t => S.Set Fact -> t (Rule a) -> CSolver (S.Set Fact)
solveFixpoint prog addFrom = fixpointSetM step prog
  where
    step :: S.Set Fact -> CSolver (S.Set Fact)
    step acc = do
      solved <- mapM (solveBody acc) addFrom
      return $ S.unions solved

solve :: forall a t. Traversable t => S.Set Fact -> t (Rule a) -> CSolver (S.Set Fact)
solve kb addFrom = do
  solved <- mapM (solveBody kb) addFrom
  return $ foldr S.union kb solved

immediateConsequences :: S.Set (Rule a) -> Fresh (S.Set Fact)
immediateConsequences (S.filter (isResolves . rhead) -> rs) = Store.evalCached $ do
  ord <- lift $ resolutionOrder rs
  let recursive = S.filter (\i -> i `S.member` G.next ord i) (G.elems ord)
  rec recursive (CG.fromGraph ord) S.empty
  where
    rec :: S.Set (Rule a) -> CG.CondensationGraph (Rule a) -> S.Set Fact -> CSolver (S.Set Fact)
    rec recursive ord acc
      | G.null ord = pure acc
      | otherwise = do
          let leaves = G.leaves ord
          let leafRules = S.toList $ S.unions leaves
          acc' <- if any ((> 1) . S.size) leaves || any (`S.member` recursive) (S.unions leaves)
            then solveFixpoint acc leafRules
            else solve acc leafRules
          rec recursive (foldr G.remove ord leaves) acc'

allResolvable :: Name String -> S.Set Fact -> S.Set Fact
allResolvable (nameTerm -> startZone) = foldr aux S.empty . S.toList
  where
    aux :: Fact -> S.Set Fact -> S.Set Fact
    aux f acc = maybe acc (`S.insert` acc) (resolvableFact startZone f)

-- TODO: Restrict neqs to variables that occur in new head term
resolvableFact :: Term -> Fact -> Maybe Fact
resolvableFact startZone (Fact t neqs) = (`Fact` neqs) <$> resolvableFromResolves startZone t

resolvableFromResolves :: Term -> Term -> Maybe Term
resolvableFromResolves startZone (toView -> TVResolves r@(Resolves { resolvesZone = z, resolvesQType = qt }))
  -- Do not report referral data as resolvable
  | z /= startZone = Nothing
  -- Do not report structural records as resolvable
  | qt `elem` map ttypeTerm [T.CNAME, T.DNAME, T.SOA, T.NS, T.DS, T.DNSKEY, T.NSEC, T.NSEC3, T.NSEC3PARAM, T.RRSIG] = Nothing
  | otherwise = Just $ resolvable
    (resolvesQType r)
    (resolvesQName r)
    (resolvesVerify r)
resolvableFromResolves _ _ = Nothing
