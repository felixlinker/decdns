{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Resolvable.Dependencies
  ( ResolutionOrder
  , resolutionOrder
  , DependencyMatch(..)
  , dependencyMatch
  , dependencyMatches
  , ConstrainedQuery(..)
  , queryFromRule
  , computeResolvable ) where

import Control.Monad (filterM, guard, unless)
import Control.Monad.Trans.Except (ExceptT, Except, runExcept, throwE)
import qualified Control.Monad.Trans.State as St
import Data.Bifunctor (Bifunctor(bimap))
import Data.Either (fromRight)
import Data.Functor (($>))
import qualified Data.Set as S
import Data.Maybe (listToMaybe, mapMaybe, isJust)

import DNS.SEC.Verification (verifySignature, verifyDigest, nsecUnsignedSubzone, nsec3UnsignedSubzone, verifyClosestEncloserProof)
import qualified Resolvable.ConstrainedUnification as CU
import Resolvable.Prolog.Core
import Resolvable.Prolog.Reverse
import Resolvable.Prolog.TermView
import Resolvable.Resolvability (RComp)
import Resolvable.Unification
import qualified Util.Graph as G
import qualified Util.CondensationGraph as CG
import Util.Monad (exceptMaybe)
import Util.Fixpoint

data DependencyMatch = DependencyMatch
  { dependentRule :: Rule (Maybe RComp)
  , dependencyRule :: Rule (Maybe RComp)
  , dependencyPredicate :: Term
  , dependencyUnifier :: Unifier
  }
  deriving Eq

dependencyMatch :: Rule (Maybe RComp) -> Rule (Maybe RComp) -> Maybe DependencyMatch
dependencyMatch lrg@(Rule _ (toView -> TVResolves {}) b) sml@(Rule _ h@(toView -> TVResolves {}) _) = do
  (predicate, u) <- listToMaybe $ mapMaybe unifyBodyPredicate b
  _ <- CU.applyRule (CU.fromUnifier u) sml
  return $ DependencyMatch lrg sml predicate u
  where
    unifyBodyPredicate :: Term -> Maybe (Term, Unifier)
    unifyBodyPredicate p = do
      u <- unify p h
      return (p, u)
dependencyMatch _ _ = Nothing

match :: Rule (Maybe RComp) -> Rule (Maybe RComp) -> Bool
match r = isJust . dependencyMatch r

dependencyMatches :: S.Set (Rule (Maybe RComp)) -> [DependencyMatch]
dependencyMatches rs = do
  dependent <- S.toList rs
  dependency <- S.toList rs
  maybe [] return $ dependencyMatch dependent dependency

type ResolutionOrder = G.Graph (Rule (Maybe RComp))

resolutionOrder :: S.Set (Rule (Maybe RComp)) -> ResolutionOrder
resolutionOrder rs = foldr go G.empty rs
  where
    go :: Rule (Maybe RComp) -> ResolutionOrder -> ResolutionOrder
    go r = G.insert r (S.filter (match r) rs)

-- TODO: Refactor
data ConstrainedQuery = CQuery Term CU.Neqs
  deriving (Eq, Ord)

instance Apply (Maybe ConstrainedQuery) where
  apply :: Unifier -> Maybe ConstrainedQuery -> Maybe ConstrainedQuery
  apply _ Nothing = Nothing
  apply u (Just (CQuery t neqs)) = do
    let app = apply u
    neqs' <- CU.fromList (map (bimap app app) (CU.toList neqs))
    return $ CQuery (apply u t) neqs'

toRule :: ConstrainedQuery -> Rule ()
toRule (CQuery q neqs) = Rule () q (CU.asPredicates neqs)

queryFromRule :: Monad m => Rule a -> ExceptT String m ConstrainedQuery
queryFromRule (Rule _ h b) = do
  (rest, cu) <- exceptMaybe "contradictory body" $ grabNeqs b CU.identity
  unless (null rest) (throwE "predicate other than inequality in body")
  return $ CQuery h (CU.constraints cu)

grabNeqs :: [Term] -> CU.ConstrainedUnifier -> Maybe ([Term], CU.ConstrainedUnifier)
grabNeqs ts = St.runStateT (filterM importNeq ts)
  where
    importNeq :: Term -> St.StateT CU.ConstrainedUnifier Maybe Bool
    importNeq bodyT = case toView bodyT of
      TVNeq x y -> do
        St.modifyM (CU.addConstraint x y)
        return False
      _ -> return True

instance Show ConstrainedQuery where
  show = show . toRule

matchHead :: Term -> Rule a -> CU.ConstrainedUnifier -> Maybe (CU.ConstrainedUnifier, [Term])
matchHead t (Rule _ h b) cu = do
  cu' <- CU.extendUnifier t h cu
  (b', cu'') <- grabNeqs b cu'
  return (cu'', CU.apply cu'' b')

solveBody :: Ord a => Bool -> S.Set (Rule a) -> Rule a -> S.Set (Rule a)
solveBody keepGround rs (Rule ann h b) = S.fromList $ map (\(u, b') -> CU.apply u $ Rule ann h b') $ foldl solvePredicate [(CU.identity, [])] b
  where
    groundP :: (CU.ConstrainedUnifier, [Term]) -> Bool
    groundP = if keepGround then null . snd else const True

    solvePredicate :: [(CU.ConstrainedUnifier, [Term])] -> Term -> [(CU.ConstrainedUnifier, [Term])]
    solvePredicate choices p = filter groundP $ case toView p of
      TVNeq t1 t2 -> mapMaybe (\(u, accB) -> (,accB) <$> CU.addConstraint t1 t2 u) choices
      TVResolves {} -> concatMap
        (\(u, accB) -> foldr
          (\cq acc -> maybe acc (:acc) $ do
            (u', b') <- matchHead p cq u
            return (u', CU.apply u' accB ++ b'))
          []
          rs)
        choices
      TVMember x l -> concatMap
        (\(u, accB) -> fromRight [(u, accB ++ [p])] $ runExcept $ do
          let ul = CU.apply u l
          l' <- exceptMaybe "member" (collectList ul)
          return $ mapMaybe (\el -> do
            u' <- CU.extendUnifier x el u
            return (u', CU.apply u' accB))
            l')
        choices
      TVDigestValid n dnskeyT dsT -> mapMaybe
        (skipUnsolvable $ \u -> do
          let app = CU.apply u
          n' <- asName (app n)
          dsDat <- asDSData =<< asType (app dsT)
          dnskeyDat <- asDNSKEYData =<< asType (app dnskeyT)
          return $ verifyDigest n' dsDat dnskeyDat)
        choices
      TVRRSIGValid n dat sig k -> mapMaybe
        (skipUnsolvable $ \u -> do
          let app = CU.apply u
          n' <- asName (app n)
          rrset <- asTypes (app dat)
          sigDat <- asRRSigData =<< asType (app sig)
          k' <- asDNSKEYData =<< asType (app k)
          return (verifySignature n' rrset sigDat k'))
        choices
      TVNSECUnsigned n nsecDat -> mapMaybe
        (skipUnsolvable $ \u -> do
          let app = CU.apply u
          n' <- asName (app n)
          nsecDat' <- asNSECData =<< asType (app nsecDat)
          return $ nsecUnsignedSubzone n' n' nsecDat')
        choices
      TVNSEC3Unsigned n hashedOwner nsec3Dat -> mapMaybe
        (skipUnsolvable $ \u -> do
          let app = CU.apply u
          n' <- asName (app n)
          hashedOwner' <- asName (app hashedOwner)
          nsec3Dat' <- asNSEC3Data =<< asType (app nsec3Dat)
          return $ nsec3UnsignedSubzone n' hashedOwner' nsec3Dat')
        choices
      TVClosestEncloser n exN exDat nonExN nonExDat -> mapMaybe
        (skipUnsolvable $ \u -> do
          let app = CU.apply u
          n' <- asName (app n)
          exN' <- asName (app exN)
          exDat' <- asNSEC3Data =<< asType (app exDat)
          nonExN' <- asName (app nonExN)
          nonExDat' <- asNSEC3Data =<< asType (app nonExDat)
          return $ verifyClosestEncloserProof n' exN' exDat' nonExN' nonExDat')
        choices
      -- This drops unsupported predicates, implicitly assuming that they are true
      _ -> choices
      where
        skipUnsolvable :: (CU.ConstrainedUnifier -> Except a Bool) -> (CU.ConstrainedUnifier, [Term]) -> Maybe (CU.ConstrainedUnifier, [Term])
        skipUnsolvable check (u, accB) = either (const $ Just (u, accB ++ [p])) (returnWhen (u, accB)) $ runExcept (check u)
          where
            returnWhen :: a -> Bool -> Maybe a
            returnWhen a = ($> a) . guard

solveFixpoint :: forall a t. (Foldable t, Ord a) => Bool -> t (Rule a) -> S.Set (Rule a) -> S.Set (Rule a)
solveFixpoint keepGround addFrom = fixpointSet step
  where
    step :: S.Set (Rule a) -> [Rule a]
    step acc = concatMap (S.toList . solveBody keepGround acc) addFrom

computeResolvable :: Bool -> S.Set (Rule (Maybe RComp)) -> S.Set (Rule (Maybe RComp))
computeResolvable keepGround (S.filter (isResolves . rhead) -> rs) =
  rec (CG.fromGraph $ resolutionOrder rs) S.empty
  where
    rec :: CG.CondensationGraph (Rule (Maybe RComp)) -> S.Set (Rule (Maybe RComp)) -> S.Set (Rule (Maybe RComp))
    rec ord acc
      | G.null ord = acc
      | otherwise =
          let leaves = G.leaves ord
              acc' = foldr (solveFixpoint keepGround) acc leaves
          in rec (foldr G.remove ord leaves) acc'
