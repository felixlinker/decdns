{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Resolvable.Constraint.Store
  ( ConstraintStore(..)
  , partitionConstraints
  , empty
  , toBody
  , add
  , Resolvable.Constraint.Store.null
  , simplify
  , SimplifyCache
  , simplifyCached
  , evalCached
  , Resolvable.Constraint.Store.solve ) where

import qualified Control.Monad.Trans.Except as Ex
import qualified Control.Monad.Trans.State as St
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.Functor.Identity (Identity(..))
import Data.Maybe (catMaybes)

import DNS.SEC.Verification
import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import Resolvable.Prolog.Reverse
import qualified Resolvable.Unification as U
import qualified Resolvable.Constraint.Neqs as N
import Resolvable.Constraint.Solving

simplifyTEx :: Term -> Ex.Except String Bool
simplifyTEx t = case toView t of
  TVNot c -> not <$> simplifyTEx c
  TVNoUnify a b -> return $ not (U.unifies a b)
  TVIsName n
    | minNameLength n > 255 -> return False
    | ground n -> return True
  TVDigestValid n dnskeyT dsT | ground t -> do
      n' <- asName n
      dsDat <- asDSData =<< asType dsT
      dnskeyDat <- asDNSKEYData =<< asType dnskeyT
      return $ verifyDigest n' dsDat dnskeyDat
  TVRRSIGValid n dat sig k | ground t -> do
      n' <- asName n
      rrset <- asTypes dat
      sigDat <- asRRSigData =<< asType sig
      k' <- asDNSKEYData =<< asType k
      return (verifySignature n' rrset sigDat k')
  TVNSECUnsigned n nsecDat | ground t -> do
      n' <- asName n
      nsecDat' <- asNSECData =<< asType nsecDat
      return $ nsecUnsignedSubzone n' n' nsecDat'
  TVNSEC3Unsigned n hashedOwner nsec3Dat | ground t -> do
      n' <- asName n
      hashedOwner' <- asName hashedOwner
      nsec3Dat' <- asNSEC3Data =<< asType nsec3Dat
      return $ nsec3UnsignedSubzone n' hashedOwner' nsec3Dat'
  TVClosestEncloser n exN exDat nonExN nonExDat | ground t -> do
      n' <- asName n
      exN' <- asName exN
      exDat' <- asNSEC3Data =<< asType exDat
      nonExN' <- asName nonExN
      nonExDat' <- asNSEC3Data =<< asType nonExDat
      return $ verifyClosestEncloserProof n' exN' exDat' nonExN' nonExDat'
  _ -> Ex.throwE "cannot solve"

resultToSolved :: Bool -> Solved (Maybe Term)
resultToSolved b = if b then Satisfiable Nothing else Unsatisfiable

simplifyT :: Term -> Solved (Maybe Term)
simplifyT t =
  either  (const $ Satisfiable $ Just t)
          resultToSolved
          (Ex.runExcept $ simplifyTEx t)

simplifyTCached :: Monad m => Term -> SimplifyCache m (Solved (Maybe Term))
simplifyTCached t = do
  mB <- St.gets (M.lookup t)
  maybe setCache (return . resultToSolved) mB
  where
    setCache :: Monad m => SimplifyCache m (Solved (Maybe Term))
    setCache = do
      either  (const $ return $ Satisfiable $ Just t)
              (\b -> St.modify (M.insert t b) >> return (resultToSolved b))
              (Ex.runExcept $ simplifyTEx t)

data ConstraintStore = CStore N.Neqs [Term]
  deriving (Eq, Ord)

instance Show ConstraintStore where
  show cs = intercalate ", " $ map show (toBody cs)

instance U.Apply ConstraintStore where
  apply u (CStore ns os) = CStore (U.apply u ns) (U.apply u os)

instance Semigroup ConstraintStore where
  CStore ns os <> CStore ns' os' = CStore (ns <> ns') (os <> os')

partitionConstraints :: [Term] -> (ConstraintStore, [Term])
partitionConstraints = foldr addConstraint (empty, [])
  where
    addConstraint :: Term -> (ConstraintStore, [Term]) -> (ConstraintStore, [Term])
    addConstraint t (st, b) = maybe (st, t:b) (, b) (add st t)

empty :: ConstraintStore
empty = CStore N.empty []

toBody :: ConstraintStore -> [Term]
toBody (CStore ns os) = N.asPredicates ns ++ os

add :: ConstraintStore -> Term -> Maybe ConstraintStore
add (CStore ns os) t = case toView t of
  TVNeq a b -> Just $ CStore (N.addConstraint a b ns) os
  _ | isConstraint t -> inOthers
    | otherwise -> Nothing
  where
    inOthers :: Maybe ConstraintStore
    inOthers = Just $ CStore ns (t:os)

isConstraint :: Term -> Bool
isConstraint t = case toView t of
  TVNeq {} -> True
  TVNoUnify {} -> True
  TVIsName {} -> True
  TVDigestValid {} -> True
  TVRRSIGValid {} -> True
  TVNSECUnsigned {} -> True
  TVNSEC3Unsigned {} -> True
  TVClosestEncloser {} -> True
  TVNot constraint -> isConstraint constraint
  _ -> False

null :: ConstraintStore -> Bool
null (CStore ns o) = Prelude.null ns && Prelude.null o

doubleNeg :: Term -> Term
doubleNeg t = case toView t of
  TVNot (toView -> TVNot c) -> doubleNeg c
  _ -> t

syntacticNots :: [Term] -> Solved [Term]
syntacticNots [] = Satisfiable []
syntacticNots ((doubleNeg -> h):r) = case toView h of
  TVNot c | c `elem` r  -> Unsatisfiable
  _ | nott h `elem` r   -> Unsatisfiable
    | otherwise         -> Satisfiable (h:) <*> syntacticNots r

-- Returns a disjunction
simplify :: ConstraintStore -> [ConstraintStore]
simplify = runIdentity . simplifyWithCache (Identity . simplifyT)

simplifyCached :: Monad m => ConstraintStore -> SimplifyCache m [ConstraintStore]
simplifyCached = simplifyWithCache simplifyTCached

type SimplifyCache m a = St.StateT (M.Map Term Bool) m a

simplifyWithCache :: Monad m => (Term -> m (Solved (Maybe Term))) -> ConstraintStore -> m [ConstraintStore]
simplifyWithCache cache (CStore ns ts) = do
  let ns' = N.simplify ns
  case syntacticNots ts of
    Unsatisfiable -> return []
    Satisfiable ts' -> do
      ts'' <- fmap catMaybes . sequenceA <$> mapM cache ts'
      return $ CStore <$> concatMap getSolved ns' <*> getSolved ts''

evalCached :: Monad m => SimplifyCache m a -> m a
evalCached = (`St.evalStateT` M.empty)

solve :: [ConstraintStore] -> Maybe Bool
solve [] = Just False
solve stores
  | any Resolvable.Constraint.Store.null stores = Just True
  | otherwise = Nothing
