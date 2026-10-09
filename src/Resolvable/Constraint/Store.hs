module Resolvable.Constraint.Store
  ( ConstraintStore(..)
  , empty
  , toBody
  , add
  , Resolvable.Constraint.Store.null
  , simplify
  , Resolvable.Constraint.Store.solve ) where

import qualified Control.Monad.Trans.Except as Ex
import Data.Maybe (catMaybes)

import DNS.SEC.Verification
import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import Resolvable.Prolog.Reverse
import qualified Resolvable.Unification as U
import qualified Resolvable.Constraint.Neqs as N
import Resolvable.Constraint.Solving as Solving

data Constraint =
    Dif Term Term
  | Complex Term
  deriving (Eq, Ord)

simplifyT :: Term -> Solved (Maybe Term)
simplifyT t =
  either  (const $ Satisfiable $ Just t)
          (\b -> if b then Satisfiable Nothing else Unsatisfiable)
          (Ex.runExcept simplifyTEx)
  where
    simplifyTEx :: Ex.Except String Bool
    simplifyTEx = case toView t of
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

instance U.Apply Constraint where
  apply u (Dif a b) = Dif (U.apply u a) (U.apply u b)
  apply u (Complex t) = Complex (U.apply u t)

data ConstraintStore = CStore N.Neqs [Term]
  deriving (Eq, Ord)

instance U.Apply ConstraintStore where
  apply u (CStore ns os) = CStore (U.apply u ns) (U.apply u os)

instance Semigroup ConstraintStore where
  CStore ns os <> CStore ns' os' = CStore (ns <> ns') (os <> os')

empty :: ConstraintStore
empty = CStore N.empty []

toBody :: ConstraintStore -> [Term]
toBody (CStore ns os) = N.asPredicates ns ++ os

add :: ConstraintStore -> Term -> Maybe ConstraintStore
add (CStore ns os) t = case toView t of
  TVNeq a b -> Just $ CStore (N.addConstraint a b ns) os
  TVIsName {} -> inOthers
  TVDigestValid {} -> inOthers
  TVRRSIGValid {} -> inOthers
  TVNSECUnsigned {} -> inOthers
  TVNSEC3Unsigned {} -> inOthers
  TVClosestEncloser {} -> inOthers
  _ -> Nothing
  where
    inOthers :: Maybe ConstraintStore
    inOthers = Just $ CStore ns (t:os)

null :: ConstraintStore -> Bool
null (CStore ns o) = Prelude.null ns && Prelude.null o

simplify :: ConstraintStore -> Solved ConstraintStore
simplify (CStore ns o) = CStore <$> N.simplify ns <*> (catMaybes <$> traverse simplifyT o)

solve :: Solved ConstraintStore -> Maybe Bool
solve = Solving.solve . fmap toBody
