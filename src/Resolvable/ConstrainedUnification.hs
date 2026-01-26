module Resolvable.ConstrainedUnification
  ( ConstrainedUnifier(..)
  , Neqs
  , identity
  , apply
  , addConstraint
  , extendUnifier
  , mergeUnifiers ) where

import qualified Data.Set as S
import qualified Data.Map as M

import qualified Resolvable.Unification as U
import Resolvable.Prolog
import Control.Monad (guard)
import Data.List (intercalate)
import Data.Bifunctor (Bifunctor(bimap))
import Util.List (zipStrict)
import Data.Maybe (fromMaybe)

type Neqs = S.Set (Term, Term)

data ConstrainedUnifier =
  CUnifier  { unif :: U.Unifier
            , constraints :: Neqs }

instance Show ConstrainedUnifier where
  show (CUnifier u neqs) = show u ++ " s.t. " ++ intercalate ", " (map (\(a, b) -> show a ++ " \\= " ++ show b) $ S.toList neqs)

identity :: ConstrainedUnifier
identity = CUnifier U.identity S.empty

apply :: ConstrainedUnifier -> Term -> Term
apply (CUnifier u _) = U.apply u

addConstraint :: Term -> Term -> ConstrainedUnifier -> Maybe ConstrainedUnifier
addConstraint a b (CUnifier u neqs) =
  let a' = U.apply u a
      b' = U.apply u b
      new = fromMaybe [(a', b')] (simplifyNeq a' b')
  in  if any (uncurry equiv) new
      then Nothing
      else Just (CUnifier u (foldr S.insert neqs new))

extendUnifier :: Term -> Term -> ConstrainedUnifier -> Maybe ConstrainedUnifier
extendUnifier a b (CUnifier u neqs) = do
  u' <- U.extendUnifier a b u
  let neqs' = S.map (bimap (U.apply u') (U.apply u')) neqs
  guard (all (uncurry (/=)) (S.toList neqs'))
  return $ CUnifier u' neqs'

mergeUnifiers :: ConstrainedUnifier -> ConstrainedUnifier -> Maybe ConstrainedUnifier
mergeUnifiers keep (CUnifier (U.Unifier m) neqs) =
  let withNeqs = foldr (\(a, b) -> (>>= addConstraint a b)) (Just keep) neqs
  in foldr (\(a, b) -> (>>= extendUnifier a b)) withNeqs (M.toList m)

simplifyNeq :: Term -> Term -> Maybe [(Term, Term)]
simplifyNeq (TOp a argsa) (TOp b argsb)
  | a == b = Just $ fromMaybe [] (zipStrict argsa argsb)
  | otherwise = Just []
simplifyNeq (TVar _) _ = Nothing
simplifyNeq _ (TVar _) = Nothing
simplifyNeq TWildcard _ = Nothing
simplifyNeq _ TWildcard = Nothing
simplifyNeq (TAtomQuoted a) (TAtomQuoted b)
  | a /= b = Just []
  | otherwise = Nothing
simplifyNeq (TList [] Nothing) (TList (_:_) _) = Just []
simplifyNeq (TList (_:_) _) (TList [] Nothing) = Just []
-- TODO: Implement more reasoning on lists
simplifyNeq (TList _ _) (TList _ _) = Nothing
simplifyNeq (TString a) (TString b)
  | a /= b = Just []
  | otherwise = Nothing
simplifyNeq (TPred a argsa) (TPred b argsb)
  | a == b = Just $ fromMaybe [] (zipStrict argsa argsb)
  | otherwise = Just []
simplifyNeq _ _ = Just []
