{-# LANGUAGE ViewPatterns #-}
module Resolvable.Update.Trivial
  ( partitionTrivial ) where

import qualified Data.List as L
import qualified Data.Set as S

import DNS.Record
import Resolvable.Dependencies
import Resolvable.Prolog
import qualified Resolvable.Unification as U
import qualified Resolvable.ConstrainedUnification as C

import Resolvable.Resolvability (presolvable)
import Data.Maybe (isJust)
import NameConstraint (ConstrainedName)
import Util.Fresh

matchesQuery :: ConstrainedQuery -> Term -> C.Neqs -> Bool
matchesQuery (CQuery t neqs) t' neqs' = isJust $ do
  cu <- C.extendUnifier t t' (C.CUnifier U.identity neqs)
  foldr (\(n1, n2) -> (C.addConstraint n1 n2 =<<)) (Just cu) neqs'

partitionTrivial :: [Record ConstrainedName] -> [Record ConstrainedName] -> Fresh [ConstrainedQuery] -> Fresh ([ConstrainedQuery], [ConstrainedQuery])
partitionTrivial old new fqs = do
  qs <- fqs
  old' <- cOld
  new' <- cNew
  return $ L.partition (isTrivial old' new') qs
  where
    cOld :: Fresh [(Term, C.Neqs)]
    cOld = mapM cResolvable old

    cNew :: Fresh [(Term, C.Neqs)]
    cNew = mapM cResolvable new

    cResolvable :: Record ConstrainedName -> Fresh (Term, C.Neqs)
    cResolvable (Record cn d) = do
      (n, S.fromList -> neqs) <- nameWithSynthesis cn
      return (presolvable TWildcard (typeAtom (rtype d)) n, neqs)

    isTrivial :: [(Term, C.Neqs)] -> [(Term, C.Neqs)] -> ConstrainedQuery -> Bool
    isTrivial old' new' q = any (uncurry (matchesQuery q)) old' && not (any (uncurry (matchesQuery q)) new')
