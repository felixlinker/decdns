{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module Resolvable.Dependencies
  ( Dependencies
  , ConstrainedQuery(..)
  , empty
  , fromRules
  , computeResolvable ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (mapMaybe, maybeToList)

import NameConstraint
import DNS.Record
import DNS.Name
import Util.Fresh
import Resolvable.Prolog
import Resolvable.Resolvability
import Resolvable.ConstrainedUnification
import Resolvable.Unification (equivModuloRenaming)

newtype Dependencies = Deps
  { deps :: M.Map StandardRecord (S.Set StandardRecord) }

instance Show Dependencies where
  show (Deps (M.toList -> items)) = unlines $ map (uncurry showItem . fmap S.toList) items
    where
      showItem :: Show a => Record a -> [Record a] -> String
      showItem r [] = "No dependencies: " ++ show r
      showItem r ds = unlines ((show r ++ " depends on:") : map (("  " ++) . show) ds)

empty :: Dependencies
empty = Deps M.empty

dinsert :: StandardRecord -> S.Set StandardRecord -> Dependencies -> Dependencies
dinsert k v (Deps m) = Deps (M.insert k v m)

fromRules :: [Rule RComp] -> Dependencies
fromRules rs = go rs empty
  where
    cannot :: Rule RComp -> ConstrainedRecord
    cannot = fromRecord . rannotation

    go :: [Rule RComp] -> Dependencies -> Dependencies
    go [] acc = acc
    go (r:rrec) acc = go rrec (dinsert (toStandard $ cannot r) (S.fromList $ getRel r) acc)

    getRel :: Rule RComp -> [StandardRecord]
    getRel (Rule _ _ []) = []
    getRel (Rule _ _ body) = map toStandard $ concat $ matchRules identity body

    matchRules :: ConstrainedUnifier -> [Term] -> [[ConstrainedRecord]]
    matchRules _ [] = [[]]
    matchRules u ((TPred "\\=" [ a, b ]):ts) = do
      u' <- maybeToList $ addConstraint a b u
      matchRules u' ts
    matchRules u (t@(TPred "resolves" _):ts) = do
      (u', cannot -> h) <- mapMaybe (matchRule u t) rs
      rec <- matchRules u' ts
      return $ h:rec
    matchRules _ _ = error "unsupported rule body predicate"

    matchRule :: ConstrainedUnifier -> Term -> Rule RComp -> Maybe (ConstrainedUnifier, Rule RComp)
    matchRule u t r = (,r) <$> extendUnifier t (rhead r) u

-- TODO: Implement equality based on equivalence modulo renaming
data ConstrainedQuery = CQuery Term Neqs

toRule :: ConstrainedQuery -> Rule ()
toRule (CQuery q (S.toList -> neqs)) = Rule () q (map (uncurry (\=)) neqs)

instance Show ConstrainedQuery where
  show = show . toRule

deriveIf :: ConstrainedUnifier -> Term -> Rule a -> Maybe (ConstrainedUnifier, [Term])
deriveIf u q (Rule _ h b) = do
  u' <- extendUnifier q h u
  return (u', map (apply u') b)

computeResolvable :: [Name String] -> Fresh [Rule a] -> Fresh [ConstrainedQuery]
computeResolvable rootServers rsM = do
  rs <- rsM
  qtyp <- TVar <$> fresh "QT"
  qname <- TVar <$> fresh "QN"
  return $ do
    rootServer <- nameTerm <$> rootServers
    let query = presolvable rootServer qtyp qname
    u <- rec [] identity (presolvable rootServer qtyp qname) rs
    return $ CQuery (apply u query) (constraints u)
  where
    solveQuery :: ConstrainedUnifier -> Term -> [Rule a] -> [(ConstrainedUnifier, [Term])]
    solveQuery u (TPred "\\=" [ a, b ]) _ = maybe [] (return . (,[])) (addConstraint a b u)
    solveQuery u q@(TPred pname _) rs
      | pname `elem` ["resolves", "resolvable"] = mapMaybe (deriveIf u q) rs
    solveQuery _ _ _ = error "unsupported predicate"

    rec :: [Term] -> ConstrainedUnifier -> Term -> [Rule a] -> [ConstrainedUnifier]
    rec trace u q rs
      -- TODO: Check soundness of loop detection
      | any (equivModuloRenaming q) trace = []
      | otherwise = do
        (u', conj) <- solveQuery u q rs
        foldr solveConjunct [u'] conj
      where
        trace' :: [Term]
        trace' = case q of
          TPred "resolves" _ -> q:trace
          _ -> trace

        solveConjunct :: Term -> [ConstrainedUnifier] -> [ConstrainedUnifier]
        solveConjunct q' us = do
          u'' <- us
          rec trace' u'' (apply u'' q') rs
