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

import DNS.Name
import Util.Fresh
import Resolvable.Prolog
import Resolvable.Resolvability
import Resolvable.ConstrainedUnification
import Data.List (intercalate, partition)
import Resolvable.Unification (equivModuloRenaming)

type Conjunct = (ConstrainedUnifier, [Rule RComp])

newtype Dependencies = Deps (M.Map (Rule RComp) [Conjunct])

instance Show Dependencies where
  show (Deps (M.toList -> items)) =
    let (facts, rules) = partition (null . snd) items
    in unlines  $ if null facts
                    then []
                    else  ( "The following rules have no dependencies:"
                          : map (("- " ++) . showRecord . fst) facts
                          ++ [""])
                ++ map (uncurry showItem) rules
    where
      showRecord :: Rule RComp -> String
      showRecord (Rule (RComp _ record) _ _) = show record

      showItem :: Rule RComp -> [(ConstrainedUnifier, [Rule RComp])] -> String
      showItem r ds = unlines ((showRecord r ++ " depends on:") : map showDep ds)

      showDep :: (ConstrainedUnifier, [Rule RComp]) -> String
      showDep (cu, rs) = "- " ++ intercalate " AND " (map showRecord rs) ++ " under " ++ show cu

empty :: Dependencies
empty = Deps M.empty

dinsert :: Rule RComp -> [(ConstrainedUnifier, [Rule RComp])] -> Dependencies -> Dependencies
dinsert k v (Deps m) = Deps (M.insert k v m)

-- TODO: Fix this; probably wrong
fromRules :: [Rule RComp] -> Dependencies
fromRules rs = go rs empty
  where
    go :: [Rule RComp] -> Dependencies -> Dependencies
    go [] acc = acc
    go (r:rrec) acc = go rrec (dinsert r (getRel r) acc)

    getRel :: Rule RComp -> [(ConstrainedUnifier, [Rule RComp])]
    getRel (Rule _ _ []) = []
    getRel (Rule _ _ body) = matchRules identity body

    matchRules :: ConstrainedUnifier -> [Term] -> [(ConstrainedUnifier, [Rule RComp])]
    matchRules u [] = [(u, [])]
    matchRules u ((TPred "\\=" [ a, b ]):ts) = do
      u' <- maybeToList $ addConstraint a b u
      matchRules u' ts
    matchRules u (t@(TPred "resolves" _):ts) = do
      (u', h) <- mapMaybe (matchRule u t) rs
      (u'', hs) <- matchRules u' ts
      return (u'', h:hs)
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
  qresult <- TVar <$> fresh "QR"
  return $ do
    rootServer <- nameTerm <$> rootServers
    let query = resolves rootServer qtyp qname qresult
    u <- rec [] identity (resolves rootServer qtyp qname qresult) rs
    return $ CQuery (apply u query) (constraints u)
  where
    solveQuery :: ConstrainedUnifier -> Term -> [Rule a] -> [(ConstrainedUnifier, [Term])]
    solveQuery u (TPred "\\=" [ a, b ]) _ = maybe [] (return . (,[])) (addConstraint a b u)
    solveQuery u q@(TPred "resolves" _) rs = mapMaybe (deriveIf u q) rs
    solveQuery _ _ _ = error "unsupported predicate"

    rec :: [Term] -> ConstrainedUnifier -> Term -> [Rule a] -> [ConstrainedUnifier]
    rec trace u q rs
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
