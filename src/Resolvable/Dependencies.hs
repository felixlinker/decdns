{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module Resolvable.Dependencies
  ( Dependencies
  , empty
  , fromRules
  , computeResolvable ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (mapMaybe, maybeToList)

import ZoneFile
import Util.Fresh
import Resolvable.Prolog
import Resolvable.Resolvability
import Resolvable.ConstrainedUnification
import Data.List (intercalate, partition)

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

data ConstrainedQuery = CQuery Term Neqs

instance Show ConstrainedQuery where
  show (CQuery q neqs) =
    let showNeqs = if null neqs then "True" else intercalate " and " (map (\(a, b) -> show a ++ "\\=" ++ show b) (S.toList neqs))
    in show q ++ " s.t. " ++ showNeqs

deriveIf :: ConstrainedUnifier -> Term -> Rule a -> Maybe (ConstrainedUnifier, [Term])
deriveIf u q (Rule _ h b) = do
  u' <- extendUnifier q h u
  return (u', map (apply u') b)

computeResolvable :: [Name String] -> Fresh [Rule a] -> Fresh [ConstrainedQuery]
computeResolvable rootServers rsM = do
  rs <- rsM
  qtype <- TVar <$> fresh "QT"
  qname <- TVar <$> fresh "QN"
  qresult <- TVar <$> fresh "QR"
  return $ do
    rootServer <- nameTerm <$> rootServers
    let query = resolves rootServer qtype qname qresult
    u <- rec identity (resolves rootServer qtype qname qresult) rs
    return $ CQuery (apply u query) (constraints u)
  where
    solveQuery :: ConstrainedUnifier -> Term -> [Rule a] -> [(ConstrainedUnifier, [Term])]
    solveQuery u (TPred "\\=" [ a, b ]) _ = maybe [] (return . (,[])) (addConstraint a b u)
    solveQuery u q@(TPred "resolves" _) rs = mapMaybe (deriveIf u q) rs
    solveQuery _ _ _ = error "unsupported predicate"

    rec :: ConstrainedUnifier -> Term -> [Rule a] -> [ConstrainedUnifier]
    rec u q rs = do
      (u', conj) <- solveQuery u q rs
      foldr (\q' us -> [ us' | u'' <- us, us' <- rec u'' (apply u'' q') rs ]) [u'] conj
