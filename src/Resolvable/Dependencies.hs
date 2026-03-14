{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module Resolvable.Dependencies
  ( ResolvableAt(..)
  , Dependencies(..)
  , ConstrainedQuery(..)
  , empty
  , leafs
  , roots
  , fromRules
  , computeResolvable ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (mapMaybe, maybeToList)

import NameConstraint
import DNS.Name
import Util.Fresh
import Resolvable.Prolog
import Resolvable.Resolvability
import Resolvable.ConstrainedUnification
import Resolvable.Unification (equivModuloRenaming)

newtype ResolvableAt a = ResolvableAt
  { rat :: Named String a }
  deriving (Eq, Ord)

instance Functor ResolvableAt where
  fmap f (ResolvableAt nd) = ResolvableAt $ fmap f nd

instance Show a => Show (ResolvableAt a) where
  show (rat -> Named q s) = show s ++ " @ " ++ show q

fromAnnot :: RComp -> ResolvableAt ConstrainedName
fromAnnot (RComp ns (Named n _)) = ResolvableAt (Named ns n)

newtype Dependencies a = Deps
  { deps :: M.Map (ResolvableAt a) (S.Set (ResolvableAt a)) }

instance Show a => Show (Dependencies a) where
  show (Deps (M.toList -> items)) = unlines $ map (uncurry showItem . fmap S.toList) items
    where
      showItem :: Show a => ResolvableAt a -> [ResolvableAt a] -> String
      showItem r [] = "No dependencies: " ++ show r
      showItem r ds = unlines ((show r ++ " depends on:") : map (("  " ++) . show) ds)

empty :: Dependencies a
empty = Deps M.empty

leafs :: Ord a => Dependencies a -> S.Set (ResolvableAt a)
leafs (Deps d) = S.fromAscList $ map fst $ filter (null . snd) $ M.toAscList d

roots :: Ord a => Dependencies a -> S.Set (ResolvableAt a)
roots (Deps d) = S.filter (not . (`S.member` img)) (M.keysSet d)
  where
    img = S.unions $ M.elems d

dinsert :: RComp -> [RComp] -> Dependencies ConstrainedName -> Dependencies ConstrainedName
dinsert k (S.fromList . map fromAnnot -> vs) (Deps m) = Deps (M.alter (Just . maybe vs (vs <>)) (fromAnnot k) m)

fromRules :: [Rule RComp] -> Dependencies ConstrainedName
fromRules rs = go rs empty
  where
    go :: [Rule RComp] -> Dependencies ConstrainedName -> Dependencies ConstrainedName
    go [] acc = acc
    go (r:rrec) acc = go rrec (dinsert (rannotation r) (map rannotation $ getRel r) acc)

    getRel :: Rule RComp -> [Rule RComp]
    getRel (Rule _ _ []) = []
    getRel (Rule _ _ body) = concat $ matchRules identity body

    matchRules :: ConstrainedUnifier -> [Term] -> [[Rule RComp]]
    matchRules _ [] = [[]]
    matchRules u ((TPred "\\=" [ a, b ]):ts) = do
      u' <- maybeToList $ addConstraint a b u
      matchRules u' ts
    matchRules u (t@(TPred "resolves" _):ts) = do
      (u', h) <- mapMaybe (matchRule u t) rs
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
  return (u', b)

computeResolvable :: [Name String] -> Fresh [Rule a] -> Fresh [ConstrainedQuery]
computeResolvable rootServers rsM = do
  rs <- rsM
  qtyp <- TVar <$> fresh "QT"
  qname <- TVar <$> fresh "QN"
  return $ do
    rootServer <- nameTerm <$> rootServers
    let query = presolvable rootServer qtyp qname
    u <- rec [] identity query rs
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
          rec trace' u'' q' rs
