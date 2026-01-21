{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module Resolvable.Dependencies
  ( Dependencies
  , empty
  , fromRules ) where

import qualified Data.Map as M
import Data.Maybe (mapMaybe, maybeToList)

import Resolvable.Prolog
import Resolvable.Resolvability
import Resolvable.ConstrainedUnification
import Data.List (intercalate)

newtype Dependencies = Deps (M.Map (Rule RComp) [(ConstrainedUnifier, [Rule RComp])])

instance Show Dependencies where
  show (Deps (M.toList -> items)) =
      unlines $ map (uncurry showItem)
              $ filter (not . null . snd) items
    where
      showRecord :: Rule RComp -> String
      showRecord (Rule (RComp _ record _) _ _) = show record

      showItem :: Rule RComp -> [(ConstrainedUnifier, [Rule RComp])] -> String
      showItem r ds = unlines ((showRecord r ++ " depends on:") : map showDep ds)

      showDep :: (ConstrainedUnifier, [Rule RComp]) -> String
      showDep (_, rs) = "- " ++ intercalate " AND " (map showRecord rs)

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
