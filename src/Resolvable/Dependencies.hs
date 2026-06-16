{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
module Resolvable.Dependencies
  ( Dependency
  , matches
  , Dependencies
  , ConstrainedQuery(..)
  , fromRules
  , computeResolvable ) where

import qualified Data.Set as S
import Data.Maybe (mapMaybe)

import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import Resolvable.Prolog.Library
import Resolvable.Unification
import DNS.Name
import qualified Resolvable.ConstrainedUnification as CU
import qualified Util.PartialOrder as PO
import Data.List (uncons, partition)
import Control.Arrow (Arrow(first))

newtype Dependency = Dependency (Term, Term, Term, Term)
  deriving (Eq, Ord)

instance Show Dependency where
  show (Dependency trip) = show trip

matches :: Dependency -> Name String -> Name String -> Bool
matches (Dependency (nsT, nT, _, _)) ns n = unifies (list [nsT, nT]) (list [nameTerm ns, nameTerm n])

canon :: Term -> Term
canon = replace aux
  where
    aux :: Term -> Term
    aux (TVar _) = TWildcard
    aux t = t

fromResolves :: Term -> Maybe Dependency
fromResolves (toView -> TVResolves ns t n _ auth) = Just $ Dependency (canon ns, canon t, canon n, auth)
fromResolves _ = Nothing

fromRule :: Rule a -> Maybe Dependency
fromRule (Rule _ h _) = fromResolves h

type Dependencies = PO.PartialOrder Dependency

instance Show Dependencies where
  show po = unlines (map (\(l, r) -> show l ++ " < " ++ show r) (PO.toFlatList po))

match :: Rule a -> Rule b -> Maybe (Dependency, Dependency)
match (Rule _ h@(toView -> TVResolves {}) b) r@(Rule _ h'@(toView -> TVResolves {}) _) = do
  (u, _) <- uncons $ mapMaybe (`unify` h') b
  -- Check whether there are any inequalities that prevent unification immediately
  _ <- CU.applyRule (CU.fromUnifier u) r
  larger <- fromResolves h
  smaller <- fromResolves h'
  return (smaller, larger)
match _ _ = Nothing

fromRules :: [Rule a] -> Dependencies
fromRules rs = go rs PO.empty
  where
    go :: [Rule a] -> Dependencies -> Dependencies
    go [] acc = acc
    go (r:rrec) acc =
      let els = mapMaybe (match r) rs
      in go rrec (foldr (uncurry PO.insert . first S.singleton) acc els)

-- TODO: Refactor
data ConstrainedQuery = CQuery Term CU.Neqs

toRule :: ConstrainedQuery -> Rule ()
toRule (CQuery q (S.toList -> neqs)) = Rule () q (map (uncurry (\=)) neqs)

instance Show ConstrainedQuery where
  show = show . toRule

extendAndMerge :: CU.ConstrainedUnifier -> Term -> ConstrainedQuery -> Maybe CU.ConstrainedUnifier
extendAndMerge cu t (CQuery t' neqs) = do
  cu' <- CU.extendUnifier t t' cu
  foldr ((=<<) . uncurry CU.addConstraint) (Just cu') neqs

solveBody :: [ConstrainedQuery] -> Rule a -> [ConstrainedQuery]
solveBody rs (Rule _ h b) = map (\u -> CQuery (CU.apply u h) (CU.constraints u)) (rec [CU.identity] b)
  where
    rec :: [CU.ConstrainedUnifier] -> [Term] -> [CU.ConstrainedUnifier]
    rec choices [] = choices
    rec choices (t:recArg) = case toView t of
      TVNeq t1 t2 -> rec (mapMaybe (CU.addConstraint t1 t2) choices) recArg
      TVResolves {} ->
        let unified = concatMap (\u -> mapMaybe (extendAndMerge u t) rs) choices
        in rec unified recArg
      -- TODO:
      TVMember {} -> rec choices recArg
      -- This drops unsupported predicates, implicitly assuming that they are true
      _ -> rec choices recArg

-- TODO: Use Except monad transformer
computeResolvable :: [Rule a] -> Maybe [ConstrainedQuery]
computeResolvable (filter (isResolves . rhead) -> rs) = do
  ord <- PO.transitiveClosure $ fromRules rs
  return $ rec ord rs []
  where
    rec :: Dependencies -> [Rule a] -> [ConstrainedQuery] -> [ConstrainedQuery]
    rec _ [] acc = acc
    rec ord toSolve acc =
      let (mins, recArg) = partition minimal toSolve
      in  if null mins
            then error "non-termination"
            else rec (foldr (maybe id PO.remove . fromRule) ord mins) recArg (concatMap (solveBody acc) mins ++ acc)
      where
        minimal :: Rule a -> Bool
        minimal r = maybe False (S.null . (`PO.rel` ord)) (fromRule r)
