{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
module Resolvable.Dependencies
  ( ResolvableAt(..)
  , Dependencies
  , ConstrainedQuery(..)
  , fromRules
  , computeResolvable ) where

import Data.Functor (($>))
import Control.Monad ((<=<), guard)
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe, fromMaybe)

import NameConstraint
import DNS.Name
import Util.Fresh
import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import qualified Resolvable.Prolog.Library as Lib
import Resolvable.Resolvability
import Resolvable.ConstrainedUnification
import Resolvable.Unification (equivModuloRenaming)
import qualified Util.PartialOrder as PO

newtype ResolvableAt a = ResolvableAt
  { rat :: Named String a }
  deriving (Eq, Ord)

instance Functor ResolvableAt where
  fmap f (ResolvableAt nd) = ResolvableAt $ fmap f nd

instance Show a => Show (ResolvableAt a) where
  show (rat -> Named q s) = show s ++ " @ " ++ show q

progressing :: ResolvableAt ConstrainedName -> ResolvableAt ConstrainedName -> Maybe (ResolvableAt ConstrainedName)
progressing (rat -> Named ns r) k@(rat -> Named ns' r') =
  -- TODO: isSubDomain might not behave correctly on wildcards
  guard (not (isSubDomain r' r) || ns /= ns') $> k

fromAtom :: Term -> Maybe String
fromAtom (TAtom s) = Just s
fromAtom _ = Nothing

fromNameTerm :: Term -> Maybe (Name String)
fromNameTerm t = case toView t of
  TVList _ _ -> do
    let l = collectList t
    l' <- mapM fromAtom l
    return $ Name $ NE.reverse $ "" NE.:| l'
  _ -> Nothing

fromResolves :: Term -> Maybe (ResolvableAt ConstrainedName)
fromResolves (toView -> TVResolves ns _ n _ _) = do
  ns' <- fromNameTerm ns
  n' <- exactly <$> fromNameTerm n
  return $ ResolvableAt (Named ns' n')
fromResolves _ = Nothing

type Dependencies a = PO.PartialOrder (ResolvableAt a)

instance Show a => Show (Dependencies a) where
  show (PO.toList -> items) = unlines $ map (uncurry showItem . fmap S.toList) items
    where
      showItem :: Show a => ResolvableAt a -> [ResolvableAt a] -> String
      showItem r [] = "No dependencies: " ++ show r
      showItem r ds = unlines ((show r ++ " depends on:") : map (("  " ++) . show) ds)

-- TODO: Handle wildcards and DNAME
fromRules :: [Rule RComp] -> Dependencies ConstrainedName
fromRules rs = go rs PO.empty
  where
    go :: [Rule RComp] -> Dependencies ConstrainedName -> Dependencies ConstrainedName
    go [] acc = acc
    go (r:rrec) acc = fromMaybe acc $ do
      h <- fromResolves $ rhead r
      -- TODO: The guard is for dropping redundant NS records, but I think this is too general
      let vs = S.fromList $ mapMaybe (progressing h <=< fromResolves) (rbody r)
      return $ go rrec (PO.insert h vs acc)

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

-- TODO: Refactor return type so that it returns rules. Resolvable queries are
-- rules with an empty body, i.e., facts, and non-empty bodies indicate missing
-- knowledge.
computeResolvable :: [Name String] -> Fresh [Rule a] -> Fresh [ConstrainedQuery]
computeResolvable rootServers rsM = do
  rs <- rsM
  qtyp <- TVar <$> fresh "QT"
  qname <- TVar <$> fresh "QN"
  return $ do
    rootServer <- Lib.nameTerm <$> rootServers
    let query = Lib.resolvable rootServer qtyp qname
    u <- rec [] identity query rs
    return $ CQuery (apply u query) (constraints u)
  where
    solveQuery :: ConstrainedUnifier -> Term -> [Rule a] -> [(ConstrainedUnifier, [Term])]
    solveQuery u q rs = case toView q of
      TVNeq a b -> maybe [] (return . (,[])) (addConstraint a b u)
      TVResolves {} -> mapMaybe (deriveIf u q) rs
      TVResolvable {} -> mapMaybe (deriveIf u q) rs
      _ -> error "unsupported predicate"

    rec :: [Term] -> ConstrainedUnifier -> Term -> [Rule a] -> [ConstrainedUnifier]
    rec trace u q rs
      -- TODO: Check soundness of loop detection
      | any (equivModuloRenaming q) trace = []
      | otherwise = do
        (u', conj) <- solveQuery u q rs
        foldr solveConjunct [u'] conj
      where
        trace' :: [Term]
        trace' = case toView q of
          TVResolves {} -> q:trace
          _ -> trace

        solveConjunct :: Term -> [ConstrainedUnifier] -> [ConstrainedUnifier]
        solveConjunct q' us = do
          u'' <- us
          rec trace' u'' q' rs
