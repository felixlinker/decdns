{-# LANGUAGE ViewPatterns #-}
module Query
  ( queryStep ) where

import Data.Maybe (mapMaybe)
import qualified Data.Set as S

import DNS.Name
import qualified DNS.RecordType as T
import Resolvable.Prolog.Core
import Resolvable.Dependencies
import qualified Resolvable.Unification as U
import Resolvable.Resolvability
import Util.Fresh

queryT :: Name String -> T.Type -> Fresh Term
queryT (nameTerm -> n) (ttypeTerm -> t) = do
  (ns, zone, d, v, a) <- freshVars5 ("NS", "Z", "D", "V", "A")
  return $ resolves ns t zone n d v a

queryStep :: Name String -> T.Type -> S.Set (Rule RComp) -> Fresh (S.Set (Rule RComp))
queryStep n t rs = do
  kb <- computeResolvable False rs
  q <- queryT n t
  undefined
  -- return $ S.fromList . mapMaybe (refine q) $ S.toList kb
  where
    refine :: Term -> Rule a -> Maybe (Rule a)
    refine q r@(Rule _ rh _) = fmap (`U.apply` r) (U.unify q rh)
