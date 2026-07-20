{-# LANGUAGE ViewPatterns #-}
module Query
  ( queryStep ) where

import Control.Monad.Trans.Except (ExceptT)
import Data.Maybe (catMaybes)
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
  (ns, d, r, v, a) <- freshVars5 ("NS", "D", "REC", "V", "A")
  return $ resolves ns n t d r v a

queryStep :: (Monad m) => Name String -> T.Type -> Fresh (S.Set (Rule (Maybe RComp))) -> ExceptT String m (S.Set (Rule (Maybe RComp)))
queryStep n t rs =
  let kb = fmap (S.fromList . catMaybes) . mapM refine . S.toList =<< rs
  in  computeResolvable $ runFresh kb
  where
    q :: Fresh Term
    q = queryT n t

    refine :: Rule a -> Fresh (Maybe (Rule a))
    refine r@(Rule _ rh _) = do
      fq <- q
      let u = U.unify fq rh
      return $ fmap (`U.apply` r) u
