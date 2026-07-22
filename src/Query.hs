{-# LANGUAGE ViewPatterns #-}
module Query
  ( queryStep ) where

import Control.Monad.Trans.Except (ExceptT)
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
  (ns, d, r, v, a) <- freshVars5 ("NS", "D", "REC", "V", "A")
  return $ resolves ns n t d r v a

queryStep :: (Monad m) => Name String -> T.Type -> Fresh (S.Set (Rule (Maybe RComp))) -> ExceptT String m (S.Set (Rule (Maybe RComp)))
queryStep n t rs =
  let (kb, q) = runFresh $ do
        kb' <- computeResolvable False <$> rs
        q' <- queryT n t
        return (kb', q')
  in S.fromList . mapMaybe (refine q) . S.toList <$> kb
  where
    refine :: Term -> Rule a -> Maybe (Rule a)
    refine q r@(Rule _ rh _) = fmap (`U.apply` r) (U.unify q rh)
