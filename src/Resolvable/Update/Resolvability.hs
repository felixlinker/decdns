module Resolvable.Update.Resolvability
  ( NameServersConfig
  , compileConfigs ) where

import qualified Data.Set as S

import Resolvable.Prolog.Core
import Resolvable.Prolog.Symbols
import Resolvable.Resolvability
import Util.Fresh (Fresh)

compileConfigs :: Ord a => Fresh (S.Set (Rule a)) -> Fresh (S.Set (Rule a)) -> Fresh (S.Set (Rule a))
compileConfigs old new = do
  old' <- S.map (renameFunctorInRule "resolves_old") <$> old
  new' <- S.map (renameFunctorInRule "resolves_new") <$> new
  return $ old' <> new'
  where
    renameFunctorInRule :: String -> Rule a -> Rule a
    renameFunctorInRule = mapRule . renameFunctor resolvesSym
