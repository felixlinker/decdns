module Resolvable.Update.Resolvability
  ( NameServersConfig
  , compileConfigs
  , plRegressions ) where

import qualified Data.Set as S

import DNS.Name
import Resolvable.Prolog.Core
import Resolvable.Prolog.Symbols
import Resolvable.Resolvability
import Util.Fresh (Fresh, runFresh)

regression :: Rule (Maybe RComp)
regression = Rule Nothing (TFunctor "regression" [ ns, q, n ]) [ resolves_old, (\+) resolves_new ]
  where
    ns = TVar "NS"
    q = TVar "Q"
    n = TVar "N"

    resolves_old = renameFunctor "resolves" "resolves_old" $ resolves ns q n TWildcard TWildcard TWildcard (ad 0)
    resolves_new = renameFunctor "resolves" "resolves_new" $ resolves ns q n TWildcard TWildcard TWildcard (ad 0)

compileConfigs :: Ord a => Fresh (S.Set (Rule a)) -> Fresh (S.Set (Rule a)) -> Fresh (S.Set (Rule a))
compileConfigs old new = do
  old' <- S.map (renameFunctorInRule "resolves_old") <$> old
  new' <- S.map (renameFunctorInRule "resolves_new") <$> new
  return $ old' <> new'
  where
    renameFunctorInRule :: String -> Rule a -> Rule a
    renameFunctorInRule = mapRule . renameFunctor resolvesSym

plRegressions :: [Name String] -> NameServersConfig -> NameServersConfig -> S.Set (Rule (Maybe RComp))
plRegressions rootServers old new = S.insert regression $ runFresh $ compileConfigs (c old) (c new)
  where
    c :: NameServersConfig -> Fresh (S.Set (Rule (Maybe RComp)))
    c = compileConfig rootServers
