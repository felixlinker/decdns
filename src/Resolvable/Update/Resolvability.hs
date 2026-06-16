module Resolvable.Update.Resolvability
  ( NameServersConfig
  , compileConfigs
  , plRegressions ) where

import DNS.Name
import Resolvable.Prolog.Core
import qualified Resolvable.Prolog.Library as L
import Resolvable.Resolvability
import Control.Monad (void)
import Util.Fresh (Fresh, runFresh)

regression :: Rule (Maybe RComp)
regression = Rule Nothing (TFunctor "regression" [ ns, q, n ]) [ resolves_old, (\+) resolves_new ]
  where
    ns = TVar "NS"
    q = TVar "Q"
    n = TVar "N"

    resolves_old = renameFunctor "resolves" "resolves_old" $ L.resolves ns q n TWildcard (L.ad 0)
    resolves_new = renameFunctor "resolves" "resolves_new" $ L.resolves ns q n TWildcard (L.ad 0)

compileConfigs :: Fresh [Rule a] -> Fresh [Rule a] -> Fresh [Rule a]
compileConfigs old new = do
  old' <- map (renameFunctorInRule "resolves_old") <$> old
  new' <- map (renameFunctorInRule "resolves_new") <$> new
  return $ old' ++ new'
  where
    renameFunctorInRule :: String -> Rule a -> Rule a
    renameFunctorInRule = mapRule . renameFunctor resolvesName

    resolvesName :: String
    resolvesName = case L.resolves TWildcard TWildcard TWildcard TWildcard TWildcard of
      TFunctor n _ -> n
      _ -> undefined

plRegressions :: [Name String] -> NameServersConfig -> NameServersConfig -> [Rule (Maybe RComp)]
plRegressions rootServers old new = (regression :) $ runFresh $ compileConfigs (c old) (c new)
  where
    c :: NameServersConfig -> Fresh [Rule (Maybe RComp)]
    c = compileConfig rootServers
