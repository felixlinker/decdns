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

regression :: Fresh (Rule (Maybe RComp))
regression = do
  (ns, q, n) <- freshVars3 ("NS", "Q", "N")
  (wc1, wc2, wc3, wc4, wc5, wc6) <- wildcards6
  let resolves_old = renameFunctor "resolves" "resolves_old" $ resolves ns q n wc1 wc2 wc3 (ad 0)
  let resolves_new = renameFunctor "resolves" "resolves_new" $ resolves ns q n wc4 wc5 wc6 (ad 0)
  return $ Rule Nothing (TFunctor "regression" [ ns, q, n ]) [ resolves_old, (\+) resolves_new ]

compileConfigs :: Ord a => Fresh (S.Set (Rule a)) -> Fresh (S.Set (Rule a)) -> Fresh (S.Set (Rule a))
compileConfigs old new = do
  old' <- S.map (renameFunctorInRule "resolves_old") <$> old
  new' <- S.map (renameFunctorInRule "resolves_new") <$> new
  return $ old' <> new'
  where
    renameFunctorInRule :: String -> Rule a -> Rule a
    renameFunctorInRule = mapRule . renameFunctor resolvesSym

plRegressions :: [Name String] -> NameServersConfig -> NameServersConfig -> S.Set (Rule (Maybe RComp))
plRegressions rootServers old new = runFresh $ do
  reg <- regression
  rs <- compileConfigs (c old) (c new)
  return $ S.insert reg rs
  where
    c :: NameServersConfig -> Fresh (S.Set (Rule (Maybe RComp)))
    c = compileConfig rootServers
