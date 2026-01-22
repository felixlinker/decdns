{-# LANGUAGE ViewPatterns #-}
module Resolvable.Update.Resolvability
  ( NameServersConfig
  , compileConfigs
  , plRegressions ) where

import ZoneFile
import Resolvable.Prolog
import Resolvable.Resolvability
import Control.Monad (void)

(\+) :: Term -> Term
(\+) t1 = TPred "\\+" [t1]

regression :: Rule ()
regression = Rule () (TPred "regression" [ ns, q, n ]) [ resolves_old, (\+) resolves_new ]
  where
    ns = TVar "NS"
    q = TVar "Q"
    n = TVar "N"

    resolves_old = renamePred "resolves" "resolves_old" $ resolves ns q n TWildcard
    resolves_new = renamePred "resolves" "resolves_new" $ resolves ns q n TWildcard

compileConfigs :: [Name String] -> NameServersConfig -> NameServersConfig -> [Rule RComp]
compileConfigs rootServers (compileConfig rootServers -> old) (compileConfig rootServers -> new) =
  map (renamePredInRule "resolves_old") old ++ map (renamePredInRule "resolves_new") new
  where
    renamePredInRule :: String -> Rule RComp -> Rule RComp
    renamePredInRule = mapRule . renamePred resolvesName

    resolvesName :: String
    resolvesName = case resolves TWildcard TWildcard TWildcard TWildcard of
      TPred n _ -> n
      _ -> undefined

plRegressions :: [Name String] -> NameServersConfig -> NameServersConfig -> [Rule ()]
plRegressions rootServers old = (regression :) . map void . compileConfigs rootServers old
