module Resolvable.Update.Resolvability
  ( NameServersConfig
  , compileConfigs
  , plRegressions ) where

import DNS.Name
import Resolvable.Prolog
import Resolvable.Resolvability
import Control.Monad (void)
import Util.Fresh (Fresh, runFresh)

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

compileConfigs :: Fresh [Rule a] -> Fresh [Rule a] -> Fresh [Rule a]
compileConfigs old new = do
  old' <- map (renamePredInRule "resolves_old") <$> old
  new' <- map (renamePredInRule "resolves_new") <$> new
  return $ old' ++ new'
  where
    renamePredInRule :: String -> Rule a -> Rule a
    renamePredInRule = mapRule . renamePred resolvesName

    resolvesName :: String
    resolvesName = case resolves TWildcard TWildcard TWildcard TWildcard of
      TPred n _ -> n
      _ -> undefined

plRegressions :: [Name String] -> NameServersConfig -> NameServersConfig -> [Rule ()]
plRegressions rootServers old new = (regression :) $ map void $ runFresh $ compileConfigs (c old) (c new)
  where
    c :: NameServersConfig -> Fresh [Rule RComp]
    c = compileConfig rootServers
