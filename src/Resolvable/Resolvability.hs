{-# LANGUAGE ViewPatterns #-}
module Resolvable.Resolvability
  ( RComp(..)
  , resolves
  , fromZoneFile
  , NameServersConfig
  , compileConfig
  , plResolvable ) where

import NameConstraint
import Resolvable.Prolog as Pl
import ZoneFile

import RecursiveResolver (nameInData)
import Data.Maybe (fromMaybe)
import Util.Fresh (fresh, runFresh)
import Control.Monad (void)

data RComp = RComp
  { fromNS :: Name String
  , fromRecord :: Record ConstrainedName
  , resultTerm :: Term }
  deriving (Eq, Ord)

resolves :: Term -> Term -> Term -> Term -> Term
resolves ns t n r = TPred "resolves" [ ns, t, n, r ]

resolvableBase :: Name String -> Record ConstrainedName -> Rule RComp
resolvableBase ns r@(Record n (RData _ _ ty (TString . unwords -> d))) = runFresh $ do
  (n', neqs) <- nameWithSynthesis n
  let cannot = RComp ns r d
  return (Rule cannot (resolves (nameTerm ns) (typeAtom ty) n' d) neqs)

resolvableStep :: [Name String] -> Name String -> Record ConstrainedName -> [Rule RComp]
resolvableStep (map nameTerm -> rootServers) ns r@(Record n d) = runFresh $ do
  (n', neqs) <- nameWithSynthesis n
  t <- TVar <$> fresh "T"
  q <- TVar <$> fresh "Q"
  res <- TVar <$> fresh "R"
  let nsTerm = nameTerm ns
  let ann = RComp ns r res
  let rules = case rtype d of
        NS -> do
          rootServer <- rootServers
          glueType <- typeAtom <$> [A, AAAA]
          let delegatedNs = nameTerm (fromMaybe undefined $ nameInData r)
          let n'' = setListTail n' t
          return $ Rule { rannotation = ann
                        , rhead = resolves nsTerm q n'' res
                        , rbody = [ resolves delegatedNs q n'' res
                                  , resolves rootServer glueType delegatedNs TWildcard ] }
        CNAME -> do
          rootServer <- rootServers
          let cname = nameTerm (fromMaybe undefined $ nameInData r)
          return $ Rule { rannotation = ann
                        , rhead = resolves nsTerm q n' res
                        , rbody = [ resolves rootServer q cname res ] }
        DNAME -> do
          rootServer <- rootServers
          let n'' = setListTail n' t
          let dname = nameTerm (fromMaybe undefined $ nameInData r)
          let rewritten = setListTail dname t
          return $ Rule { rannotation = ann
                        , rhead = resolves nsTerm q n'' res
                        , rbody = [ t \= TList [] Nothing
                                  , resolves rootServer q rewritten res ] }
        _ -> []
  return (map (addNeqs neqs) rules)
  where
    addNeqs :: [Term] -> Rule RComp -> Rule RComp
    addNeqs ps rule@(Rule _ _ b) = rule { rbody = ps ++ b }

    setListTail :: Term -> Term -> Term
    setListTail (TList pref Nothing) t = TList pref (Just t)
    setListTail (TList _ (Just _)) _ = undefined
    setListTail _ _ = undefined

resolvable :: Rule ()
resolvable =
  Rule () (TPred "resolvable" [ q, rns, ns ]) [ bagof n (r Pl.^ resolves rns q n r) ns ]
  where
    q = TVar "Q"
    rns = TVar "RS"
    ns = TVar "Names"
    n = TVar "Name"
    r = TVar "R"

    bagof :: Term -> Term -> Term -> Term
    bagof a b c = TPred "bagof" [ a, b, c ]

fromZoneFile :: [Name String] -> Name String -> Zone (NameConstraint String) -> [Rule RComp]
fromZoneFile rootServers ns (allRecords -> rs) = concatMap recordToRules rs
  where
    recordToRules :: Record ConstrainedName -> [Rule RComp]
    recordToRules r = resolvableBase ns r : resolvableStep rootServers ns r

type NameServersConfig = [(Name String, [Zone (NameConstraint String)])]

compileConfig :: [Name String] -> NameServersConfig -> [Rule RComp]
compileConfig rootServers = concatMap (uncurry compileOne)
  where
    compileOne :: Name String -> [Zone (NameConstraint String)] -> [Rule RComp]
    compileOne ns = concatMap (fromZoneFile rootServers ns)

plResolvable :: [Name String] -> NameServersConfig -> [Rule ()]
plResolvable rootServers = (resolvable :) . map void . compileConfig rootServers
