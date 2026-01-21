{-# LANGUAGE ViewPatterns #-}
module Resolvable.Resolvability
  ( RComp(..)
  , resolvable
  , regression
  , fromZoneFile ) where

import NameConstraint
import Resolvable.Prolog as Pl
import ZoneFile

import RecursiveResolver (nameInData)
import Data.Maybe (fromMaybe)
import Util.Fresh (fresh, runFresh)

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

resolvableStep :: Name String -> Record ConstrainedName -> [Rule RComp]
resolvableStep ns r@(Record n d) = runFresh $ do
  (n', neqs) <- nameWithSynthesis n
  t <- TVar <$> fresh "T"
  q <- TVar <$> fresh "Q"
  res <- TVar <$> fresh "R"
  let nsTerm = nameTerm ns
  let ann = RComp ns r res
  let rules = case rtype d of
        NS -> let delegatedNs = nameTerm (fromMaybe undefined $ nameInData r)
                  n'' = setListTail n' t
                  resolvesHead = resolves nsTerm q n'' res
                  resolvesDeleg = resolves delegatedNs q n'' res
                  resolvesNSAddr typ = resolves TWildcard (typeAtom typ) delegatedNs TWildcard
              in  [ Rule  { rannotation = ann
                          , rhead = resolvesHead
                          , rbody = [ resolvesDeleg, resolvesNSAddr A ] }
                  , Rule  { rannotation = ann
                          , rhead = resolvesHead
                          , rbody = [ resolvesDeleg, resolvesNSAddr AAAA ] }]
        CNAME ->
          let cname = nameTerm (fromMaybe undefined $ nameInData r)
          in [ Rule { rannotation = ann
                    , rhead = resolves nsTerm q n' res
                    , rbody = [ resolves TWildcard q cname res ] } ]
        DNAME ->
          let n'' = setListTail n' t
              dname = nameTerm (fromMaybe undefined $ nameInData r)
              dname' = setListTail dname t
          in [ Rule { rannotation = ann
                    , rhead = resolves nsTerm q n'' res
                    , rbody = [ t \= TList [] Nothing
                              , resolves TWildcard q dname' res ] }]
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

fromZoneFile :: Name String -> Zone (NameConstraint String) -> [Rule RComp]
fromZoneFile ns (allRecords -> rs) = concatMap recordToRules rs
  where
    recordToRules :: Record ConstrainedName -> [Rule RComp]
    recordToRules r = resolvableBase ns r : resolvableStep ns r
