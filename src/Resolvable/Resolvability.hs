{-# LANGUAGE ViewPatterns #-}
module Resolvable.Resolvability
  ( resolvable
  , fromZoneFile ) where

import NameConstraint
import Resolvable.Prolog as Pl
import ZoneFile

import Control.Applicative ((<|>))
import RecursiveResolver (nameInData)
import Data.Maybe (fromMaybe)
import Util.Fresh (fresh, runFresh)

resolves :: Term -> Term -> Term -> Term -> Term
resolves ns t n r = TPred "resolves" [ ns, t, n, r ]

resolvableBase :: Name String -> Record ConstrainedName -> Rule ()
resolvableBase ns (Record n (RData _ _ ty (unwords -> d))) = runFresh $ do
  (n', neqs) <- nameWithSynthesis n
  return (Rule () (resolves (nameTerm ns) (typeAtom ty) n' (TString d)) neqs)

resolvableStep :: Name String -> Name String -> Record ConstrainedName -> [Rule ()]
resolvableStep (nameTerm -> rootServer) (nameTerm -> ns) r@(Record n d) = runFresh $ do
  (n', neqs) <- nameWithSynthesis n
  t <- TVar <$> fresh "T"
  q <- TVar <$> fresh "Q"
  res <- TVar <$> fresh "R"
  let rules = case rtype d of
        NS -> let delegatedNs = nameTerm (fromMaybe undefined $ nameInData r)
                  n'' = mapListTail n' (Just t)
              in [ Rule { rannotation = ()
                        , rhead = resolves ns q n'' res
                        , rbody = [ resolves delegatedNs q n'' res
                                  , resolves rootServer (typeAtom A) delegatedNs plWildcard
                                  ] } ]
        CNAME ->
          let cname = nameTerm (fromMaybe undefined $ nameInData r)
          in [ Rule { rannotation = ()
                    , rhead = resolves ns q n' res
                    , rbody = [ resolves rootServer q cname res ] } ]
        A -> []
        TXT -> []
        _ -> undefined
  return (map (addNeqs neqs) rules)
  where
    addNeqs :: [Term] -> Rule () -> Rule ()
    addNeqs ps rule@(Rule _ _ b) = rule { rbody = ps ++ b }

    mapListTail :: Term -> Maybe Term -> Term
    mapListTail (TList pref listTail) defaultTail = TList pref (listTail <|> defaultTail)
    mapListTail _ _ = undefined

resolvable :: Name String -> Rule ()
resolvable (nameTerm -> rootServer) =
  Rule () (TPred "resolvable" [ q, ns ]) [ bagof n (r Pl.^ resolves rootServer q n r) ns ]
  where
    q = TVar "Q"
    ns = TVar "Names"
    n = TVar "Name"
    r = TVar "R"

    bagof :: Term -> Term -> Term -> Term
    bagof a b c = TPred "bagof" [ a, b, c ]

fromZoneFile :: Name String -> Name String -> Zone (NameConstraint String) -> [(Record ConstrainedName, [Rule ()])]
fromZoneFile rootServer ns (allRecords -> rs) = map (\r -> (r, recordToRules r)) rs
  where
    recordToRules :: Record ConstrainedName -> [Rule ()]
    recordToRules r = resolvableBase ns r : resolvableStep rootServer ns r
