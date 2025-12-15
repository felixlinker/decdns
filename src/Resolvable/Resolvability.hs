{-# LANGUAGE ViewPatterns #-}
module Resolvable.Resolvability
  ( RComp(..)
  , resolvable
  , fromZoneFile ) where

import NameConstraint
import Resolvable.Prolog as Pl
import ZoneFile

import Control.Applicative ((<|>))
import RecursiveResolver (nameInData)
import Data.Maybe (fromMaybe)
import Util.Fresh (fresh, runFresh)

data RComp = RComp
  { fromNS :: Name String
  , fromRecord :: Record ConstrainedName
  , resultTerm :: Term }

resolves :: Term -> Term -> Term -> Term -> Term
resolves ns t n r = TPred "resolves" [ ns, t, n, r ]

resolvableBase :: Name String -> Record ConstrainedName -> Rule RComp
resolvableBase ns r@(Record n (RData _ _ ty (TString . unwords -> d))) = runFresh $ do
  (n', neqs) <- nameWithSynthesis n
  let cannot = RComp ns r d
  return (Rule cannot (resolves (nameTerm ns) (typeAtom ty) n' d) neqs)

resolvableStep :: Name String -> Name String -> Record ConstrainedName -> [Rule RComp]
resolvableStep (nameTerm -> rootServer) ns r@(Record n d) = runFresh $ do
  (n', neqs) <- nameWithSynthesis n
  t <- fresh "T"
  q <- TVar <$> fresh "Q"
  res <- TVar <$> fresh "R"
  let nsTerm = nameTerm ns
  let cannot = RComp ns r res
  let rules = case rtype d of
        NS -> let delegatedNs = nameTerm (fromMaybe undefined $ nameInData r)
                  n'' = mapListTail n' (Just t)
              in [ Rule { rannotation = cannot
                        , rhead = resolves nsTerm q n'' res
                        , rbody = [ resolves delegatedNs q n'' res
                                  , resolves rootServer (typeAtom A) delegatedNs plWildcard
                                  ] } ]
        CNAME ->
          let cname = nameTerm (fromMaybe undefined $ nameInData r)
          in [ Rule { rannotation = cannot
                    , rhead = resolves nsTerm q n' res
                    , rbody = [ resolves rootServer q cname res ] } ]
        A -> []
        TXT -> []
        _ -> undefined
  return (map (addNeqs neqs) rules)
  where
    addNeqs :: [Term] -> Rule RComp -> Rule RComp
    addNeqs ps rule@(Rule _ _ b) = rule { rbody = ps ++ b }

    mapListTail :: Term -> Maybe String -> Term
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

fromZoneFile :: Name String -> Name String -> Zone (NameConstraint String) -> [(Record ConstrainedName, [Rule RComp])]
fromZoneFile rootServer ns (allRecords -> rs) = map (\r -> (r, recordToRules r)) rs
  where
    recordToRules :: Record ConstrainedName -> [Rule RComp]
    recordToRules r = resolvableBase ns r : resolvableStep rootServer ns r
