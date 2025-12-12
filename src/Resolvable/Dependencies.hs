{-# LANGUAGE ViewPatterns #-}
module Resolvable.Dependencies
  ( resolvable
  , fromZoneFile ) where

import NameConstraint
import Resolvable.Prolog as Pl
import ZoneFile

import Control.Applicative ((<|>))
import RecursiveResolver (nameInData)
import Data.Maybe (fromMaybe)
import Util.Fresh (fresh, runFresh, Fresh)

resolves :: Term -> Term -> Term -> Term
resolves q d0 d1 = TPred "resolves" [ q, d0, d1 ]

recordTerm :: Name String -> Record ConstrainedName -> Term
recordTerm (nameTerm -> ns) (Record (nameTerm . toName -> n) (RData _ _ (typeAtom -> ty) _)) =
  TList [ ns, ty, n ] Nothing

resolvableBase :: Name String -> Record ConstrainedName -> Rule
resolvableBase ns r@(Record n (RData _ _ (typeAtom -> ty) _)) = runFresh $ do
  (n', neqs) <- nameWithSynthesis n
  let rTerm = recordTerm ns r
  let q = TList [ nameTerm ns, ty, n' ] Nothing
  return (Rule (resolves q rTerm rTerm) neqs)

resolvableStep :: Name String -> Name String -> Record ConstrainedName -> [Rule]
resolvableStep (nameTerm -> rootServer) ns r@(Record n d) = runFresh $ do
  (n', neqs) <- nameWithSynthesis n
  t <- TVar <$> fresh "T"
  q <- TVar <$> fresh "Q"
  d0 <- TVar <$> fresh "D"
  -- d1 <- TVar <$> fresh "D"
  let rTerm = recordTerm ns r
  let nsTerm = nameTerm ns
  let rules = case rtype d of
        NS -> let delegatedNs = nameTerm (fromMaybe undefined $ nameInData r)
                  n'' = mapListTail n' (Just t)
                  query = TList [ nsTerm, q, n'' ] Nothing
                  recQuery = TList [ delegatedNs, q, n'' ] Nothing
                  aQuery = TList [ rootServer, typeAtom A, delegatedNs ] Nothing
              in  [ Rule  { rhead = resolves query rTerm d0
                          , rbody = [ resolves recQuery d0 plWildcard
                                    , resolves aQuery plWildcard plWildcard
                                    ] }
                  , Rule  { rhead = resolves query rTerm d0
                          , rbody = [ resolves recQuery plWildcard plWildcard
                                    , resolves aQuery d0 plWildcard
                                    ] } ]
        CNAME ->
          let cname = nameTerm (fromMaybe undefined $ nameInData r)
              query = TList [ nsTerm, q, n' ] Nothing
              recQuery = TList [ rootServer, q, cname ] Nothing
          in [ Rule { rhead = resolves query rTerm d0
                    , rbody = [ resolves recQuery d0 plWildcard ] } ]
        A -> []
        TXT -> []
        _ -> undefined
  return (map (addNeqs neqs) rules)
  where
    addNeqs :: [Term] -> Rule -> Rule
    addNeqs ps rule@(Rule _ b) = rule { rbody = ps ++ b }

    mapListTail :: Term -> Maybe Term -> Term
    mapListTail (TList pref listTail) defaultTail = TList pref (listTail <|> defaultTail)
    mapListTail _ _ = undefined

resolvable :: Name String -> Rule
resolvable (nameTerm -> rootServer) = undefined
  -- Rule (TPred "resolvable" [ q, ns ]) [ bagof n (r Pl.^ resolves rootServer q n r) ns ]
  -- where
  --   q = TVar "Q"
  --   ns = TVar "Names"
  --   n = TVar "Name"
  --   r = TVar "R"

  --   bagof :: Term -> Term -> Term -> Term
  --   bagof a b c = TPred "bagof" [ a, b, c ]

fromZoneFile :: Name String -> Name String -> Zone (NameConstraint String) -> [(Record ConstrainedName, [Rule])]
fromZoneFile rootServer ns (allRecords -> rs) = map (\r -> (r, recordToRules r)) rs
  where
    recordToRules :: Record ConstrainedName -> [Rule]
    recordToRules r = resolvableBase ns r : resolvableStep rootServer ns r
