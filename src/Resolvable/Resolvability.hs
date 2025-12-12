{-# LANGUAGE ViewPatterns #-}
module Resolvable.Resolvability
  ( fromZoneFile ) where

import NameConstraint
import Resolvable.Prolog
import ZoneFile

import Control.Applicative ((<|>))
import RecursiveResolver (nameInData)
import Data.Maybe (fromMaybe)
import Util.Fresh (fresh, runFresh)

resolvableBase :: Name String -> Record ConstrainedName -> Rule
resolvableBase ns (Record n (RData _ _ ty _)) = runFresh $ do
  (n', neqs) <- nameWithSynthesis n
  return (Rule (resolvable (nameTerm ns) (typeAtom ty) n') neqs)

resolvableStep :: Name String -> Name String -> Record ConstrainedName -> [Rule]
resolvableStep (nameTerm -> rootServer) (nameTerm -> ns) r@(Record n d) = runFresh $ do
  (n', neqs) <- nameWithSynthesis n
  t <- TVar <$> fresh "T"
  q <- TVar <$> fresh "Q"
  let rules = case rtype d of
        NS -> let delegatedNs = nameTerm (fromMaybe undefined $ nameInData r)
                  n'' = mapListTail n' (Just t)
              in [ Rule { rhead = resolvable ns q n''
                        , rbody = [ resolvable delegatedNs q n''
                                  , resolvable rootServer (typeAtom A) delegatedNs
                                  ] } ]
        CNAME ->
          let cname = nameTerm (fromMaybe undefined $ nameInData r)
          in [ Rule { rhead = resolvable ns q n'
                    , rbody = [ resolvable rootServer q cname ] } ]
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

fromZoneFile :: Name String -> Name String -> Zone (NameConstraint String) -> [(Record ConstrainedName, [Rule])]
fromZoneFile rootServer ns (allRecords -> rs) = map (\r -> (r, recordToRules r)) rs
  where
    recordToRules :: Record ConstrainedName -> [Rule]
    recordToRules r = resolvableBase ns r : resolvableStep rootServer ns r
