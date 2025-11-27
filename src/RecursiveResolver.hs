{-# LANGUAGE ViewPatterns #-}
module RecursiveResolver
  ( Record
  , SList
  , QName
  , QType
  , Trace
  , nameInData
  , subtrace
  , resolve ) where

import Control.Monad (guard)

import NonDet
import ZoneFile
import Data.Maybe (fromMaybe)
import Data.List (nub)
import qualified Data.Set as S
import Parsing.Name (parseNameM)
import NameConstraint (NameConstraint, match)

nameInData :: Record a -> Maybe (Name String)
nameInData (Record _ (RData _ _ NS [d])) = parseNameM d
nameInData (Record _ (RData _ _ CNAME [d])) = parseNameM d
nameInData (Record _ (RData _ _ DNAME [d])) = parseNameM d
nameInData (Record _ (RData _ _ MX [_, d])) = parseNameM d
nameInData Record {} = Nothing

type SList = [Name String]
type QName = Name String
type QType = Type
type Trace = [(Name String, Record String)]

subtrace :: Ord a => [a] -> [a] -> Bool
subtrace (S.fromList -> s1) (S.fromList -> s2) = s1 `S.isProperSubsetOf` s2

resolve :: (Name String -> [Record (NameConstraint String)]) -> SList -> QName -> QType -> NonDet Trace
resolve query = rresolve []
  where
    rresolve :: Trace -> SList -> QName -> QType -> NonDet Trace
    rresolve trace slist qname qtype = do
      ns <- choose slist
      r@(Record nc d) <- choose $ query ns
      n <- liftFoldable $ match qname nc
      let synthesized = Record n d
      guard ((ns, synthesized) `notElem` trace)
      let ref = fromMaybe undefined (nameInData r)
      let trace' = (ns, synthesized):trace
      case rtype d of
        t | n == qname && t == qtype -> return trace'
        NS -> do
          trace'' <- rresolve trace' slist ref A
          rresolve trace'' (nub $ ref:slist) qname qtype
        CNAME | rname synthesized == qname -> rresolve trace' slist ref qtype
        DNAME -> do
          let rewritten = dnameRewrite qname (rname synthesized) ref
          rresolve trace' slist rewritten qtype
        _ -> NonDet.fail
