{-# LANGUAGE ViewPatterns #-}
module RecursiveResolver
  ( Record
  , SList
  , QName
  , QType
  , Trace
  , nameInRData
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

nameInRData :: RData -> Maybe (Name String)
nameInRData (RData _ _ NS [d]) = parseNameM d
nameInRData (RData _ _ CNAME [d]) = parseNameM d
nameInRData (RData _ _ DNAME [d]) = parseNameM d
nameInRData (RData _ _ MX [_, d]) = parseNameM d
nameInRData _ = Nothing

nameInData :: Record a -> Maybe (Name String)
nameInData (Record _ d) = nameInRData d

type SList = [Name String]
type QName = Name String
type QType = Type
type Trace = [(Name String, Type)]

subtrace :: Ord a => [a] -> [a] -> Bool
subtrace (S.fromList -> s1) (S.fromList -> s2) = s1 `S.isProperSubsetOf` s2

resolve :: (Name String -> Name String -> [Record (Name String)]) -> SList -> QName -> QType -> NonDet Trace
resolve query ses qn qt = snd <$> rresolve [] ses qn qt
  where
    rresolve :: Trace -> SList -> QName -> QType -> NonDet (SList, Trace)
    rresolve trace slist qname qtype = do
      ns <- choose slist
      r@(Record n d) <- choose $ query ns qname
      let traceEl = (n, rtype d)
      guard (traceEl `notElem` trace)
      guard (qname `isSubDomain` n)
      let ref = fromMaybe undefined (nameInData r)
      let trace' = traceEl:trace
      case rtype d of
        t | n == qname && t == qtype -> return (slist, trace')
        NS -> do
          -- Non-deterministically choose to resolve the name server's A or AAAA record
          typ <- choose [ A, AAAA ]
          (slist', trace'') <- rresolve trace' slist ref typ
          rresolve trace'' (nub $ ref:slist') qname qtype
        CNAME | rname r == qname -> rresolve trace' slist ref qtype
        DNAME -> do
          let rewritten = dnameRewrite qname (rname r) ref
          rresolve trace' slist rewritten qtype
        _ -> NonDet.fail
