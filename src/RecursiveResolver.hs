{-# LANGUAGE ViewPatterns #-}
module RecursiveResolver
  ( Record
  , SList
  , QName
  , QType
  , Trace
  , subtrace
  , resolve ) where

import Control.Monad (guard)

import NonDet
import DNS.Name
import DNS.Record
import qualified DNS.RecordType as T
import Data.List (nub)
import qualified Data.Set as S

type SList = [Name String]
type QName = Name String
type QType = T.Type
type Trace = [(Name String, T.Type)]

subtrace :: Ord a => [a] -> [a] -> Bool
subtrace (S.fromList -> s1) (S.fromList -> s2) = s1 `S.isProperSubsetOf` s2

resolve :: (Name String -> Name String -> [StandardRecord]) -> SList -> QName -> QType -> NonDet Trace
resolve query ses qn qt = snd <$> rresolve [] ses qn qt
  where
    rresolve :: Trace -> SList -> QName -> QType -> NonDet (SList, Trace)
    rresolve trace slist qname qtype = do
      ns <- choose slist
      r@(Record n d) <- choose $ query ns qname
      let traceEl = (n, toType $ rtype d)
      guard (traceEl `notElem` trace)
      guard (qname `isSubDomain` n)
      let trace' = traceEl:trace
      case rtype d of
        t | n == qname && toType t == qtype -> return (slist, trace')
        NS ref -> do
          -- Non-deterministically choose to resolve the name server's A or AAAA record
          typ <- choose [ T.A, T.AAAA ]
          (slist', trace'') <- rresolve trace' slist ref typ
          rresolve trace'' (nub $ ref:slist') qname qtype
        CNAME ref | rname r == qname -> rresolve trace' slist ref qtype
        DNAME ref -> do
          let rewritten = dnameRewrite qname (rname r) ref
          rresolve trace' slist rewritten qtype
        _ -> NonDet.fail
