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
import ZoneFile
import Data.List (nub)
import qualified Data.Set as S

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
      let traceEl = (n, dtype $ rtype d)
      guard (traceEl `notElem` trace)
      guard (qname `isSubDomain` n)
      let trace' = traceEl:trace
      case rtype d of
        t | n == qname && dtype t == qtype -> return (slist, trace')
        RNS ref -> do
          -- Non-deterministically choose to resolve the name server's A or AAAA record
          typ <- choose [ A, AAAA ]
          (slist', trace'') <- rresolve trace' slist ref typ
          rresolve trace'' (nub $ ref:slist') qname qtype
        RCNAME ref | rname r == qname -> rresolve trace' slist ref qtype
        RDNAME ref -> do
          let rewritten = dnameRewrite qname (rname r) ref
          rresolve trace' slist rewritten qtype
        _ -> NonDet.fail
