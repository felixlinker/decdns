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
import Data.Maybe (fromMaybe)
import Data.List (nub)
import qualified Data.Set as S

nameInData :: Record String -> Name String
nameInData (Record _ _ _ NS [d]) = fromMaybe undefined (parseNameM d)
nameInData (Record _ _ _ CNAME [d]) = fromMaybe undefined (parseNameM d)
nameInData (Record _ _ _ DNAME [d]) = fromMaybe undefined (parseNameM d)
nameInData (Record _ _ _ MX [_, d]) = fromMaybe undefined (parseNameM d)
nameInData Record {} = Name []

type SList = [Name String]
type QName = Name String
type QType = Type
type Trace = [(Name String, Record String)]

subtrace :: Ord a => [a] -> [a] -> Bool
subtrace (S.fromList -> s1) (S.fromList -> s2) = s1 `S.isProperSubsetOf` s2

resolve :: (Name String -> [Record String]) -> SList -> QName -> QType -> NonDet Trace
resolve query = rresolve []
  where
    rresolve :: Trace -> SList -> QName -> QType -> NonDet Trace
    rresolve trace slist qname qtype = do
      ns <- choose slist
      r <- choose $ query ns
      guard ((ns, r) `notElem` trace)
      guard (isSubDomain qname (rname r))
      let trace' = (ns, r):trace
      case rtype r of
        t | rname r == qname && t == qtype -> return trace'
        NS -> do
          trace'' <- rresolve trace' slist (nameInData r) A
          rresolve trace'' (nub $ nameInData r:slist) qname qtype
        CNAME | rname r == qname -> rresolve trace' slist (nameInData r) qtype
        DNAME -> do
          let rewritten = dnameRewrite qname (rname r) (nameInData r)
          rresolve trace' slist rewritten qtype
        _ -> NonDet.fail
