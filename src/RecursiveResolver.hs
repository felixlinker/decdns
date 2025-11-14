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

nameInData :: Record -> Name
nameInData (Record _ A _) = Name []
nameInData (Record _ _ d) = fromMaybe undefined (parseNameM d)

type SList = [Name]
type QName = Name
type QType = Type
type Trace = [(Name, Record)]

subtrace :: Eq a => [a] -> [a] -> Bool
subtrace = go False
  where
    go :: Eq a => Bool -> [a] -> [a] -> Bool
    go acc  []      []    = acc
    go _    []      (_:_) = True
    go _    (_:_)   []    = False
    go acc  (h1:t1) (h2:t2)
      -- Accumulator stores whether second list is known to be smaller than
      -- first list
      | h1 == h2 = go acc t1 t2
      | otherwise = go True (h1:t1) t2

resolve :: (Name -> [Record]) -> SList -> QName -> QType -> NonDet Trace
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
