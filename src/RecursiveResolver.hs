module RecursiveResolver
  ( Record
  , resolve ) where

import Control.Monad (guard)

import NonDet
import ZoneFile
import Data.Maybe (fromMaybe)
import Data.List (nub)

nameInData :: Record -> Name
nameInData (Record _ A _) = Name []
nameInData (Record _ _ d) = fromMaybe undefined (parseNameM d)

resolve :: (Name -> [Record]) -> [Name] -> Name -> Type -> NonDet [(Name, Record)]
resolve query = rresolve []
  where
    rresolve :: [(Name, Record)] -> [Name] -> Name -> Type -> NonDet [(Name, Record)]
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
        CNAME -> rresolve trace' slist (nameInData r) qtype
        DNAME -> do
          let rewritten = dnameRewrite qname (rname r) (nameInData r)
          rresolve trace' slist rewritten qtype
        _ -> NonDet.fail
