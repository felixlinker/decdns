module RecursiveResolver
  ( Record
  , resolve ) where

import qualified Data.Set as S
import Control.Monad (guard)

import NonDet
import ZoneFile
import Data.Maybe (fromMaybe)

nameInData :: Record -> Name
nameInData (Record _ A _) = Name []
nameInData (Record _ _ d) = fromMaybe undefined (parseNameM d)

type Zones = Name -> NonDet Record

resolve :: Zones -> S.Set Name -> Name -> Type -> NonDet [(Name, Record)]
resolve = rresolve []

rresolve :: [(Name, Record)] -> Zones -> S.Set Name -> Name -> Type -> NonDet [(Name, Record)]
rresolve trace zones slist qname qtype = do
  ns <- choose slist
  r <- zones ns
  guard ((ns, r) `notElem` trace)
  guard (isSubDomain qname (rname r))
  let trace' = (ns, r):trace
  case rtype r of
    t | rname r == qname && t == qtype -> return trace'
    NS -> do
      trace'' <- rresolve trace' zones slist (nameInData r) A
      rresolve trace'' zones (S.insert (nameInData r) slist) qname qtype
    CNAME -> rresolve trace' zones slist (nameInData r) qtype
    DNAME -> do
      let rewritten = dnameRewrite qname (rname r) (nameInData r)
      rresolve trace' zones slist rewritten qtype
    _ -> NonDet.fail
