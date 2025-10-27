module RecursiveResolver
  ( Record
  , resolve ) where

import qualified Data.List as L
import qualified Data.Set as S
import Control.Applicative ((<|>))
import Control.Monad (guard)
import GHC.Base (failIO)

type Name = [String]
-- TODO: Integrate DNAME | MX | PTR | SOA | TXT
data Type = CNAME | DNAME | NS | A
  deriving (Eq, Ord)

data Record = Record
  { rname :: Name
  , rtype :: Type
  , rdata :: String }
  deriving (Eq, Ord)

nameInData :: Record -> Name
nameInData (Record _ A _) = []
nameInData (Record _ _ d) = parse d
  where
    parse :: String -> Name
    parse val = case span (/= '.') val of
      (label, "") -> [label]
      (label, '.':t) -> label : parse t
      (_, _) -> undefined

-- Returns some element for a non-empty foldable and Nothing otherwise.
choose :: Foldable t => t a -> IO a
choose = foldr ((<|>) . return) (failIO "empty sequence")

query :: Name -> IO Record
query = undefined

resolve :: S.Set Name -> Name -> Type -> IO [Record]
resolve = rresolve []

rresolve :: [Record] -> S.Set Name -> Name -> Type -> IO [Record]
rresolve trace slist qname qtype = do
  ns <- choose slist
  r <- query ns
  guard (rname r `L.isSuffixOf` qname)
  case rtype r of
    t | rname r == qname && t == qtype -> return (r:trace)
    NS -> do
      trace' <- rresolve trace slist (nameInData r) A
      rresolve trace' (S.insert (nameInData r) slist) qname qtype
    CNAME -> rresolve trace slist (nameInData r) qtype
    DNAME -> do
      let rewritten = take (length qname - length (rname r)) qname ++ nameInData r
      rresolve trace slist rewritten qtype
    _ -> return []
