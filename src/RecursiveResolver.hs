module RecursiveResolver
  ( Record
  , resolve ) where

import qualified Data.Set as S
import Control.Applicative ((<|>),asum)
import Data.Functor (($>))
import Control.Monad (guard)

type NAME = String
-- TODO: Integrate DNAME | MX | PTR | SOA | TXT
data TYPE = CNAME | NS | A
  deriving (Eq, Ord)

data Record = Record
  { name :: NAME
  , rtype :: TYPE
  , rdata :: String }
  deriving (Eq, Ord)

data NameServer = NameServer NAME (Maybe [Record])
  deriving (Eq, Ord)

-- Returns some element for a non-empty foldable and Nothing otherwise.
choose :: Foldable t => t a -> Maybe a
choose = foldr ((<|>) . Just) Nothing

query :: NAME -> NameServer
query = error ""

-- TODO: This model assumes that name servers are static, i.e., they return all
-- responses at once. This is not realistic.
resolve :: S.Set NameServer -> NAME -> TYPE -> Maybe [Record]
resolve slist qname qtype = do
  s@(NameServer _ mrs) <- choose slist
  rs <- mrs <|> resolve (S.delete s slist) qname qtype
  let ars = [ r | r@(Record rn rt _) <- rs, rn == qname, rt == qtype ]
  asum  [ guard (not (null ars)) $> ars
        , followNS rs
        , rewriteCNAME rs ]
  where
    followNS :: [Record] -> Maybe [Record]
    followNS rs = do
      Record _ _ ns  <- choose [ r | r@(Record rname NS _) <- rs, rname == qname ]
      -- model that we can only query a nameserver if we know its address
      _ <- resolve slist ns A
      resolve (S.insert (query ns) slist) qname qtype

    rewriteCNAME :: [Record] -> Maybe [Record]
    rewriteCNAME rs = do
      Record _ _ newQ <- choose [ r | r@(Record rname CNAME _) <- rs, rname == qname ]
      resolve slist newQ qtype
