module RecursiveResolver
  ( Record
  , resolve ) where

import qualified Data.List as L
import qualified Data.Set as S
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class (MonadIO(liftIO))

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
      (_, _) -> error "unreachable"

-- Returns some element for a non-empty foldable and Nothing otherwise.
choose :: Foldable t => t a -> Maybe a
choose = foldr ((<|>) . Just) Nothing

-- TODO: Model non-deterministic
query :: Name -> IO [Record]
query = error ""

resolve :: S.Set Name -> Name -> Type -> MaybeT IO Record
resolve slist qname qtype = do
  ns <- hoistMaybe $ choose slist
  rs <- liftIO $ query ns
  r <- hoistMaybe $ choose rs
  guard (rname r `L.isSuffixOf` qname)
  case rtype r of
    t | rname r == qname && t == qtype -> return r
    NS -> do
      _ <- resolve slist (nameInData r) A
      resolve (S.insert (nameInData r) slist) qname qtype
    CNAME -> resolve slist (nameInData r) qtype
    DNAME -> do
      let rewritten = take (length qname - length (rname r)) qname ++ nameInData r
      resolve slist rewritten qtype
    _ -> hoistMaybe Nothing
