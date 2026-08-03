module DNS.Dig
  ( Opt(..)
  , Dig(..)
  , followUpQueries ) where

import Data.Maybe (mapMaybe)
import qualified Data.Set as S

import DNS.Name
import qualified DNS.RecordType as T
import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import Resolvable.Prolog.Reverse
import Util.Monad (maybeExcept)

data Opt  = Opt   String
          | OptNo String
          deriving (Eq, Ord)

instance Show Opt where
  show (Opt o) = '+':o
  show (OptNo o) = "+no" ++ o

data Dig =
  Dig { server :: Maybe (Name String)
      , qname :: Name String
      , qtype :: T.Type
      , options :: [Opt] }
      deriving (Eq, Ord)

instance Show Dig where
  show (Dig s qn qt os) = unwords $ filter (not . null)
    (   [ "dig"
        , maybe "" (('@':) . show) s
        , show qn
        , show qt ]
    ++  map show os )

opts :: [Opt]
opts = [ OptNo "recurse", Opt "authority" ]

followUpQueries :: S.Set (Rule a) -> S.Set Dig
followUpQueries = S.fromList . concatMap fromBody . S.toList
  where
    fromBody :: Rule a -> [Dig]
    fromBody = mapMaybe fromResolves . rbody

    fromResolves :: Term -> Maybe Dig
    fromResolves t = case toView t of
      TVResolves res -> maybeExcept $ do
        ns <- asName (resolvesNameServer res)
        qn <- asName (resolvesQueryType res)
        qt <- asTType (resolvesQueryName res)
        return $ dig ns qn qt
      _ -> Nothing

    dig :: Name String -> Name String -> T.Type -> Dig
    dig sn qn qt = Dig (Just sn) qn qt opts
