module NonDet
  ( NonDet
  , some
  , NonDet.all
  , chooseFoldable
  , choose
  , liftFoldable
  , NonDet.fail ) where

import qualified Data.List as L
import Control.Applicative (Alternative(empty,(<|>)))
import Data.Foldable (Foldable(toList))

newtype NonDet a = NonDet { source :: [a] }

instance Functor NonDet where
  fmap f (NonDet as) = NonDet (fmap f as)

instance Applicative NonDet where
  pure a = NonDet [a]
  NonDet fas <*> NonDet as = NonDet (fas <*> as)

instance Monad NonDet where
  NonDet as >>= fm = NonDet [ b | a <- as, b <- source (fm a) ]

instance Alternative NonDet where
  empty = NonDet []
  NonDet as1 <|> NonDet as2 = NonDet (as1 <|> as2)

some :: NonDet a -> Maybe a
some (NonDet la) = fst <$> L.uncons la

all :: NonDet a -> [a]
all (NonDet la) = la

chooseFoldable :: Foldable t => t a -> NonDet a
chooseFoldable = NonDet . foldr (:) []

choose :: [a] -> NonDet a
choose = NonDet

liftFoldable :: Foldable f => f a -> NonDet a
liftFoldable = NonDet . toList

fail :: NonDet a
fail = NonDet []
