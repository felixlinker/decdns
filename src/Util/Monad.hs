module Util.Monad
  ( one
  , foreachM ) where

one :: Functor f => f a -> f [a]
one = fmap return

foreachM :: (Monad m, Traversable t) => t a -> (a -> m [b]) -> m [b]
foreachM t f = concat <$> mapM f t
