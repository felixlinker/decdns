module Util.Functor
  ( split ) where

split :: Functor f => (a -> (b, c)) -> f a -> (f b, f c)
split f as =
  let as' = fmap f as
  in (fmap fst as', fmap snd as')
