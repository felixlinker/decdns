module Util.Func
  ( (<.>)) where

(<.>) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(<.>) = (.) . (.)
