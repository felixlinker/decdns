module Util.List
  ( zipWithStrict
  , zipStrict ) where

zipWithStrict :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
zipWithStrict f (x:xs) (y:ys) = (f x y :) <$> zipWithStrict f xs ys
zipWithStrict _ [] [] = Just []
zipWithStrict _ _ _ = Nothing

zipStrict :: [a] -> [b] -> Maybe [(a, b)]
zipStrict = zipWithStrict (,)
