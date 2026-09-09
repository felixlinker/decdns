module Util.List
  ( nonEmpty
  , zipWithStrict
  , zipStrict ) where

import qualified Data.List.NonEmpty as NE

nonEmpty :: Foldable t => t a -> Maybe (NE.NonEmpty a)
nonEmpty = foldr (\a -> Just . maybe (a NE.:| []) (a NE.<|)) Nothing

zipWithStrict :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
zipWithStrict f (x:xs) (y:ys) = (f x y :) <$> zipWithStrict f xs ys
zipWithStrict _ [] [] = Just []
zipWithStrict _ _ _ = Nothing

zipStrict :: [a] -> [b] -> Maybe [(a, b)]
zipStrict = zipWithStrict (,)
