{-# LANGUAGE ScopedTypeVariables #-}
module Util.List
  ( nonEmpty
  , zipWithStrict
  , zipStrict
  , foldrWith
  , snub ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

nonEmpty :: Foldable t => t a -> Maybe (NE.NonEmpty a)
nonEmpty = foldr (\a -> Just . maybe (a NE.:| []) (a NE.<|)) Nothing

zipWithStrict :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
zipWithStrict f (x:xs) (y:ys) = (f x y :) <$> zipWithStrict f xs ys
zipWithStrict _ [] [] = Just []
zipWithStrict _ _ _ = Nothing

zipStrict :: [a] -> [b] -> Maybe [(a, b)]
zipStrict = zipWithStrict (,)

foldrWith :: forall a b t. Foldable t => (a -> b -> (a, b)) -> b -> t a -> ([a], b)
foldrWith f acc = foldr aux ([], acc)
  where
    aux :: a -> ([a], b) -> ([a], b)
    aux a (accL, accB) =
      let (a', accB') = f a accB
      in (a':accL, accB')

snub :: Ord a => [a] -> [a]
snub = S.toList . S.fromList
