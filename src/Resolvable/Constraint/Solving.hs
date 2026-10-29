module Resolvable.Constraint.Solving
  ( Solved(..)
  , simplifyDisj
  , getSolved
  , solve ) where

import Control.Monad (MonadPlus(..))
import Data.Foldable (toList)

data Solved a = Unsatisfiable | Satisfiable a
  deriving (Eq, Ord, Show)

instance Functor Solved where
  fmap _ Unsatisfiable = Unsatisfiable
  fmap f (Satisfiable a) = Satisfiable (f a)

instance Applicative Solved where
  pure = Satisfiable
  Unsatisfiable <*> _ = Unsatisfiable
  _ <*> Unsatisfiable = Unsatisfiable
  Satisfiable fs <*> Satisfiable as = Satisfiable (fs as)

simplifyDisj :: Foldable f => [Solved (f a)] -> [[a]]
simplifyDisj [] = []
simplifyDisj (Unsatisfiable:t) = simplifyDisj t
simplifyDisj (Satisfiable f:t)
  | null f = [[]]
  | otherwise = case simplifyDisj t of
    [[]] -> [[]]
    t' -> toList f:t'

getSolved :: MonadPlus m => Solved a -> m a
getSolved Unsatisfiable = mzero
getSolved (Satisfiable a) = return a

solve :: Foldable f => Solved (f a) -> Maybe Bool
solve Unsatisfiable = Just False
solve (Satisfiable fa) = if null fa then Just True else Nothing
