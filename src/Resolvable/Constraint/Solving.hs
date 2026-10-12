module Resolvable.Constraint.Solving
  ( Solved(..)
  , cup
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

cup :: (Foldable f1, Foldable f2) => Solved (f1 a) -> Solved (f2 a) -> Solved [a]
cup Unsatisfiable s = toList <$> s
cup s Unsatisfiable = toList <$> s
cup (Satisfiable fa) _ | null fa = Satisfiable []
cup _ (Satisfiable fa) | null fa = Satisfiable []
cup (Satisfiable b1) (Satisfiable b2) = Satisfiable $ toList b1 ++ toList b2

getSolved :: MonadPlus m => Solved a -> m a
getSolved Unsatisfiable = mzero
getSolved (Satisfiable a) = return a

solve :: Foldable f => Solved (f a) -> Maybe Bool
solve Unsatisfiable = Just False
solve (Satisfiable fa) = if null fa then Just True else Nothing
