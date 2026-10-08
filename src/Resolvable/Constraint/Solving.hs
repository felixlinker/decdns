module Resolvable.Constraint.Solving
  ( Solved(..)
  , cup
  , getSolved
  , solve ) where

import Control.Monad (MonadPlus(..))

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

cup :: Solved [a] -> Solved [a] -> Solved [a]
cup Unsatisfiable s = s
cup s Unsatisfiable = s
cup s@(Satisfiable []) _ = s
cup _ s@(Satisfiable []) = s
cup (Satisfiable b1) (Satisfiable b2) = Satisfiable $ b1 ++ b2

getSolved :: MonadPlus m => Solved a -> m a
getSolved Unsatisfiable = mzero
getSolved (Satisfiable a) = return a

solve :: Foldable f => Solved (f a) -> Maybe Bool
solve Unsatisfiable = Just False
solve (Satisfiable fa) = if null fa then Just True else Nothing
