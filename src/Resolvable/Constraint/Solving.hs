module Resolvable.Constraint.Solving
  ( Solved(..)
  , solved
  , simplifyDisj
  , simplifyDisjList
  , simplifyConj
  , cnfToDnf
  , getSolved
  , solve ) where

import Control.Monad (MonadPlus(..))
import Data.Foldable (toList)

data Solved a = Unsatisfiable | Satisfiable a
  deriving (Eq, Ord, Show)

solved :: b -> (a -> b) -> Solved a -> b
solved def _ Unsatisfiable = def
solved _ f (Satisfiable a) = f a

instance Functor Solved where
  fmap _ Unsatisfiable = Unsatisfiable
  fmap f (Satisfiable a) = Satisfiable (f a)

instance Applicative Solved where
  pure = Satisfiable
  Unsatisfiable <*> _ = Unsatisfiable
  _ <*> Unsatisfiable = Unsatisfiable
  Satisfiable fs <*> Satisfiable as = Satisfiable (fs as)

instance Monad Solved where
  return = pure
  Unsatisfiable >>= _ = Unsatisfiable
  Satisfiable as >>= f = f as

simplifyDisj :: Foldable f => [Solved (f a)] -> [f a]
simplifyDisj [] = []
simplifyDisj (Unsatisfiable:t) = simplifyDisj t
simplifyDisj (Satisfiable f:t)
  | null f = [f]
  | otherwise = case simplifyDisj t of
    [fRec] | null fRec -> [fRec]
    t' -> f:t'

simplifyDisjList :: Foldable f => [Solved (f a)] -> [[a]]
simplifyDisjList = map toList . simplifyDisj

simplifyConj :: Foldable f => [Solved (f a)] -> Solved [f a]
simplifyConj [] = Satisfiable []
simplifyConj (Unsatisfiable:_) = Unsatisfiable
simplifyConj (Satisfiable f:t)
  | null f = simplifyConj t
  | otherwise = do
    t' <- simplifyConj t
    return $ f : t'

cnfToDnf :: [[a]] -> [[a]]
cnfToDnf = foldr (\as acc -> concatMap (\a -> map (a:) acc) as) [[]]

getSolved :: MonadPlus m => Solved a -> m a
getSolved Unsatisfiable = mzero
getSolved (Satisfiable a) = return a

solve :: Foldable f => Solved (f a) -> Maybe Bool
solve Unsatisfiable = Just False
solve (Satisfiable fa) = if null fa then Just True else Nothing
