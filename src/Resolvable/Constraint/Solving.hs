module Resolvable.Constraint.Solving
  ( Conjunction(..)
  , conjunction
  , Disjunction(..)
  , disjunction
  , simplifyDisj
  , simplifyConj
  , cnfToDnf
  , solve ) where

data Conjunction a = Bot | Conjunction a
  deriving (Eq, Ord, Show)

conjunction :: b -> (a -> b) -> Conjunction a -> b
conjunction def _ Bot = def
conjunction _ f (Conjunction a) = f a

instance Functor Conjunction where
  fmap _ Bot = Bot
  fmap f (Conjunction a) = Conjunction (f a)

instance Applicative Conjunction where
  pure = Conjunction
  Bot <*> _ = Bot
  _ <*> Bot = Bot
  Conjunction fs <*> Conjunction as = Conjunction (fs as)

instance Monad Conjunction where
  return = pure
  Bot >>= _ = Bot
  Conjunction as >>= f = f as

data Disjunction a = Top | Disjunction a
  deriving (Eq, Ord)

disjunction :: b -> (a -> b) -> Disjunction a -> b
disjunction def _ Top = def
disjunction _ f (Disjunction a) = f a

instance Functor Disjunction where
  fmap _ Top = Top
  fmap f (Disjunction a) = Disjunction (f a)

instance Applicative Disjunction where
  pure = Disjunction
  Top <*> _ = Top
  _ <*> Top = Top
  Disjunction fs <*> Disjunction as = Disjunction (fs as)

instance Monad Disjunction where
  return = pure
  Top >>= _ = Top
  Disjunction as >>= f = f as

simplifyDisj :: Foldable f => [Conjunction (f a)] -> Disjunction [f a]
simplifyDisj [] = Disjunction []
simplifyDisj (Bot:t) = simplifyDisj t
simplifyDisj (Conjunction f:t)
  | null f = Top
  | otherwise = do
    t' <- simplifyDisj t
    return $ f:t'

simplifyConj :: Foldable f => [Conjunction (f a)] -> Conjunction [f a]
simplifyConj [] = Conjunction []
simplifyConj (Bot:_) = Bot
simplifyConj (Conjunction f:t)
  | null f = simplifyConj t
  | otherwise = do
    t' <- simplifyConj t
    return $ f : t'

cnfToDnf :: [Disjunction [a]] -> [Conjunction [a]]
cnfToDnf = foldr (\as acc -> disjunction acc (concatMap (\a -> map (fmap (a:)) acc)) as) [Conjunction []]

solve :: Foldable f => Conjunction (f a) -> Maybe Bool
solve Bot = Just False
solve (Conjunction fa) = if null fa then Just True else Nothing
