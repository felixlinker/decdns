module Util.Bool
  ( (==>)
  , bool
  , Or(..)
  , And(..) ) where

(==>) :: Bool -> Bool -> Bool
a ==> b = not a || b

class Boolean t where
  bool :: t -> Bool

newtype Or = Or Bool

instance Boolean Or where
  bool (Or b) = b

instance Semigroup Or where
  Or True <> _ = Or True
  _ <> o = o

instance Monoid Or where
  mempty = Or False

newtype And = And Bool

instance Boolean And where
  bool (And b) = b

instance Semigroup And where
  And False <> _ = And False
  _ <> a = a

instance Monoid And where
  mempty = And True
