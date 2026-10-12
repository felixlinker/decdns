module Resolvable.ResolutionSpec
  ( tests ) where


import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Control.Monad.Trans.Identity
import Data.Maybe (fromMaybe)

import qualified Resolvable.Constraint.Store as Store
import Resolvable.Prolog.Core
import Resolvable.Resolution
import Util.Fresh

stubResolution :: [Rule ()] -> Goal [Term] -> [Goal [Term]]
stubResolution kb = fromMaybe [] . (`evalFreshT` 0) . runIdentityT . resolution kb

goal :: Term -> Goal [Term]
goal = (`Goal` Store.empty) . (:[])

p :: Term -> Term
p = TFunctor "p" . (:[])

q :: Term -> Term
q = TFunctor "a" . (:[])

rules :: [Rule ()]
rules = Rule () (p x) [q x] : map (fact () . q) ints
  where
    x = TVar "x"

    ints :: [Term]
    ints = map TInt [0..2]

tests :: TestTree
tests =
  testGroup
    "Resolvable.Resolution"
    [ testCase "can resolve" $ stubResolution rules (goal (p $ TVar "x")) @?= [] ]
