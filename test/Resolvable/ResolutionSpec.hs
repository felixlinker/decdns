module Resolvable.ResolutionSpec
  ( tests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.Functor.Identity (runIdentity)
import qualified Data.Set as S

import Resolvable.Prolog.Core
import Resolvable.Resolution (goal, runSLD, sld)
import Resolvable.Unification (Unifier, apply)

stubSLD :: [Rule ()] -> Term -> [Unifier]
stubSLD kb g = fst $ runIdentity $ runSLD $ uncurry (sld kb) (goal g)

p :: Term -> Term
p = TFunctor "p" . (:[])

q :: Term -> Term
q = TFunctor "q" . (:[])

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
    [ testCase "returns all matching alternatives" $
        let g = p $ TVar "x"
            us = stubSLD rules g
        in S.fromList (map (`apply` g) us) @?= S.fromList (map (p . TInt) [0..2])
    ]
