module Resolvable.Util
  ( genTerm ) where

import Test.Tasty.QuickCheck
import Resolvable.Prolog.Core (Term(..))
import Util.Fresh (runFresh, fresh)

names :: Gen String
names = elements (runFresh $ mapM (const $ fresh "v") ([0..4] :: [Int]))

-- Also generate lists but not other functors
functors :: Gen String
functors = oneof [ return ".", names ]

genTerm :: Gen Term
genTerm = do
  n <- chooseInt (0, 6)
  go n
  where
    go :: Int -> Gen Term
    go n
      | n <= 0 = oneof  [ TVar <$> names
                        , TAtom <$> names
                        , TAtomQuoted <$> names
                        , TString <$> names
                        ]
      | otherwise =
        let rec = go (n - 1)
        in  oneof [ go 0, TFunctor <$> functors <*> genList rec ]

    genList :: Gen Term -> Gen [Term]
    genList g = do
      k <- chooseInt (0, 3)
      vectorOf k g
