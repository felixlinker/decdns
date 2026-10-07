module Util.Except
  ( ExceptM
  , runExceptM
  , toExceptM ) where

import Control.Monad.Trans.Except
import Data.Functor.Identity (Identity(runIdentity))
import Data.Maybe (fromMaybe)

type ExceptM = ExceptT String Maybe

runExceptM :: ExceptM a -> Either String a
runExceptM = fromMaybe (Left "no data") . runExceptT

toExceptM :: Except String a -> ExceptM a
toExceptM = mapExceptT (pure . runIdentity)
