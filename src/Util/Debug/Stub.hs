module Util.Debug.Stub
  ( dbgWith
  , dbgMsg
  , dbg ) where

dbgWith :: (a -> String) -> a -> a
dbgWith _ = id

dbgMsg :: String -> a -> a
dbgMsg _ = id

dbg :: a -> a
dbg = id
