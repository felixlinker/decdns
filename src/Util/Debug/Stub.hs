module Util.Debug.Stub
  ( dbgWith
  , dbgMsg
  , dbg ) where

dbgWith :: (a -> String) -> a -> a
dbgWith _ = id

dbgMsg :: Show a => String -> a -> a
dbgMsg _ = id

dbg :: Show a => a -> a
dbg = id
