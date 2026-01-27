module Util.Debug
  ( dbgWith
  , dbgMsg
  , dbg ) where

import Debug.Trace (trace)

dbgWith :: (a -> String) -> a -> a
dbgWith f a = trace (f a) a

dbgMsg :: Show a => String -> a -> a
dbgMsg m = dbgWith (((m ++ " ") ++) . show)

dbg :: Show a => a -> a
dbg = dbgWith show
