module Resolvable.Prolog.Symbols
  ( caretSym
  , listSym
  , memberSym
  , neqSym
  , leqSym
  , negSym
  , resolvesSym
  , resolvableSym
  , adSym
  , rrsigValidSym
  , digestValidSym
  , keyTagSym ) where

caretSym :: String
caretSym = "^"

listSym :: String
listSym = "."

memberSym :: String
memberSym = "member"

neqSym :: String
neqSym = "\\="

leqSym :: String
leqSym = "=<"

negSym :: String
negSym = "\\+"

resolvesSym :: String
resolvesSym = "resolves"

resolvableSym :: String
resolvableSym = "resolvable"

adSym :: String
adSym = "ad"

rrsigValidSym :: String
rrsigValidSym = "rrsigValid"

digestValidSym :: String
digestValidSym = "digestValid"

keyTagSym :: String
keyTagSym = "keyTag"
