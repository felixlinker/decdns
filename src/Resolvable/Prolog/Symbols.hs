module Resolvable.Prolog.Symbols
  ( caretSym
  , listSym
  , memberSym
  , neqSym
  , minSym
  , leqSym
  , negSym
  , resolvesSym
  , resolvableSym
  , adSym
  , isNameSym
  , rrsigValidSym
  , digestValidSym
  , nsecUnsignedSym
  , closestEncloserSym
  , nsec3UnsignedSym
  , keyTagSym ) where

caretSym :: String
caretSym = "^"

listSym :: String
listSym = "."

memberSym :: String
memberSym = "member"

neqSym :: String
neqSym = "dif"

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

isNameSym :: String
isNameSym = "isName"

rrsigValidSym :: String
rrsigValidSym = "rrsigValid"

digestValidSym :: String
digestValidSym = "digestValid"

nsecUnsignedSym :: String
nsecUnsignedSym = "nsecUnsigned"

closestEncloserSym :: String
closestEncloserSym = "closestEnclosers"

nsec3UnsignedSym :: String
nsec3UnsignedSym = "nsec3Unsigned"

keyTagSym :: String
keyTagSym = "keyTag"

minSym :: String
minSym = "min"
