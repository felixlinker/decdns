{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
module Resolvable.Prolog.TermView
  ( Resolves(..)
  , TermView(..)
  , toView
  , fromView
  , isResolves
  , isNeq ) where

import DNS.Record (Type)
import Resolvable.Prolog.Symbols
import Resolvable.Prolog.Core
import qualified Util.PartialOrder as PO

data Resolves = Resolves
  { resolvesNS :: Term
  , resolvesQType :: Term
  , resolvesQName :: Term
  , resolvesData :: Term
  , resolvesRecurse :: Term
  , resolvesVerify :: Term
  , resolvesAD :: Term
  }
  deriving (Eq, Ord)

instance Show Resolves where
  show (Resolves ns qtype qname dat rec ver adb) = show $ resolves ns qtype qname dat rec ver adb

instance Show (PO.PartialOrder Resolves) where
  show po = unlines (map (\(l, r) -> show l ++ " < " ++ show r) (PO.toFlatList po))

data TermView = TVList Term Term
              | TVMember Term Term
              | TVNeq Term Term
              | TVLeq Term Term
              | TVResolves Resolves
              | TVResolvable Term Term Term Term Term
              | TVRRSIGValid Term Term Term Term
              | TVDigestValid Term Term Term
              | TVNSECUnsigned Term Term
              | TVClosestEncloser Term Term Term Term Term
              | TVNSEC3Unsigned Term Term Term
              | TVKeyTag Term Term
              | TVAd Bool
              | TVFunctor String [ Term ]
              | TVEmptyList
              | TVVar String
              | TVAtomQuoted String
              | TVAtom String
              | TVString String
              | TVInt Int
              | TVComplex Type

toView :: Term -> TermView
toView (TFunctor n args)
  | n == listSym = case args of
    [ a, b ] -> TVList a b
    _ -> undefined
  | n == memberSym = case args of
    [ a, b ] -> TVMember a b
    _ -> undefined
  | n == neqSym = case args of
    [ a, b ] -> TVNeq a b
    _ -> undefined
  | n == leqSym = case args of
    [ a, b ] -> TVLeq a b
    _ -> undefined
  | n == resolvesSym = case args of
    [ ns, qtype, qname, d, rec, ver, adb ] -> TVResolves $ Resolves ns qtype qname d rec ver adb
    _ -> undefined
  | n == resolvableSym = case args of
    [ ns, qtype, qname, ver, auth ] -> TVResolvable ns qtype qname ver auth
    _ -> undefined
  | n == rrsigValidSym = case args of
    [ nam, dat, sig, key ] -> TVRRSIGValid nam dat sig key
    _ -> undefined
  | n == digestValidSym = case args of
    [ nam, dsTerm, dnskeyTerm ] -> TVDigestValid nam dsTerm dnskeyTerm
    _ -> undefined
  | n == nsecUnsignedSym = case args of
    [ nam, nsecDat ] -> TVNSECUnsigned nam nsecDat
    _ -> undefined
  | n == closestEncloserSym = case args of
    [ nam, exNam, exDat, nonExNam, nonExDat ] -> TVClosestEncloser nam exNam exDat nonExNam nonExDat
    _ -> undefined
  | n == nsec3UnsignedSym = case args of
    [ nam, hashedNam, nsecDat ] -> TVNSEC3Unsigned nam hashedNam nsecDat
    _ -> undefined
  | n == keyTagSym = case args of
    [ key, tag ] -> TVKeyTag key tag
    _ -> undefined
  | n == adSym = case map toView args of
    [ TVInt 0 ] -> TVAd False
    [ TVInt 1 ] -> TVAd True
    _ -> undefined
  | otherwise = TVFunctor n args
toView TEmptyList = TVEmptyList
toView (TVar v) = TVVar v
toView (TAtomQuoted n) = TVAtomQuoted n
toView (TAtom n) = TVAtom n
toView (TString s) = TVString s
toView (TInt i) = TVInt i
toView (TComplex dat) = TVComplex dat

fromView :: TermView -> Term
fromView (TVList a b) = TFunctor listSym [ a, b ]
fromView (TVMember a b) = member a b
fromView (TVNeq a b) = a \= b
fromView (TVLeq a b) = a =< b
fromView (TVResolves (Resolves ns qtype qname d rec ver adb)) = resolves ns qtype qname d rec ver adb
fromView (TVResolvable ns qtype qname ver auth) = resolvable ns qtype qname ver auth
fromView (TVRRSIGValid nam dat sig key) = rrsigValid nam dat sig key
fromView (TVDigestValid nam dsTerm dnskeyTerm) = digestValid nam dsTerm dnskeyTerm
fromView (TVNSECUnsigned nam nsecDat) = nsecUnsigned nam nsecDat
fromView (TVClosestEncloser nam exNam exDat nonExNam nonExDat) = closestEncloser nam exNam exDat nonExNam nonExDat
fromView (TVNSEC3Unsigned nam hashedNam nsecDat) = nsec3Unsigned nam hashedNam nsecDat
fromView (TVKeyTag key tag) = keyTag key tag
fromView (TVAd b) = ad (fromEnum b)
fromView (TVFunctor n args) = TFunctor n args
fromView TVEmptyList = TEmptyList
fromView (TVVar v) = TVar v
fromView (TVAtomQuoted n) = TAtomQuoted n
fromView (TVAtom n) = TAtom n
fromView (TVString s) = TString s
fromView (TVInt i) = TInt i
fromView (TVComplex dat) = TComplex dat

isResolves :: Term -> Bool
isResolves (toView -> TVResolves {}) = True
isResolves _ = False

isNeq :: Term -> Bool
isNeq (toView -> TVNeq {}) = True
isNeq _ = False
