module Resolvable.Prolog.TermView
  -- TODO: Enforce usage of termView or exported constructors by hiding constructors of Term
  ( TermView(..)
  , toView
  , fromView ) where

import Resolvable.Prolog.Symbols
import Resolvable.Prolog.Core
import qualified Resolvable.Prolog.Library as Lib

data TermView = TVList Term Term
              | TVMember Term Term
              | TVNeq Term Term
              | TVLeq Term Term
              | TVResolves Term Term Term Term Term
              | TVResolvable Term Term Term
              | TVRRSIGValid Term Term Term Term Term Term Term
              | TVDigestValid Term Term Term Term Term Term
              | TVKeyTag Term Term
              | TVAd Bool
              | TVFunctor String [ Term ]
              | TVEmptyList
              | TVVar String
              | TVWildcard
              | TVAtomQuoted String
              | TVAtom String
              | TVString String
              | TVInt Int

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
    [ ns, qtype, qname, d, adb ] -> TVResolves ns qtype qname d adb
    _ -> undefined
  | n == resolvableSym = case args of
    [ ns, qtype, qname ] -> TVResolvable ns qtype qname
    _ -> undefined
  | n == rrsigValidSym = case args of
    [ nam, typeCovered, orgTTL, rrset, ls, sig, key ] -> TVRRSIGValid nam typeCovered orgTTL rrset ls sig key
    _ -> undefined
  | n == digestValidSym = case args of
    [ typ, nam, flags, alg, key, dig ] -> TVDigestValid typ nam flags alg key dig
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
toView TWildcard = TVWildcard
toView (TAtomQuoted n) = TVAtomQuoted n
toView (TAtom n) = TVAtom n
toView (TString s) = TVString s
toView (TInt i) = TVInt i

fromView :: TermView -> Term
fromView (TVList a b) = TFunctor listSym [ a, b ]
fromView (TVMember a b) = member a b
fromView (TVNeq a b) = a \= b
fromView (TVLeq a b) = a =< b
fromView (TVResolves ns qtype qname d adb) = Lib.resolves ns qtype qname d adb
fromView (TVResolvable ns qtype qname) = Lib.resolvable ns qtype qname
fromView (TVRRSIGValid nam typeCovered orgTTL rrset ls sig key) = Lib.rrsigValid nam typeCovered orgTTL rrset ls sig key
fromView (TVDigestValid typ nam flags alg key dig) = Lib.digestValid typ nam flags alg key dig
fromView (TVKeyTag key tag) = Lib.keyTag key tag
fromView (TVAd b) = Lib.ad (fromEnum b)
fromView (TVFunctor n args) = TFunctor n args
fromView TVEmptyList = TEmptyList
fromView (TVVar v) = TVar v
fromView TVWildcard = TWildcard
fromView (TVAtomQuoted n) = TAtomQuoted n
fromView (TVAtom n) = TAtom n
fromView (TVString s) = TString s
fromView (TVInt i) = TInt i
