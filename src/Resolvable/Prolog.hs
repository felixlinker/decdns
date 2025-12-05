{-# LANGUAGE ViewPatterns #-}
module Resolvable.Prolog
  ( Rule(..)
  , fromZoneFile ) where

import NameConstraint
import ZoneFile

import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.Char (toLower, toUpper)
import RecursiveResolver (nameInData)
import Data.Maybe (fromMaybe)
import Util.Fresh (Fresh, fresh)

data Term = TVar String | TAtom String | TList [Term] (Maybe Term)

instance Show Term where
  show (TVar v) = map toUpper v
  show (TAtom a) = map toLower a
  show (TList [] _) = "[]"
  show (TList hs t) =
    "[" ++ intercalate ", " (map show hs ++ maybe [] (\x -> [" | " ++ show x]) t) ++ "]"

plWildcard :: Term
plWildcard = TVar "_"

dnsWildcard :: Term
dnsWildcard = TAtom "'*'"

typeAtom :: Type -> Term
typeAtom = TAtom . show

data Predicate = Predicate String [Term]

instance Show Predicate where
  show (Predicate h (map show -> args)) = h ++ "(" ++ intercalate ", " args ++ ")"

resolvable :: Term -> Term -> Term -> Predicate
resolvable ns t n = Predicate "resolvable" [ ns, t, n ]

dif :: Term -> Term -> Predicate
dif a b = Predicate "dif" [ a, b ]

partialNameRule :: Name (NameConstraint String) -> Fresh (Term, [Predicate])
partialNameRule (Name (l NE.:| ls)) = case l of
  Wildcard neqs -> do
    h <- TVar <$> fresh "H"
    t <- TVar <$> fresh "T"
    return (TList (reverse (h : exactlyNatoms ls)) (Just t), difs h neqs)
  _ -> return (TList (reverse (exactlyNatoms (l:ls))) Nothing, [])
  where
    difs :: Term -> [String] -> [Predicate]
    difs var = map (dif var . TAtom)

    exactlyNatoms :: [NameConstraint String] -> [Term]
    exactlyNatoms [] = []
    exactlyNatoms (Wildcard _ : t) = dnsWildcard : exactlyNatoms t
    exactlyNatoms (Exactly "" : t) = exactlyNatoms t
    exactlyNatoms (Exactly c : t) = TAtom c : exactlyNatoms t

nameTerm :: Name String -> Term
nameTerm (Name (NE.toList -> ls)) = TList (reverse $ map TAtom $ filter (not . null) ls) Nothing

data Rule = Rule
  { rhead :: Predicate
  , rbody :: [Predicate] }

instance Show Rule where
  show (Rule h []) = show h ++ "."
  show (Rule h b) = show h ++ " :- " ++ intercalate ", " (map show b) ++ "."

resolvableBase :: Name String -> Record (NameConstraint String) -> Fresh Rule
resolvableBase ns (Record n (RData _ _ ty _)) = do
  (n', difs) <- partialNameRule n
  return (Rule (resolvable (nameTerm ns) (typeAtom ty) n') difs)

resolvableStep :: Name String -> Record (NameConstraint String) -> Fresh [Rule]
resolvableStep (nameTerm -> ns) r@(Record n d) = do
  (n', difs) <- partialNameRule n
  q <- TVar <$> fresh "Q"
  let rules = case rtype d of
        NS -> let delegatedNs = nameTerm (fromMaybe undefined $ nameInData r)
              in [ Rule { rhead = resolvable ns q n'
                        , rbody = [ dif q (typeAtom NS)
                                  , resolvable plWildcard (typeAtom A) delegatedNs
                                  , resolvable delegatedNs q n' ] } ]
        CNAME ->
          let cname = nameTerm (fromMaybe undefined $ nameInData r)
          in [ Rule { rhead = resolvable ns q n'
                    , rbody = [ dif q (typeAtom CNAME)
                              , resolvable plWildcard q cname ] } ]
        A -> []
        TXT -> []
        _ -> undefined
  return (map (addDifs difs) rules)
  where
    addDifs :: [Predicate] -> Rule -> Rule
    addDifs ps rule@(Rule _ b) = rule { rbody = ps ++ b }

fromZoneFile :: Name String -> Zone (NameConstraint String) -> Fresh [Rule]
fromZoneFile ns z = concat <$> mapM recordToRules rs
  where
    rs :: [Record (NameConstraint String)]
    rs = allRecords z

    recordToRules :: Record (NameConstraint String) -> Fresh [Rule]
    recordToRules r = do
      base <- resolvableBase ns r
      steps <- resolvableStep ns r
      return (base:steps)
