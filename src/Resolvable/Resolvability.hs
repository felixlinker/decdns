{-# LANGUAGE ViewPatterns #-}
module Resolvable.Resolvability
  ( RComp(..)
  , fromZoneFile
  , NameServersConfig
  , compileConfig
  , plResolvable ) where

import qualified Data.List.NonEmpty as NE

import NameConstraint
import Resolvable.Prolog as Pl
import DNS.Name
import DNS.Record
import qualified DNS.RecordType as T
import DNS.ZoneFile

import Util.Fresh (Fresh, fresh)
import Control.Monad (void, guard)

injectLast :: Term -> Term -> Term
injectLast newLast t = case toView t of
  v@(TVList _ _ ) -> rec v
  _ -> t
  where
    rec :: TermView -> Term
    rec TVEmptyList = tList newLast emptyList
    rec (TVList a b) = tList a (rec (toView b))
    -- x cannot be a list
    rec x = tList newLast (fromView x)

captureListTail :: Term -> Term -> Term
captureListTail captureWith t = case toView t of
  v@(TVList _ _) -> rec v
  _ -> t
  where
    rec :: TermView -> Term
    rec (TVList a b) = tList a (rec (toView b))
    rec _ = captureWith

data RComp = RComp
  { fromNS :: Name String
  , fromRecord :: ConstrainedRRSet }
  deriving (Eq, Ord)

resolvableBase :: [Name String] -> Name String -> ConstrainedRRSet -> Fresh (Rule RComp)
resolvableBase rootServers ns s@(Named n rds@((RData _ _ ty) NE.:| _)) = do
    (n', map (uncurry (\=)) -> neqs) <- nameWithSynthesis n
    let annot = RComp ns s
        -- TODO: Refine so that only root keys are trusted
        -- Set the ad bit of DNSKEY entries stored at root servers to 1
        adb = fromEnum (toType ty == T.DNSKEY && ns `elem` rootServers)
    return (Rule annot (resolves (nameTerm ns) (typeAtom ty) n' (list $ NE.map (dataFunctor . rtype) rds) (ad adb)) neqs)

resolvableStep :: [Name String] -> Name String -> ConstrainedRRSet -> Fresh [Rule RComp]
resolvableStep (map nameTerm -> rootServers) ns s@(Named n rs) = concat <$> mapM (go . rtype) (NE.toList rs)
  where
    rannot :: RComp
    rannot = RComp ns s

    go :: Type -> Fresh [Rule RComp]
    go (NS (nameTerm -> delegatedNs)) = sequence $ do
      rootServer <- rootServers
      glueType <- ttypeAtom <$> [T.A, T.AAAA]
      return $ do
        t <- TVar <$> fresh "T"
        -- Do not expect wildcard neq constraints on NS records
        let (captureListTail t -> n') = nameWithoutSynthesis n
        q <- TVar <$> fresh "Q"
        res <- TVar <$> fresh "R"
        adb <- TVar <$> fresh "AD"
        return Rule { rannotation = rannot
                    , rhead = resolves (nameTerm ns) q n' res adb
                    , rbody = [ resolves delegatedNs q n' res adb
                              , resolves rootServer glueType delegatedNs TWildcard TWildcard ] }
    go (CNAME (nameTerm -> cname)) = sequence $ do
      rootServer <- rootServers
      return $ do
        (n', map (uncurry (\=)) -> neqs) <- nameWithSynthesis n
        q <- TVar <$> fresh "Q"
        res <- TVar <$> fresh "R"
        adb <- TVar <$> fresh "AD"
        return Rule { rannotation = rannot
                    , rhead = resolves (nameTerm ns) q n' res adb
                    , rbody = neqs ++ [ resolves rootServer q cname res adb ] }
    go (DNAME (nameTerm -> dname)) = sequence $ do
      -- TODO: Instantiate rootServer over predicate instead of generated one rule per server
      rootServer <- rootServers
      return $ do
        l <- TVar <$> fresh "L"
        t <- TVar <$> fresh "T"
        let rewrite = injectLast l . captureListTail t
        -- Do not expect wildcard neq constraints on DNAME records
        let (rewrite -> n') = nameWithoutSynthesis n
        q <- TVar <$> fresh "Q"
        res <- TVar <$> fresh "R"
        adb <- TVar <$> fresh "AD"
        return Rule { rannotation = rannot
                    , rhead = resolves (nameTerm ns) q n' res adb
                    , rbody = [ resolves rootServer q (rewrite dname) res adb ] }
    -- TODO: I think I will need to branch on `cov` to cover authenticating
    -- DNSKEY records; those require a corresponding DS record additionally
    -- TODO: Check that sigName is the name of the zone containing the record;
    -- does that mean there ex. a corresponding SOA record? Could be, but seems
    -- technically wrong as there could be intermediate SOA records. It could be
    -- that this will be implicitly addressed by the ad(1) resolution of the
    -- DNSKEY record.
    -- TODO: Verify that DNSKEY is at zone apex
    go (RRSIG cov (algAtom -> alg) ls (TInt -> ottl) _ _ (TInt -> tag) (nameTerm -> sigName) (b64Atom -> sig)) = sequence $ do
      -- Spec say that ls may also be greater than owner name of signed records,
      -- but this can only happen due to synthesis, which we have not performed
      -- yet. Thus, we simply check equality.
      guard (ls + 1 == nlength n)  -- +1 because labels field does not count root
      rootServer <- rootServers
      return $ do
        let covT = ttypeAtom cov
        t <- TVar <$> fresh "T"
        (captureListTail t -> n', map (uncurry (\=)) -> neqs) <- nameWithSynthesis n
        d0 <- TVar <$> fresh "D"
        let ad0Q = resolves (nameTerm ns) covT n' d0 (ad 0)
        let ad1Q = resolves (nameTerm ns) covT n' d0 (ad 1)
        d1 <- TVar <$> fresh "D"
        k <- TVar <$> fresh "K"
        flags <- TVar <$> fresh "F"
        let keyF = keyData flags alg k tag
        let valid = rrsigValid n' covT ottl d0 (TInt ls) sig k
        keyQs <- if cov == T.DNSKEY
            then do
              d2 <- TVar <$> fresh "D"
              let keyQ = resolves rootServer (ttypeAtom T.DNSKEY) sigName d1 (ad 0)
              let dsQ = resolves rootServer (ttypeAtom T.DS) sigName d2 (ad 1)
              digT <- TVar <$> fresh "DIGT"
              dig <- TVar <$> fresh "DIG"
              let pkRef = pkRefData tag alg digT dig
              let digValid = digestValid digT sigName flags alg k dig
              return  [ keyQ, dsQ, member pkRef d2, digValid ]
            else return [ resolves rootServer (ttypeAtom T.DNSKEY) sigName d1 (ad 1) ]
        return Rule { rannotation = rannot
                    , rhead = ad1Q
                    , rbody = [ ad0Q
                              ] ++ keyQs ++
                              [ member keyF d1
                              , valid ] ++ neqs }
    -- Ignore below, but pattern-match so compiler generates warnings on future
    -- extensions
    go (A {}) = return []
    go (AAAA {}) = return []
    go (TXT {}) = return []
    go (MX {}) = return []
    go (SOA {}) = return []
    go (NSEC {}) = return []
    go (DS {}) = return []
    go (DNSKEY {}) = return []

resolvableR :: Fresh (Rule ())
resolvableR = do
  s <- TVar <$> fresh "S"
  q <- TVar <$> fresh "Q"
  n <- TVar <$> fresh "N"
  return $ Rule () (resolvable s q n) [ resolves s q n TWildcard (ad 0) ]

allResolvable :: Rule ()
allResolvable =
  Rule () (TFunctor "all_resolvable" [ q, rns, ns ]) [ bagof n (resolvable rns q n) ns ]
  where
    q = TVar "Q"
    rns = TVar "RS"
    ns = TVar "Names"
    n = TVar "Name"

    bagof :: Term -> Term -> Term -> Term
    bagof a b c = TFunctor "bagof" [ a, b, c ]

fromZoneFile :: [Name String] -> Name String -> [Zone ConstrainedLabel] -> Fresh [Rule RComp]
fromZoneFile rootServers ns rs = concat <$> mapM zoneToRules rs
  where
    zoneToRules :: Zone ConstrainedLabel -> Fresh [Rule RComp]
    zoneToRules z = concat <$> mapM rrsetToRules (allRRSets z)

    rrsetToRules :: ConstrainedRRSet -> Fresh [Rule RComp]
    rrsetToRules r = (:) <$> resolvableBase rootServers ns r <*> resolvableStep rootServers ns r

type NameServersConfig = [(Name String, [Zone ConstrainedLabel])]

compileConfig :: [Name String] -> NameServersConfig -> Fresh [Rule RComp]
compileConfig rootServers = (concat <$>) . mapM (uncurry (fromZoneFile rootServers))

plResolvable :: [Rule RComp] -> Fresh [Rule ()]
plResolvable compiled = do
  res <- resolvableR
  return $ allResolvable : res : map void compiled
