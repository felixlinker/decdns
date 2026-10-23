{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module Resolvable.Resolvability
  ( RComp(..)
  , nsesTerm
  , fromZone
  , compileRRSet
  , compileConfig ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.List (find)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

import NameConstraint
import Resolvable.Prolog.Core
import Resolvable.Prolog.TermView
import Resolvable.Prolog.Reverse (asRRSigData)
import DNS.Name
import DNS.Record
import qualified DNS.RecordType as T
import DNS.RRSet (RRSet, nsNames, rrsetType, getAliased)
import DNS.Zone (collectReferrals, apexNameServers)
import DNS.ZoneFile as Z
import Util.Fresh (Fresh, freshId)
import Util.Monad

type ZoneNSesT = Term

nsesTerm :: Foldable f => f (Name String) -> ZoneNSesT
nsesTerm = list . foldr ((:) . nameTerm) []

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
  { rid :: Int
  , fromType :: T.Type }
  deriving (Eq, Ord)

synthesisCases :: [ConstrainedName -> Fresh (Term, [Term])]
synthesisCases = [nameWithoutSynthesis', nameWithSynthesis]
  where
    nameWithoutSynthesis' :: ConstrainedName -> Fresh (Term, [Term])
    nameWithoutSynthesis' n = return (nameWithoutSynthesis n, [])

resolvableBase :: ZoneNSesT -> ConstrainedName -> Bool -> ConstrainedRRSet -> Fresh (S.Set (Rule RComp))
resolvableBase nses zone isReferral (Named n rds@((RData _ _ ty) NE.:| _)) =
  foreachM synthesisCases $ \synth -> do
    (n', cs) <- synth n
    ra <- rann
    return $ S.singleton $ Rule ra (resolves nses (typeTerm ty) zoneTerm n' (rrsetTerm rds) bot bot (ad 0)) cs
    where
      zoneTerm :: Term
      zoneTerm = if isReferral then referralZone else nameWithoutSynthesis zone

      rann :: Fresh RComp
      rann = do
        ruleId <- freshId
        return $ RComp ruleId (toType ty)

dnssecTypeNeqs :: Term -> [Term]
dnssecTypeNeqs t = map ($ t) [(\= dsType), (\= dnskeyType), (\= rrsigType), (\= nsecType), (\= nsec3Type)]

resolvableStep :: ZoneNSesT -> ZoneNSesT -> ConstrainedName -> ConstrainedRRSet -> Fresh (S.Set (Rule RComp))
resolvableStep roots nses (nameWithoutSynthesis -> zone) rrset@(Named n rs) = go (rrsetType rrset)
  where
    rannot :: T.Type -> Fresh RComp
    rannot ty = do
      ruleId <- freshId
      return $ RComp ruleId ty

    ownerNoSynth :: Term
    ownerNoSynth = nameWithoutSynthesis n

    glueCases :: Name String -> [Fresh (Term, Term)]
    glueCases delegated =
      return (nses, referralZone) : [ (roots,) <$> wildcard | not (isSubDomain delegated (toStringName n)) ]

    go :: T.Type -> Fresh (S.Set (Rule RComp))
    go r = case r of
      T.NS  | ownerNoSynth == zone -> return S.empty
            | otherwise ->
        let delegated = nsNames rs
            delegatedT = nsesTerm delegated
        in mconcat <$> sequence
          [ foreachM [aType, aaaaType] $ \glueType -> foreachM delegated $ \s -> foreachM (glueCases s) $ \glueCase ->
              do
                (nTail, t, d, v, a, alias) <- freshVars6 ("N", "T", "D", "V", "A", "Alias")
                let n' = captureListTail nTail ownerNoSynth
                rann <- rannot r
                (glueServers, glueZone) <- glueCase
                glueData <- wildcard
                return (S.singleton $ Rule rann (resolves nses t zone n' d alias v a)
                  [ t \= dsType
                  , isName n'
                  , resolves glueServers glueType glueZone (nameTerm s) glueData bot bot (ad 0)
                  , resolves delegatedT t ownerNoSynth n' d alias v a ])
            -- Note: Below is a temporary rule for performance
            ,  foreachM [aType, aaaaType] $ \glueType -> foreachM delegated $ \s -> foreachM (glueCases s) $ \glueCase ->
              do
                (l, nTail, d, v, a, alias) <- freshVars6 ("L", "N", "D", "V", "A", "Alias")
                let n' = injectLast l $ captureListTail nTail ownerNoSynth
                rann <- rannot r
                (glueServers, glueZone) <- glueCase
                glueData <- wildcard
                return (S.singleton $ Rule rann (resolves nses dsType zone n' d alias v a)
                  [ isName n'
                  , resolves glueServers glueType glueZone (nameTerm s) glueData bot bot (ad 0)
                  , resolves delegatedT dsType ownerNoSynth n' d alias v a ])
            , do  -- NSEC for unsigned subzone
                (nTail, t, d, dNSEC) <- freshVars4 ("N", "T", "D", "D_NSEC")
                let n' = captureListTail nTail ownerNoSynth
                rann <- rannot r
                return $ S.singleton $ Rule rann (resolves nses t zone n' d bot top (ad 0))
                  [ t \= dsType
                  , isName n'
                  , resolves nses nsecType zone ownerNoSynth (list [dNSEC]) bot top (ad 1)
                  , nsecUnsigned ownerNoSynth dNSEC
                  , resolves nses t zone n' d bot bot (ad 0) ]
            , do  -- NSEC3 for owner for unsigned subzone
                (nTail, t, d, nsecV, dNSEC) <- freshVars5 ("N", "T", "D", "N_NSEC", "D_NSEC")
                let nNSEC = injectLast nsecV zone
                let n'' = captureListTail nTail ownerNoSynth
                rann <- rannot r
                return $ S.singleton $ Rule rann (resolves nses t zone n'' d bot top (ad 0))
                  [ t \= dsType
                  , isName n''
                  , resolves nses nsec3Type zone nNSEC (list [dNSEC]) bot top (ad 1)
                  , nsec3Unsigned ownerNoSynth nNSEC dNSEC
                  , resolves nses t zone n'' d bot bot (ad 0) ]
            , do  -- NSEC3 closest encloser proof for unsigned subzone
              (nTail, t, d, nsecV, nsecV', dNSEC, dNSEC') <- freshVars7 ("N", "T", "D", "N_NSEC", "N_NSEC", "D_NSEC", "D_NSEC")
              let n'' = captureListTail nTail ownerNoSynth
              let nNSEC = injectLast nsecV zone
              let nNSEC' = injectLast nsecV' zone
              rann <- rannot r
              return $ S.singleton $ Rule rann (resolves nses t zone n'' d bot top (ad 0))
                [ t \= dsType
                , isName n''
                , resolves nses nsec3Type zone nNSEC (list [dNSEC]) bot top (ad 1)
                , resolves nses nsec3Type zone nNSEC' (list [dNSEC']) bot top (ad 1)
                , closestEncloser ownerNoSynth nNSEC dNSEC nNSEC' dNSEC'
                , resolves nses t zone n'' d bot bot (ad 0) ]
            ]
      T.CNAME ->
        let cname = maybe (error "invariant error") nameTerm (getAliased rrset)
        in foreachM synthesisCases $ \synth -> S.fromList <$> sequence
        [ do  -- Unauthenticated
          (n', cs) <- synth n
          (t, d, targetZone) <- freshVars3 ("T", "D", "Z")
          targetAlias <- wildcard
          rann <- rannot r
          return $ Rule rann (resolves nses t zone n' d top bot (ad 0))
            (cs ++ dnssecTypeNeqs t ++
              [ t \= cnameType, targetZone \= referralZone
              , resolves roots t targetZone cname d targetAlias bot (ad 0) ])
        , do  -- Authenticated
          (n', cs) <- synth n
          (t, d, a, aCNAME, aAlias, targetZone) <- freshVars6 ("T", "D", "AD_Answer", "AD_CNAME", "AD_Alias", "Z")
          cnameZone <- wildcard
          targetAlias <- wildcard
          rann <- rannot r
          return $ Rule rann (resolves nses t zone n' d top top a)
            (cs ++ dnssecTypeNeqs t ++
              [ t \= cnameType, targetZone \= referralZone
              , resolves roots cnameType cnameZone ownerNoSynth (rrsetTerm rs) bot top aCNAME
              , resolves roots t targetZone cname d targetAlias top aAlias
              , minTerm aCNAME aAlias a ]) ]
      T.DNAME ->
        let dname = maybe (error "invariant error") nameTerm (getAliased rrset)
        in S.fromList <$> sequence
        [ do  -- Unauthenticated
          (l, nTail, t, d, targetZone) <- freshVars5 ("L", "N", "T", "D", "Z")
          let rewrite = injectLast l . captureListTail nTail
          let n' = rewrite ownerNoSynth
          -- Do not expect wildcard neq constraints on DNAME records
          targetAlias <- wildcard
          rann <- rannot r
          return $ Rule rann (resolves nses t zone n' d top bot (ad 0))
            (dnssecTypeNeqs t ++
              [ targetZone \= referralZone
              , isName n'
              , resolves roots t targetZone (rewrite dname) d targetAlias bot (ad 0) ])
        , do  -- Authenticated
          (l, nTail, t, d, a, aDNAME, aAlias, targetZone) <- freshVars8 ("L", "N", "T", "D", "A_Answer", "A_DNAME", "A_Alias", "Z")
          dnameZone <- wildcard
          let rewrite = injectLast l . captureListTail nTail
          let n' = rewrite ownerNoSynth
          -- Do not expect wildcard neq constraints on DNAME records
          targetAlias <- wildcard
          rann <- rannot r
          return $ Rule rann (resolves nses t zone n' d top top a)
            (dnssecTypeNeqs t ++
              [ targetZone \= referralZone
              , isName n'
              , resolves roots dnameType dnameZone ownerNoSynth (rrsetTerm rs) bot top aDNAME
              , resolves roots t targetZone (rewrite dname) d targetAlias top aAlias
              , minTerm aDNAME aAlias a ]) ]
      -- TODO: Check that sigName is the name of the zone containing the record;
      -- does that mean there ex. a corresponding SOA record? Could be, but seems
      -- technically wrong as there could be intermediate SOA records. It could be
      -- that this will be implicitly addressed by the ad(1) resolution of the
      -- DNSKEY record.
      T.RRSIG -> foreachM (map (exceptUnsafe asRRSigData . rtype) $ NE.toList rs) $
        \sig@(RRSIGData cov _ _ _ _ _ _ (nameTerm -> sigName) _) ->
        case cov of
          T.DNSKEY -> do
            (d, d_DS, k, kRef) <- freshVars4 ("D", "D_DS", "K", "K_REF")
            parentZone <- wildcard
            rann <- rannot r
            return $ S.singleton $ Rule rann
              (resolves nses dnskeyType zone ownerNoSynth d bot top (ad 1))
              [ resolves nses dnskeyType zone ownerNoSynth d bot bot (ad 0)
              , resolves roots dsType parentZone ownerNoSynth d_DS bot top (ad 1)
              , member k d
              , member kRef d_DS
              , digestValid ownerNoSynth k kRef
              , rrsigValid ownerNoSynth d (TComplex (RRSIG sig)) k ]
          _ -> foreachM synthesisCases $ \synth -> do
            (n', cs) <- synth n
            (d, d_KEY, k) <- freshVars3 ("D", "D_KEY", "K")
            rann <- rannot r
            return $ S.singleton $ Rule rann
              (resolves nses (ttypeTerm cov) zone n' d bot top (ad 1))
              (cs ++  [ isName n'
                      , resolves nses (ttypeTerm cov) zone n' d bot bot (ad 0)
                      , resolves nses dnskeyType zone sigName d_KEY bot top (ad 1)
                      , member k d_KEY
                      , rrsigValid ownerNoSynth d (TComplex (RRSIG sig)) k ])
      T.A -> return S.empty
      T.AAAA -> return S.empty
      T.TXT -> return S.empty
      T.ZONEMD -> return S.empty
      T.MX -> return S.empty
      T.SOA -> return S.empty
      T.NSEC -> return S.empty
      T.NSEC3 -> return S.empty
      T.NSEC3PARAM -> return S.empty
      T.DS -> return S.empty
      T.DNSKEY -> return S.empty

compileRRSet :: ZoneNSesT -> ZoneNSesT -> ConstrainedName -> Bool -> ConstrainedRRSet -> Fresh (S.Set (Rule RComp))
compileRRSet roots nses zone isReferral rs =
  (<>) <$> resolvableBase nses zone isReferral rs <*> resolvableStep roots nses zone rs

trustedKey :: RRSet String -> Fresh (Rule RComp)
trustedKey (Named n dsData) = do
  i <- freshId
  nses <- wildcard
  return $ Rule (RComp i T.DS) (resolves nses dsType (nameTerm n) (nameTerm n) (rrsetTerm dsData) bot top (ad 1)) []

fromZone :: ZoneNSesT -> Zone ConstrainedLabel -> Fresh (S.Set (Rule RComp))
fromZone roots z = do
    let (z', referrals) = collectReferrals z
    authoritative <- mapM (comp False) (allRRSets z')
    nonAuthoritative <-  mapM (comp True) referrals
    return $ mconcat (authoritative ++ nonAuthoritative)
  where
    zoneNSesT :: ZoneNSesT
    zoneNSesT = nsesTerm $ fromMaybe (error "invariant error") (apexNameServers z)

    comp :: Bool -> ConstrainedRRSet -> Fresh (S.Set (Rule RComp))
    comp = compileRRSet roots zoneNSesT (zoneName z)

compileConfig :: Name String -> [RRSet String] -> [Zone ConstrainedLabel] -> ExceptT String Fresh (S.Set (Rule RComp))
compileConfig rootZoneName dsData zones = do
  roots <- maybe (throwE "no root zone data") return $ do
    z <- find isRootZone zones
    nsesTerm <$> apexNameServers z
  trustedKeys <- mapM (lift . trustedKey) dsData
  rs <- mconcat <$> mapM (lift . fromZone roots) zones
  return $ foldr S.insert rs trustedKeys
  where
    isRootZone :: Label a => Zone a -> Bool
    isRootZone = (== rootZoneName) . canonicalName . toStringName . zoneName
