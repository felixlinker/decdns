module DNS.Response
  ( fromResourceRecord ) where

import Control.Monad (guard)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (mapMaybe)
import qualified Network.DNS as W

import DNS.Name
import DNS.Record
import qualified DNS.RecordType as T
import DNS.SEC.Crypto
import DNS.SEC.Keys (dnsKeyFlagsFromInt)
import Parsing.Name (parseNameM)

wireName :: W.Domain -> Maybe (Name String)
wireName = parseNameM . BS.unpack

-- The dns package decodes SOA RNAMEs as mailboxes (local@domain),
-- whereas our record model stores the original sequence of DNS labels.
wireMailbox :: W.Mailbox -> Maybe (Name String)
wireMailbox bs = case BS.break (== '@') bs of
  (local, domain) | not (BS.null domain) ->
    (BS.unpack local <|) <$> wireName (BS.drop 1 domain)
  _ -> wireName bs

wireType :: W.TYPE -> Maybe T.Type
wireType = T.intToType . fromIntegral . W.fromTYPE

fromResourceRecord :: W.ResourceRecord -> Maybe StandardRecord
fromResourceRecord r = do
  guard (W.rrclass r == 1)
  n <- wireName (W.rrname r)
  d <- dat (W.rdata r)
  return $ Named n (RData IN (fromIntegral (W.rrttl r)) d)
  where
    dat :: W.RData -> Maybe Type
    dat (W.RD_A ip) = Just (A ip)
    dat (W.RD_AAAA ip) = Just (AAAA ip)
    dat (W.RD_NS n) = NS <$> wireName n
    dat (W.RD_CNAME n) = CNAME <$> wireName n
    dat (W.RD_DNAME n) = DNAME <$> wireName n
    dat (W.RD_MX p n) = MX (fromIntegral p) <$> wireName n
    dat (W.RD_TXT s) = Just (TXT (BS.unpack s))
    dat (W.RD_SOA mn rn s rf rt e m) =
      SOA <$> wireName mn <*> wireMailbox rn <*> pure (fromIntegral s)
          <*> pure (fromIntegral rf) <*> pure (fromIntegral rt)
          <*> pure (fromIntegral e) <*> pure (fromIntegral m)
    dat (W.RD_DS tag alg digest bs) = DS <$>
      (DSData (fromIntegral tag) <$> algorithmFromInt (fromIntegral alg)
        <*> digestFromInt (fromIntegral digest) <*> pure bs)
    dat (W.RD_DNSKEY flags protocol alg bs) = do
      guard (protocol == 3)
      DNSKEY <$> (DNSKEYData (dnsKeyFlagsFromInt (fromIntegral flags))
        <$> algorithmFromInt (fromIntegral alg) <*> pure bs)
    dat (W.RD_RRSIG sig) = RRSIG <$>
      (RRSIGData <$> wireType (W.rrsigType sig)
        <*> algorithmFromInt (fromIntegral (W.rrsigKeyAlg sig))
        <*> pure (fromIntegral (W.rrsigNumLabels sig))
        <*> pure (fromIntegral (W.rrsigTTL sig))
        <*> pure (fromIntegral (W.rrsigExpiration sig))
        <*> pure (fromIntegral (W.rrsigInception sig))
        <*> pure (fromIntegral (W.rrsigKeyTag sig))
        <*> wireName (W.rrsigZone sig) <*> pure (W.rrsigValue sig))
    dat (W.RD_NSEC n ts) = NSEC . (`NSECData` mapMaybe wireType ts) <$> wireName n
    dat (W.RD_NSEC3 alg flags iter salt next ts) = NSEC3 <$>
      (NSEC3Data <$> nsec3DigestFromInt (fromIntegral alg)
        <*> pure (nsec3FlagsFromInt (fromIntegral flags)) <*> pure (fromIntegral iter)
        <*> pure salt <*> pure next <*> pure (mapMaybe wireType ts))
    dat (W.RD_NSEC3PARAM alg flags iter salt) =
      NSEC3PARAM <$> nsec3DigestFromInt (fromIntegral alg)
        <*> pure (nsec3FlagsFromInt (fromIntegral flags))
        <*> pure (fromIntegral iter) <*> pure salt
    -- Unknown types and unsupported algorithms cannot produce usable rules.
    dat (W.UnknownRData _) = Nothing
    dat _ = Nothing
