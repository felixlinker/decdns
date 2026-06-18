{-# LANGUAGE ViewPatterns #-}
module DNS.SEC.Verification
  ( verify ) where

import qualified Crypto.Hash.Algorithms as Hash
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

import DNS.Name
import DNS.Record
import Util.Bytes
import DNS.SEC.Crypto
import DNS.RRSet
import Data.Maybe (fromMaybe)

verify :: StdRRSet -> RRSIGData BS.ByteString -> BS.ByteString -> Bool
verify (canonicalRRSet -> rrset) (canonicalRRSIGData -> sig) key = fromMaybe False $
  case _signatureAlgorithm sig of
    ECDSAP256SHA256 -> do
      cSig <- ecdsaSig (_signature sig)
      cPk <- ecdsaPk key
      return $ ECDSA.verify Hash.SHA256 cPk cSig (BS.concat (serialize sig : serializedRRSet))
    _ -> Nothing
  where
    lc :: Int
    lc = _labels sig

    originalOwner :: Name String
    originalOwner = case name rrset of
      n@(Name ls) ->
        let lc' = labelCount n
        in  if lc' > lc then Name ("*" NE.:| NE.drop (lc' - lc) ls) else n

    -- Set construction for sorting and deduplication
    serializedRRSet :: [BS.ByteString]
    serializedRRSet = S.toAscList $ S.fromList $ NE.toList
      $ fmap serialize $ toRecords $ Named originalOwner (value rrset)
