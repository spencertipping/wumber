{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Fingerprintable objects, a feature that Wumber uses to cache expensive
--   objects to disk.
module Wumber.Fingerprint (
  Fingerprintable(..),
  binary_fingerprint,
  Fingerprint(..)
) where


import Crypto.Hash.SHA256   (hash)
import Data.Binary          (Binary(..), decode, encode)
import Data.ByteString      (ByteString(..))
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Word            (Word64)
import GHC.Fingerprint      (Fingerprint(..))


-- | Things that can provide a fingerprint that uniquely represents their state.
--   /This function is not expected to be super-fast./ It's fine to lean on
--   'Binary' and hash the output using SHA256, then take the first 128 bits of
--   that.
class Fingerprintable a where fingerprint :: a -> Fingerprint

instance Fingerprintable ByteString where
  fingerprint = decode . fromStrict . hash


-- | A handy function you can use to instantly become 'Fingerprintable'.
binary_fingerprint :: Binary a => a -> Fingerprint
binary_fingerprint = fingerprint . toStrict . encode
