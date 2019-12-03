{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Wumber.Fingerprint where


import Crypto.Hash.SHA256   (hash)
import Data.Binary          (Binary(..), decode, encode)
import Data.ByteString      (ByteString(..))
import Data.ByteString.Lazy (fromStrict, toStrict)
import GHC.Fingerprint      (Fingerprint(..))


-- | Things that can provide a fingerprint that uniquely represents their state.
--   /This function is not expected to be super-fast./ It's fine to lean on
--   'Binary' and hash the output using SHA256, then take the first 128 bits of
--   that.
class Fingerprintable a where fingerprint :: a -> Fingerprint

instance {-# OVERLAPPABLE #-} Binary a => Fingerprintable a where
  fingerprint = fingerprint . toStrict . encode

instance Fingerprintable ByteString where
  fingerprint b = Fingerprint l h
    where (l, h) = decode $ fromStrict (hash b)
