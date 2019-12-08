{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Fingerprintable objects, a feature that Wumber uses to cache expensive
--   objects to disk.
--
--   __WARNING:__ fingerprints are not required to be consistent across
--   machines, operating systems, or architectures. The only guarantee is that
--   multiple runs of the same build of Wumber on the same machine should yield
--   the same results.
--
--   Also note that different types can have overlapping fingerprints.

module Wumber.Fingerprint (
  Fingerprintable(..),
  binary_fingerprint,
  tree_fingerprint,
  Fingerprint(..)
) where


import Control.Monad        (forM)
import Crypto.Hash.SHA256   (hash, init, finalize, update)
import Data.Binary          (Binary(..), decode, encode)
import Data.ByteString      (ByteString(..))
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Foldable        (foldl')
import Data.Int             (Int8, Int16, Int32, Int64)
import Data.Word            (Word, Word8, Word16, Word32, Word64)
import GHC.Fingerprint      (Fingerprint(..))
import Language.Haskell.TH  (Type(ConT))

import Prelude hiding (init)

import Wumber.Macros


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


-- | Builds a Merkle tree from fingerprints. Generally you should use this for
--   recursive data structures, rather than backing into 'Binary'.
tree_fingerprint :: Foldable f => f Fingerprint -> Fingerprint
tree_fingerprint = decode . fromStrict . finalize . foldl' each init
  where each h = update h . toStrict . encode


-- NOTE
-- We can't do what this is obviously trying to do, add 'Binary a' as a context,
-- because GHC doesn't consider context when evaluating instance heads. I don't
-- know whether TH is the right workaround under the circumstances.

$(forM [''(), ''Bool, ''Char, ''Double, ''Float,
        ''Int, ''Int8, ''Int16, ''Int32, ''Int64, ''Integer,
        ''Ordering,
        ''String,
        ''Word, ''Word8, ''Word16, ''Word32, ''Word64]
   \n -> reinstantiate (ConT n) <$>
         [d| instance Fingerprintable where fingerprint = binary_fingerprint |])
