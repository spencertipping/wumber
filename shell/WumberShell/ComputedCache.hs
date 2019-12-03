{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- NOTE
-- Don't enable 'NoMonomorphismRestriction' for this module. We rely on a fixed
-- type for 'compute' because 'Computed' doesn't have a fundep from a -> b.

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Manages on-disk caching for 'Computed' objects. This provides potentially
--   huge speedups for expensive objects that change infrequently, e.g.
--   isosurface boundaries and constraint systems.
module WumberShell.ComputedCache where


import Data.Binary      (Binary(..), encodeFile, decodeFile)
import GHC.Generics     (Generic)
import System.Directory (createDirectoryIfMissing, doesPathExist,
                         getXdgDirectory, XdgDirectory(..))
import System.IO        (FilePath)
import System.IO.Unsafe (unsafePerformIO)

import Wumber.Fingerprint
import Wumber.Model


-- | A directory to store stuff. 'user_cache' will give you a sensible default.
newtype ComputedCache = CC { unCC :: FilePath }
  deriving (Show, Eq, Generic, Binary)


-- | The Right Place (TM) to store computed results.
user_cache :: ComputedCache
user_cache = CC $ unsafePerformIO (getXdgDirectory XdgCache "wumber")


-- | A version of 'compute' that populates and/or uses entries in the specified
--   disk cache.
cached_compute :: Computed a b => ComputedCache -> a -> IO b
cached_compute (CC p) v = do
  let f        = show $ fingerprint v
      filename = p ++ "/" ++ f
  createDirectoryIfMissing True p
  e <- doesPathExist filename
  if e
    then decodeFile filename
    else do let c = compute v
            encodeFile filename c
            return c
