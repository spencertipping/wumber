{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Manages on-disk caching for 'Computed' objects. This provides potentially
--   huge speedups for expensive objects that change infrequently, e.g.
--   isosurface boundaries and constraint systems.
--
--   All data is gzip-compressed before writing to disk. For the iso model test
--   this reduces the size by about 11x (3.7MB -> 333KB).

module WumberShell.ComputedCache where


import Codec.Compression.GZip (compress, decompress)
import Control.Concurrent     (forkIO)
import Data.Binary            (Binary(..), encode, decode)
import GHC.Generics           (Generic)
import System.Directory       (createDirectoryIfMissing, doesPathExist,
                               getXdgDirectory, XdgDirectory(..))
import System.IO              (FilePath)
import System.IO.Unsafe       (unsafePerformIO)

import qualified Data.ByteString.Lazy as BL

import Wumber.Fingerprint
import Wumber.Model


-- | Computes using the cache.
cached_compute :: Binary a => ComputedCache -> Fingerprint -> a -> IO a
cached_compute (CC p) f v = do
  let f' = p ++ "/" ++ show f
  createDirectoryIfMissing True p
  e <- doesPathExist f'
  if e
    then decode <$> decompress <$> BL.readFile f'
    else do forkIO $ BL.writeFile f' $ compress (encode v)
            return v


-- | A directory to store stuff. 'user_cache' will give you a sensible default.
newtype ComputedCache = CC { unCC :: FilePath }
  deriving (Show, Eq, Generic, Binary)


-- | The Right Place (TM) to store computed results.
user_cache :: ComputedCache
user_cache = CC $ unsafePerformIO (getXdgDirectory XdgCache "wumber")
