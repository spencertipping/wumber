{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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
import GHC.Fingerprint  (Fingerprint)
import GHC.Generics     (Generic)
import System.Directory (createDirectoryIfMissing, doesPathExist,
                         getXdgDirectory, XdgDirectory(..))
import System.IO        (FilePath)
import System.IO.Unsafe (unsafePerformIO)

import Wumber.Fingerprint
import Wumber.Model


-- | Computes using the cache.
cached_compute :: Binary a => ComputedCache -> Fingerprint -> a -> IO a
cached_compute (CC p) f v = do let f' = p ++ "/" ++ show f
                               createDirectoryIfMissing True p
                               e <- doesPathExist f'
                               putStrLn "gonna do the file thing"
                               if e
                                 then decodeFile f'
                                 else do putStrLn "about to encode"
                                         encodeFile f' v
                                         putStrLn "encoded"
                                         return v


-- | A directory to store stuff. 'user_cache' will give you a sensible default.
newtype ComputedCache = CC { unCC :: FilePath }
  deriving (Show, Eq, Generic, Binary)


-- | The Right Place (TM) to store computed results.
user_cache :: ComputedCache
user_cache = CC $ unsafePerformIO (getXdgDirectory XdgCache "wumber")
