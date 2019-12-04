{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns, LambdaCase, BlockArguments #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module WumberShell.Compiler (
  compiler_loop,
  update_model,
  type_is
) where

import Control.Concurrent      (ThreadId, forkIO, forkOS, killThread)
import Control.Concurrent.MVar (MVar, modifyMVar, newEmptyMVar, putMVar,
                                swapMVar, tryTakeMVar)
import Control.Monad           (forM_)
import Data.Foldable           (toList)
import Data.Typeable           (Typeable)
import Linear.Matrix           (identity)
import Linear.V3               (V3(..))
import System.Clock            (Clock(..), getTime, toNanoSecs)
import System.INotify          (addWatch, EventVariety(..), initINotify)
import System.IO               (stderr)
import System.IO.Unsafe        (unsafePerformIO)
import Text.Printf             (HPrintfType, hPrintf, printf)

import qualified Data.ByteString.UTF8         as B8
import qualified Language.Haskell.Interpreter as HI

import Wumber
import Wumber.DualContour
import Wumber.SymbolicJIT

import WumberShell.ComputedCache


eprintf :: HPrintfType r => String -> r
eprintf = hPrintf stderr


type_is :: Typeable a => a
type_is = HI.as


compiler_loop :: (Typeable a, Computed a (Sketch (V3 R)))
              => MVar (Maybe [Element]) -> FilePath -> String -> a -> IO ()
compiler_loop model f expr type_marker = do
  i <- initINotify
  addWatch i [MoveIn, Modify] (B8.fromString f)
    $ const (recompile model f expr type_marker)
  recompile model f expr type_marker


module_name :: FilePath -> String
module_name p = map dotify $ take (length p - 3) p
  where dotify '/' = '.'
        dotify  c  =  c


worker :: MVar ThreadId
worker = unsafePerformIO newEmptyMVar


nanos :: IO Integer
nanos = toNanoSecs <$> getTime Realtime

nanos_since :: Integer -> IO Integer
nanos_since t = flip (-) t <$> nanos


recompile :: (Typeable a, Computed a (Sketch (V3 R)))
          => MVar (Maybe [Element]) -> FilePath -> String -> a -> IO ()
recompile model f expr type_marker = do
  eprintf "\027[2J\027[1;1Hcompiling...\n"
  start_time <- nanos

  r <- HI.runInterpreter do
    HI.loadModules [f]
    HI.setTopLevelModules [module_name f]
    HI.interpret expr type_marker

  compile_nanos <- nanos_since start_time

  case r of
    Left (HI.WontCompile xs) -> do
      swapMVar model Nothing
      eprintf "\027[2J\027[1;1H%s error\n" f
      mapM_ (\case HI.GhcError {HI.errMsg} -> printf "%s\n" errMsg) xs

    Left e -> do
      swapMVar model Nothing
      eprintf "\027[2J\027[1;1H%s compiler error\n" f
      eprintf "%s\n" (show e)

    Right p -> do
      w <- tryTakeMVar worker
      case w of Just t -> killThread t
                _      -> return ()

      forkOS (update_model model p) >>= putMVar worker
      eprintf "\027[2J\027[1;1H%s OK\n" f
      eprintf "  compile time: %dms\n" (compile_nanos `quot` 1_000_000)


update_model :: Computed a (Sketch (V3 R))
             => MVar (Maybe [Element]) -> a -> IO ()
update_model model v = do
  start_time <- nanos
  !s <- cached_compute user_cache (fingerprint v) (compute v)
  compute_nanos <- nanos_since start_time
  eprintf "  compute/cache time: %dms\n" (compute_nanos `quot` 1_000_000)

  swapMVar model $ Just (map line (unSketch s))
  return ()
  where line (a, b) = shape_of identity [a, b]
