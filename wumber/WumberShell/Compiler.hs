{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns, LambdaCase, BlockArguments #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module WumberShell.Compiler (
  compiler_loop
) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (forM_)
import Data.Foldable (toList)
import Data.Maybe
import Data.Typeable (Typeable)
import Linear.Matrix (identity)
import Linear.V3     (V3(..))
import System.INotify hiding (Event)
import System.IO (stderr)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf

import qualified Data.ByteString.UTF8         as B8
import qualified Language.Haskell.Interpreter as HI

import Wumber
import Wumber.DualContour
import Wumber.SymbolicJIT

import WumberShell.ComputedCache


eprintf :: HPrintfType r => String -> r
eprintf = hPrintf stderr


compiler_loop :: MVar (Maybe [Element]) -> FilePath -> IO ()
compiler_loop model f = do
  i <- initINotify
  addWatch i [MoveIn, Modify] (B8.fromString f) (const $ recompile model f)
  recompile model f


module_name :: FilePath -> String
module_name p = map dotify $ take (length p - 3) p
  where dotify '/' = '.'
        dotify  c  =  c


worker :: MVar ThreadId
worker = unsafePerformIO newEmptyMVar


recompile :: MVar (Maybe [Element]) -> FilePath -> IO ()
recompile model f = do
  eprintf "\027[2J\027[1;1Hcompiling...\n"

  r <- HI.runInterpreter do
    -- TODO
    -- For whatever reason, I can't seem to get this to work outside a stack
    -- build environment. It's possible it never will, but I'd like to figure
    -- out why hint doesn't load the modules built into our executable.
    --
    -- It fails on the loadModules step, before any of our imports/etc. I
    -- suspect I'm misusing hint by calling loadModules and then trying to mess
    -- with imports.
    --
    -- Worst case we git-clone wumber as source into some temp dir, then add it
    -- to the search path. (Ugh.)

    HI.loadModules [f]
    HI.setTopLevelModules [module_name f]
    HI.interpret "main" (HI.as :: Wumber (Sketch (V3 R)))

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

      w <- update_model model p
      swapMVar worker w
      eprintf "\027[2J\027[1;1H%s OK\n" f


update_model :: MVar (Maybe [Element]) -> Wumber (Sketch (V3 R)) -> IO ThreadId
update_model model (f, v) = forkOS do
  s <- cached_compute user_cache f v
  swapMVar model $ Just (map line (unSketch s))
  return ()
  where line (a, b) = shape_of identity [a, b]
