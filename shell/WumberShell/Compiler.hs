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
import Data.Vector.Storable (unsafeWith)
import System.INotify hiding (Event)
import System.IO (stderr)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf

import qualified Data.ByteString.UTF8         as B8
import qualified Language.Haskell.Interpreter as HI

import Wumber


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
    HI.interpret "main" (HI.as :: Wumber ())

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


update_model :: MVar (Maybe [Element]) -> Wumber () -> IO ThreadId
update_model model m = forkOS do
  fn <- (. to_storable_vector) <$> jit <$> head <$> runWumber init_cursor m
  forM_ [6..18] \r -> do
    eprintf "\027[2J\027[1;1Hrendering at %d..." r
    let ls = toList $ iso_contour fn (BB (-2) 2) r (max 15 (r + 6)) 0.1
    eprintf " [%d line(s)]" (length ls) -- NB: force list before swapping mvar
    swapMVar model $! Just $! ls
