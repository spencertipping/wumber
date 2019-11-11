{-# LANGUAGE NamedFieldPuns, LambdaCase, BlockArguments, TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module WumberShell.Compiler where

import Control.Concurrent
import Control.Concurrent.MVar
import qualified Data.ByteString.UTF8 as B8
import Data.Maybe
import Language.Haskell.Interpreter
import System.INotify hiding (Event)
import System.IO (stderr)
import Text.Printf

import Wumber hiding (compile)


eprintf :: HPrintfType r => String -> r
eprintf = hPrintf stderr


compiler_loop :: MVar (Maybe [Element]) -> FilePath -> IO ()
compiler_loop model f = do
  i <- initINotify
  addWatch i [MoveIn, Modify] (B8.fromString f) (const $ compile model f)
  compile model f


module_name :: FilePath -> String
module_name p = map dotify $ take (length p - 3) p
  where dotify '/' = '.'
        dotify  c  =  c


compile :: MVar (Maybe [Element]) -> FilePath -> IO ()
compile model f = do
  eprintf "\027[2J\027[1;1Hcompiling...\n"

  r <- runInterpreter do
    -- TODO
    -- For whatever reason, I can't seem to get this to work outside a stack
    -- build environment. It's possible it never will, but I'd like to figure
    -- out why hint doesn't load the modules built into our executable.
    --
    -- It fails on the loadModules step, before any of our imports/etc. I
    -- suspect I'm misusing hint by calling loadModules and then trying to mess
    -- with imports.

    loadModules [f]
    setTopLevelModules [module_name f]
    interpret "main" (as :: Wumber ())

  case r of
    Left (WontCompile xs) -> do
      swapMVar model Nothing
      eprintf "\027[2J\027[1;1H%s error\n" f
      mapM_ (\case GhcError {errMsg} -> printf "%s\n" errMsg) xs

    Left e -> do
      swapMVar model Nothing
      eprintf "\027[2J\027[1;1H%s compiler error\n" f
      eprintf "%s\n" (show e)

    Right p -> do
      m <- runWumber init_cursor p
      swapMVar model $ Just m
      eprintf "\027[2J\027[1;1H%s OK\n" f
