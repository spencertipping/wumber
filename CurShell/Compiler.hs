{-# LANGUAGE NamedFieldPuns, LambdaCase, BlockArguments, TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module CurShell.Compiler where

import Control.Concurrent
import Control.Concurrent.MVar
import Cur
import qualified Data.ByteString.UTF8 as B8
import Data.Maybe
import Language.Haskell.Interpreter
import System.INotify hiding (Event)
import Text.Printf


compiler_loop :: MVar (Maybe (Cur ())) -> FilePath -> IO ()
compiler_loop model f = do
  i <- initINotify
  addWatch i [MoveIn, Modify] (B8.fromString f) (const $ compile model f)
  compile model f


compile :: MVar (Maybe (Cur ())) -> FilePath -> IO ()
compile model f = do
  r <- runInterpreter do
    loadModules [f]
    setImports ["Prelude"]
    setTopLevelModules [take (length f - 3) f]
    interpret "main" (as :: Cur ())
  case r of
    Left (WontCompile xs) -> do
      swapMVar model Nothing
      printf "\027[2J\027[1;1H%s error\n" f
      mapM_ (\case GhcError {errMsg} -> printf "%s\n" errMsg) xs
    Right p -> do
      swapMVar model $ Just p
      printf "\027[2J\027[1;1H%s OK\n" f
