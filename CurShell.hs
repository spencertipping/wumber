{-# LANGUAGE NamedFieldPuns, LambdaCase, BlockArguments, TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.IORef
import Data.Maybe
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact as I
import System.Environment
import Text.Printf

import Cur

import CurShell.Compiler
import CurShell.Event
import CurShell.Render
import CurShell.View


main :: IO ()
main = do
  fname:_    <- getArgs
  model      <- newMVar Nothing
  controller <- newIORef Nothing

  let ic = init_cursor (makeColor 0.8 0.8 0.9 0.8)

  forkIO $ compiler_loop model fname
  interactIO
    (InWindow ("Cur " ++ fname) (1920, 1080) (100, 100))
    (makeColor 0.2 0.2 0.2 0)
    (init_view 1080)
    (\v -> screenify v <$> runCur ic <$> fromMaybe (return ()) <$> readMVar model)
    (\e v -> do Just c <- readIORef controller
                controllerSetRedraw c
                return $ update_view e v)
    (writeIORef controller . Just)
