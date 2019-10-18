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

import Wumber

import WumberShell.Compiler
import WumberShell.Event
import WumberShell.Render
import WumberShell.View


main :: IO ()
main = do
  fname:_    <- getArgs
  model      <- newMVar Nothing
  controller <- newIORef Nothing

  forkIO $ compiler_loop model fname
  interactIO
    (InWindow ("Wumber " ++ fname) (1920, 1080) (100, 100))
    (makeColor 0.2 0.2 0.2 0)
    (init_view 1080)
    (\v -> do m  <- fromMaybe (return ()) <$> readMVar model
              m' <- runWumber init_cursor m
              return $ screenify v m')
    (\e v -> do Just c <- readIORef controller
                controllerSetRedraw c
                return $ update_view e v)
    (writeIORef controller . Just)
