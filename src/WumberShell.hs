{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns, LambdaCase, BlockArguments, TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module WumberShell (
  wumber_live,
  wumber_main,
  WumberShell.Compiler.type_is
) where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.IORef
import Data.Maybe
import Data.Typeable (Typeable)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact as I
import Linear.V3 (V3)
import System.Environment
import System.Posix.Directory (changeWorkingDirectory)
import Text.Printf

import Wumber

import WumberShell.Compiler
import WumberShell.Event
import WumberShell.Render
import WumberShell.View


wumber_live :: (Show a, Typeable a, Computed a (Sketch (V3 R)))
            => FilePath -> FilePath -> String -> a -> IO ()
wumber_live base_path file expr type_marker = do
  changeWorkingDirectory base_path
  model <- newMVar Nothing
  forkIO $ compiler_loop model file expr type_marker
  wumber_window ("Wumber " ++ file) model


wumber_main :: (Show a, Computed a (Sketch (V3 R))) => a -> IO ()
wumber_main m = do
  putStrLn "computing model..."
  model <- newMVar Nothing
  update_model model m
  wumber_window "Wumber" model


wumber_window :: String -> MVar (Maybe [Element]) -> IO ()
wumber_window title model = do
  controller <- newIORef Nothing
  interactIO
    (InWindow title (1920, 1080) (100, 100))
    (makeColor 0.2 0.2 0.2 0)
    (init_view 1080)
    (\v -> do m <- fromMaybe [] <$> readMVar model
              return $ screenify v m)
    (\e v -> do Just c <- readIORef controller
                controllerSetRedraw c
                return $ update_view e v)
    (writeIORef controller . Just)
