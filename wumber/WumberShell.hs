{-# LANGUAGE NamedFieldPuns, LambdaCase, BlockArguments, TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module WumberShell where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.IORef
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact as I
import Linear.V3 (V3)
import System.Environment
import Text.Printf

import Wumber

import WumberShell.Compiler
import WumberShell.Event
import WumberShell.Render
import WumberShell.View


wumber_main :: Wumber (Sketch (V3 R)) -> IO ()
wumber_main m = do
  model      <- newMVar Nothing
  controller <- newIORef Nothing

  update_model model m
  interactIO
    (InWindow "Wumber" (1920, 1080) (100, 100))
    (makeColor 0.2 0.2 0.2 0)
    (init_view 1080)
    (\v -> do m <- fromMaybe [] <$> readMVar model
              return $ screenify v m)
    (\e v -> do Just c <- readIORef controller
                controllerSetRedraw c
                return $ update_view e v)
    (writeIORef controller . Just)
