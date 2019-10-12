{-# LANGUAGE NamedFieldPuns, LambdaCase, BlockArguments #-}
module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Cur
import qualified Data.ByteString.UTF8 as B8
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate as A
import Graphics.Gloss.Interface.IO.Interact as I
import Language.Haskell.Interpreter
import System.Environment
import System.INotify
import Text.Printf


loading :: Float -> Picture
loading t = color (makeColor 0.8 0.8 0.9 0.8)
  $ pictures
  $ map (\x -> rotate (sin t * x) (Arc x (x*2) x))
  $ map (** 2) [1..20]


render :: MVar (Maybe (Float -> Picture)) -> FilePath -> IO ()
render pic f = do
  r <- runInterpreter do
    loadModules [f]
    setImportsQ [("Prelude", Nothing),
                 ("Graphics.Gloss", Nothing),
                 ("Cur", Nothing)]
    setTopLevelModules [take (length f - 3) f]
    interpret "pic" (as :: Float -> Picture)
  case r of
    Left (WontCompile xs) -> do
      swapMVar pic Nothing
      printf "\027[2J\027[1;1H%s error\n" f
      mapM_ (\case GhcError {errMsg} -> printf "%s\n" errMsg) xs
    Right p -> do
      swapMVar pic $ Just p
      printf "\027[2J\027[1;1H%s OK\n" f


compiler :: MVar (Maybe (Float -> Picture)) -> FilePath -> IO ()
compiler pic f = do
  i <- initINotify
  addWatch i [MoveIn, Modify] (B8.fromString f) (const $ render pic f)
  render pic f


main :: IO ()
main = do
  fname:_ <- getArgs
  pic     <- newMVar Nothing
  forkIO $ compiler pic fname
  animateIO (InWindow "Cur" (1920, 1080) (100, 100))
            (makeColor 0.2 0.2 0.2 0)
            (\t -> flip ($) t <$> fromMaybe loading <$> readMVar pic)
            (\c -> return ())
