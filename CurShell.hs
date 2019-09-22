{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Cur
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as B8
import Data.Maybe
import GHC.Float
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate as A
import Language.Haskell.Interpreter
import System.Clock
import System.Environment
import System.INotify
import System.IO.Unsafe
import Text.Printf


type StoredPic = Maybe (Float -> Picture)


loading :: Float -> Picture
loading t = color (makeColor 0.8 0.8 0.9 0.8)
  $ pictures
  $ map (\x -> rotate (sin t * x) (Arc x (x*2) x))
  $ map (** 2) [1..20]


render :: MVar StoredPic -> FilePath -> Event -> IO ()
render pic f e = do
  r <- runInterpreter do
    loadModules [f]
    setImportsQ [("Prelude", Nothing),
                 ("Graphics.Gloss", Nothing),
                 ("Cur", Nothing)]
    setTopLevelModules [take (length f - 3) f]
    interpret "pic" (as :: Float -> Picture)
  case r of
    Left  (WontCompile xs) -> do
      swapMVar pic Nothing
      mapM_ (\case GhcError {errMsg} -> printf "%s\n" errMsg) xs
    Right p -> do
      swapMVar pic $ Just p
      printf "\027[2J\027[1;1H%s OK\n" f


compiler :: MVar StoredPic -> FilePath -> IO ()
compiler pic f = do
  i <- initINotify
  addWatch i [MoveIn, Modify] (B8.fromString f) (render pic f)
  render pic f (Modified False Nothing)


now :: IO Double
now = do t <- fromIntegral <$> toNanoSecs <$> getTime Realtime
         return $! t / 1000000000.0


main :: IO ()
main = do
  fname:_ <- getArgs
  pic     <- newMVar Nothing
  forkIO $ compiler pic fname
  animateIO (InWindow "Cur" (1920, 1080) (100, 100))
            (makeColor 0.2 0.2 0.2 0)
            (\t -> do f <- fromMaybe loading <$> readMVar pic
                      return $ f t)
            (\c -> return ())
