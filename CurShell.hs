{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as B8
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate as A
import Language.Haskell.Interpreter
import System.Environment
import System.INotify
import System.IO.Unsafe
import Text.Printf


default_fg :: Color
default_fg = makeColor 0.8 0.8 0.9 0.8


loading :: Float -> Picture
loading t = color default_fg
  $ pictures
  $ map (\x -> rotate (sin t * x) (rectangleWire x x))
  $ map (** 2) [1..20]


render :: MVar (Maybe Picture) -> FilePath -> Event -> IO ()
render pic f e = do
  r <- runInterpreter do
    setImportsQ [("Prelude", Nothing), ("Graphics.Gloss", Nothing)]
    loadModules [f]
    setTopLevelModules [take (length f - 3) f]
    interpret "pic" (as :: Picture)
  case r of Left  err -> do swapMVar pic Nothing
                            printf "ERROR\n%s\n" (show err)
            Right p   -> do swapMVar pic $ Just p
                            printf "compiled %s\n" f


compiler :: MVar (Maybe Picture) -> FilePath -> IO ()
compiler pic f = do
  i <- initINotify
  addWatch i [MoveIn, Modify] (B8.fromString f) (render pic f)
  render pic f (Modified False Nothing)


main :: IO ()
main = do
  fname:_ <- getArgs
  pic     <- newMVar Nothing
  forkIO $ compiler pic fname
  animateIO (InWindow "Cur" (1920, 1080) (100, 100))
            (makeColor 0.2 0.2 0.2 0)
            (\t -> fromMaybe (loading t) <$> readMVar pic)
            (\c -> return ())
