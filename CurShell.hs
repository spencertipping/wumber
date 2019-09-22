module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import qualified Data.ByteString as BS
import Data.ByteString.UTF8
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate as A
import Language.Haskell.Interpreter
import System.Environment
import System.INotify
import System.IO.Unsafe


default_fg :: Color
default_fg = makeColor 0.8 0.8 0.9 0.8

pic :: MVar Picture
pic = unsafePerformIO $ newMVar (color default_fg p)
  where p = pictures $ map Circle [50, 60, 70, 80, 100, 200, 380]

render :: FilePath -> Event -> IO ()
render f e = do
  putStrLn $ "rendering " ++ f

compiler :: FilePath -> IO ()
compiler f = do
  i <- initINotify
  addWatch i [MoveIn, Modify] (fromString f) (render f)
  return ()

main :: IO ()
main = do
  fname:_ <- getArgs
  forkIO $ compiler fname
  animateIO (InWindow "Cur" (1920, 1080) (0, 0))
            (makeColor 0.2 0.2 0.2 0)
            (\t -> readMVar pic)
            (\c -> putStrLn "controller?")
