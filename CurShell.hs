{-# LANGUAGE NamedFieldPuns, LambdaCase, BlockArguments #-}
module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Cur
import qualified Data.ByteString.UTF8 as B8
import Data.IORef
import Data.Maybe
import GHC.Float
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact as I
import Language.Haskell.Interpreter
import Lens.Micro
import Linear.Matrix hiding (trace)
import Linear.V3
import Linear.V4
import Linear.Vector
import System.Environment
import System.INotify hiding (Event)
import Text.Printf


render :: MVar (Maybe (Cur ())) -> FilePath -> IO ()
render model f = do
  r <- runInterpreter do
    loadModules [f]
    setImports ["Prelude", "Graphics.Gloss.Data.Picture", "Cur"]
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


compiler :: MVar (Maybe (Cur ())) -> FilePath -> IO ()
compiler model f = do
  i <- initINotify
  addWatch i [MoveIn, Modify] (B8.fromString f) (const $ render model f)
  render model f


xsz = 1080
ysz = 1080
xfd = float2Double . (/ xsz)
yfd = float2Double . (/ ysz)

screenify :: Picture -> Picture
screenify = scale xsz ysz

translate_rel :: V3 Double -> View -> View
translate_rel d v = v & vt %~ (^+^ inv33 (rs_matrix v) !* d)

update_view :: Event -> View -> View

update_view (EventKey (Char c) Down _ _) = case c of
  'r' -> const init_view
  'b' -> (vmaxz .~ 2)        . (vminz .~ 1)
  'B' -> (vmaxz .~ maxBound) . (vminz .~ 0)
  'x' -> (vry .~ (-90)) . (vrx .~ 0)
  'y' -> (vry .~ 0)     . (vrx .~ 90)
  'z' -> (vry .~ 0)     . (vrx .~ 0)
  'X' -> (vry .~ 90)    . (vrx .~ 0)
  'Y' -> (vry .~ 0)     . (vrx .~ (-90))
  'Z' -> (vry .~ 180)   . (vrx .~ 0)
  'p' -> vp %~ not
  _   -> id

update_view (EventKey (MouseButton LeftButton) Down m p) = vmouse .~ (m, Just p)
update_view (EventKey (MouseButton LeftButton) Up   m p) = vmouse .~ (m, Nothing)

update_view (EventKey (MouseButton b) Down (Modifiers s Up Up) _)
  | b == WheelUp   && s == Up   = vz %~ (* 1.1)
  | b == WheelDown && s == Up   = vz %~ (/ 1.1)
  | b == WheelUp   && s == Down = translate_rel $ V3 0 0   0.01
  | b == WheelDown && s == Down = translate_rel $ V3 0 0 (-0.01)

update_view (EventKey (MouseButton b) Down (Modifiers Up Down Up) _)
  | b == WheelUp   = vmaxz %~ (* 1.01)
  | b == WheelDown = vmaxz %~ (/ 1.01)

update_view (EventMotion (x, y)) = \v ->
  case _vmouse v of
    (Modifiers Up Up Up, Just (x0, y0)) ->
      translate_rel (V3 (xfd $ x - x0) (yfd $ y - y0) 0) v
        & vmouse._2 .~ Just (x, y)

    (Modifiers Down Up Up, Just (x0, y0)) ->
      v & vry %~ (+ xfd (x0 - x) * 360)
        & vrx %~ (+ yfd (y0 - y) * 360)
        & vmouse._2 .~ Just (x, y)

    _ -> v

update_view _ = id


main :: IO ()
main = do
  fname:_    <- getArgs
  model      <- newMVar Nothing
  controller <- newIORef Nothing

  forkIO $ compiler model fname
  interactIO
    (InWindow ("Cur " ++ fname) (1920, 1080) (100, 100))
    (makeColor 0.2 0.2 0.2 0)
    init_view
    (\v -> screenify <$> runCur v <$> fromMaybe (return ()) <$> readMVar model)
    (\e v -> do Just c <- readIORef controller
                controllerSetRedraw c
                return $ update_view e v)
    (writeIORef controller . Just)
