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
render pic f = do
  r <- runInterpreter do
    loadModules [f]
    setImports ["Prelude", "Graphics.Gloss.Data.Picture", "Cur"]
    setTopLevelModules [take (length f - 3) f]
    interpret "main" (as :: Cur ())
  case r of
    Left (WontCompile xs) -> do
      swapMVar pic Nothing
      printf "\027[2J\027[1;1H%s error\n" f
      mapM_ (\case GhcError {errMsg} -> printf "%s\n" errMsg) xs
    Right p -> do
      swapMVar pic $ Just p
      printf "\027[2J\027[1;1H%s OK\n" f


compiler :: MVar (Maybe (Cur ())) -> FilePath -> IO ()
compiler pic f = do
  i <- initINotify
  addWatch i [MoveIn, Modify] (B8.fromString f) (const $ render pic f)
  render pic f


xsz = 1080
ysz = 1080
xfd = float2Double . (/ xsz)
yfd = float2Double . (/ ysz)

init_view :: View
init_view = V 0 0 0 1
              identity
              (BB minBound maxBound)
              (Modifiers Up Up Up, Nothing)


update_view :: Event -> View -> View

update_view (EventKey (Char 'r') Down _ _) _ = init_view

update_view (EventKey (Char 'x') Down _ _) v = v & vry .~ 90 & vrx .~ 0
update_view (EventKey (Char 'y') Down _ _) v = v & vry .~ 0  & vrx .~ 90
update_view (EventKey (Char 'z') Down _ _) v = v & vry .~ 0  & vrx .~ 0

update_view (EventKey (Char 'h') Down _ _) v = v & vt._x %~ (+ (-0.1) / _vz v)
update_view (EventKey (Char 'l') Down _ _) v = v & vt._x %~ (+   0.1  / _vz v)
update_view (EventKey (Char 'j') Down _ _) v = v & vt._y %~ (+ (-0.1) / _vz v)
update_view (EventKey (Char 'k') Down _ _) v = v & vt._y %~ (+   0.1  / _vz v)

update_view (EventKey (MouseButton LeftButton) Down m p) v = v & vmouse .~ (m, Just p)
update_view (EventKey (MouseButton LeftButton) Up   m p) v = v & vmouse .~ (m, Nothing)

update_view (EventKey (MouseButton WheelUp)   Down m p) v = v & vz %~ (* 1.1)
update_view (EventKey (MouseButton WheelDown) Down m p) v = v & vz %~ (/ 1.1)

update_view (EventMotion (x, y)) v =
  case _vmouse v of
    (Modifiers Up Up Up, Just (x0, y0)) ->
      v & vt %~ (^+^ inv33 (rs_matrix v) !* V3 (xfd $ x - x0) (yfd $ y - y0) 0)
        & vmouse._2 .~ Just (x, y)

    (Modifiers Down Up Up, Just (x0, y0)) ->
      v & vry %~ (+ xfd (x0 - x) * 360)
        & vrx %~ (+ yfd (y0 - y) * 360)
        & vmouse._2 .~ Just (x, y)

    _ -> v

update_view _ v = v


main :: IO ()
main = do
  fname:_    <- getArgs
  pic        <- newMVar Nothing
  controller <- newIORef Nothing
  forkIO $ compiler pic fname
  interactIO
    (InWindow ("Cur " ++ fname) (1920, 1080) (100, 100))
    (makeColor 0.2 0.2 0.2 0)
    init_view
    (\v -> scale xsz ysz <$> runCur v <$> fromMaybe (return ()) <$> readMVar pic)
    (\e v -> do Just c <- readIORef controller
                controllerSetRedraw c
                return $ update_view e v)
    (writeIORef controller . Just)
