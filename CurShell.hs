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
import Linear.V4
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


init_view :: View
init_view = V (V4 (V4 1080 0 0 0)
                  (V4 0 1080 0 0)
                  (V4 0 0 1080 0)
                  (V4 0 0 1 0))
              (BB minBound maxBound)
              (Modifiers Up Up Up, Nothing)


f2d = float2Double

update_view :: Event -> View -> View

update_view (EventKey (Char 'r') _ _ _) _ = init_view

update_view (EventKey (MouseButton LeftButton) Down m p) v = v & vmouse .~ (m, Just p)
update_view (EventKey (MouseButton LeftButton) Up   m p) v = v & vmouse .~ (m, Nothing)
update_view (EventMotion (x, y)) v =
  case _vmouse v of
    (Modifiers Up Up Up, Just (x0, y0)) ->
      v & vm %~ (!+! V4 0 0 0 (V4 (f2d $ x - x0) (f2d $ y - y0) 0 0))
        & vmouse._2 .~ Just (x, y)

    (Modifiers Down Up Up, Just (x0, y0)) ->
      v & vm %~ (V4 (V4   cx  0 sx 0)
                    (V4    0  1  0 0)
                    (V4 (-sx) 0 cx 0)
                    (V4    0  0  0 1) !*!)

        & vm %~ (V4 (V4 1  0    0  0)
                    (V4 0 cy (-sy) 0)
                    (V4 0 sy   cy  0)
                    (V4 0  0    0  1) !*!)

        & vmouse._2 .~ Just (x, y)

        where (cx, sx) = cs $ f2d (x - x0)
              (cy, sy) = cs $ f2d (y - y0)

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
    (\v -> runCur v <$> fromMaybe (return ()) <$> readMVar pic)
    (\e v -> do Just c <- readIORef controller
                controllerSetRedraw c
                return $ update_view e v)
    (writeIORef controller . Just)
