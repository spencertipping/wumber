{-# LANGUAGE NamedFieldPuns, LambdaCase, BlockArguments, TemplateHaskell #-}
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
import Lens.Micro.TH
import Linear.Matrix hiding (trace)
import Linear.V3
import Linear.V4
import Linear.Vector
import System.Environment
import System.INotify hiding (Event)
import Text.Printf


data View = V { _vt     :: !(V3 Double),
                _vry    :: !Double,
                _vrx    :: !Double,
                _vz     :: !Double,
                _vp     :: !Bool,
                _vrs    :: !(M33 Double),
                _vm     :: !(M44 Double),
                _vclipz :: !Double,
                _vclipc :: !Color,
                _vmouse :: (Modifiers, Maybe Point) }
  deriving (Show)

makeLenses ''View

init_view :: View
init_view = V 0 0 0 1 True
              identity
              identity
              maxBound
              (makeColor 0.8 0.8 0.9 0.05)
              (Modifiers Up Up Up, Nothing)

rs_matrix :: View -> M33 Double
rs_matrix v = V3 (V3 1    0   0)
                 (V3 0   cx  sx)
                 (V3 0 (-sx) cx)
          !*! V3 (V3   cy  0 sy)
                 (V3    0  1  0)
                 (V3 (-sy) 0 cy)
          !*! identity !!* _vz v
  where (cy, sy) = cs (_vry v)
        (cx, sx) = cs (_vrx v)


view_matrix :: View -> M44 Double
view_matrix v = (identity & _w .~ (if _vp v then V4 0 0 1 0 else V4 0 0 0 1))
            !*! (identity & _z._w .~ 1)
            !*! (identity & _m33 .~ rs_matrix v)
            !*! transpose (identity & _w._xyz .~ _vt v)


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

screenify :: View -> [Element] -> Picture
screenify v = scale xsz ysz . pictures . map (project v')
  where v' = v & vm  .~ view_matrix v
               & vrs .~ rs_matrix v

translate_rel :: V3 Double -> View -> View
translate_rel d v = v & vt %~ (^+^ inv33 (rs_matrix v) !* d)

update_view :: Event -> View -> View

update_view (EventKey (Char c) Down _ _) = case c of
  'r' -> const init_view
  'b' -> vclipz .~ 0.5
  'B' -> vclipz .~ maxBound
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
  | b == WheelUp   = vclipz %~ (* 1.1)
  | b == WheelDown = vclipz %~ (/ 1.1)

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


pp :: View -> V3 Double -> (Point, Double)
pp v p = ((double2Float (x/w), double2Float (y/w)), z)
  where V4 x y z w = _vm v !* point p

project :: View -> Element -> Picture
project v (L3D c v1 v2) =
  if z1 > 0 && z2 > 0
  then color c' $ Line [xy1, xy2]
  else Blank
  where (xy1, z1) = pp v v1
        (xy2, z2) = pp v v2
        clipz     = _vclipz v
        c'        = if abs (z1-1) <= clipz && abs (z2-1) <= clipz
                    then c
                    else _vclipc v


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
    (\v -> screenify v <$> runCur <$> fromMaybe (return ()) <$> readMVar model)
    (\e v -> do Just c <- readIORef controller
                controllerSetRedraw c
                return $ update_view e v)
    (writeIORef controller . Just)
