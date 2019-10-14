{-# LANGUAGE NamedFieldPuns, LambdaCase, BlockArguments, TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Cur
import qualified Data.ByteString.UTF8 as B8
import Data.IORef
import Data.Maybe
import Debug.Trace
import GHC.Float
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact as I
import Language.Haskell.Interpreter
import Lens.Micro
import Lens.Micro.TH
import Linear.Matrix hiding (trace)
import qualified Linear.Metric as LM
import Linear.V2
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
                _vsz    :: !Double,
                _vclipz :: !Double,
                _vclipc :: !Color,
                _vclipa :: !Float,
                _vhovd  :: !Float,
                _vhovc  :: !Color,
                _vmouse :: (Modifiers, Maybe MouseButton, Point) }
  deriving (Show)

makeLenses ''View

init_view :: View
init_view = V 0 0 0 1 True
              identity
              identity
              1080
              maxBound
              (makeColor 0.8 0.8 0.9 1)
              0.05
              0.01
              (makeColor 1.0 0.7 0.2 1)
              (Modifiers Up Up Up, Nothing, (0, 0))

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


f2d = float2Double
d2f = double2Float

screenify :: View -> [Element] -> Picture
screenify v = scale sz sz . pictures . map (project v')
  where sz = d2f (_vsz v)
        v' = v & vm     .~ view_matrix v
               & vrs    .~ rs_matrix v
               & vclipc .~ withAlpha (_vclipa v) (_vclipc v)

translate_rel :: V3 Double -> View -> View
translate_rel d v = v & vt %~ (^+^ inv33 (rs_matrix v) !* (d & _xy %~ (^/ _vsz v)))

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

update_view (EventKey (MouseButton b) d m@(Modifiers s c a) p)
  | b == LeftButton || b == MiddleButton || b == RightButton
    = if d == Down then vmouse .~ (m, Just b, p)
                   else vmouse .~ (m, Nothing, p)

  | c == Up = case (s, b) of
                (Up,   WheelUp)   -> vz %~ (* 1.1)
                (Up,   WheelDown) -> vz %~ (/ 1.1)
                (Down, WheelUp)   -> translate_rel $ V3 0 0   0.01
                (Down, WheelDown) -> translate_rel $ V3 0 0 (-0.01)
                _                 -> id

  | c == Down = case (s, b) of
                  (Up,   WheelUp)   -> vclipz %~ (* 1.1)
                  (Up,   WheelDown) -> vclipz %~ (/ 1.1)
                  (Down, WheelUp)   -> vclipa %~ (* 1.1)
                  (Down, WheelDown) -> vclipa %~ (/ 1.1)
                  _                 -> id

  | otherwise = id

update_view (EventMotion p@(x, y)) = \v ->
  case _vmouse v of
    (Modifiers Up Up Up, Just LeftButton, (x0, y0)) ->
      translate_rel (V3 (f2d $ x - x0) (f2d $ y - y0) 0) v
        & vmouse._3 .~ p

    (Modifiers Down Up Up, Just LeftButton, (x0, y0)) ->
      v & vry %~ (+ f2d (x0 - x) * 360 / _vsz v)
        & vrx %~ (+ f2d (y0 - y) * 360 / _vsz v)
        & vmouse._3 .~ p

    _ -> v & vmouse._3 .~ p

update_view _ = id


{-# INLINE ldist #-}
ldist :: (LM.Metric v, Floating a, Ord a) => v a -> v a -> v a -> a
ldist l1 l2 v = LM.norm $ V2 dorth dpar
  where l2'   = l2 ^-^ l1
        v'    = v  ^-^ l1
        ld    = LM.norm l2'
        pvl1  = LM.project l2' v'
        pvl2  = LM.project (l1 ^-^ l2) (v ^-^ l2)
        dorth = LM.distance v' pvl1
        dpar  = foldl1 max [0, LM.norm pvl1 - ld, LM.norm pvl2 - ld]


{-# INLINE pv #-}
pv :: (Float, Float) -> V2 Float
pv (x, y) = V2 x y

{-# INLINE pp #-}
pp :: View -> V3 Double -> (Point, Double)
pp v p = ((d2f (x/w), d2f (y/w)), z) where V4 x y z w = _vm v !* point p

project :: View -> Element -> Picture
project v (L3D c v1 v2)
  | z1 <= 0 || z2 <= 0 = Blank
  | otherwise          = color c' $ Line [xy1, xy2]

  where (xy1, z1) = pp v v1
        (xy2, z2) = pp v v2
        (_, _, m) = _vmouse v
        d         = ldist (pv xy1) (pv xy2) (pv m ^/ d2f (_vsz v))
        clipz     = _vclipz v * _vz v
        c'        = if abs (z1-1) <= clipz && abs (z2-1) <= clipz
                    then if d < _vhovd v
                         then _vhovc v
                         else c
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
