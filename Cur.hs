{-# LANGUAGE BlockArguments, TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Cur where

import Control.Monad.Identity
import Control.Monad.RWS.Strict
import Data.Map as M
import Data.Text as T
import Debug.Trace
import GHC.Float
import Graphics.Gloss
import Lens.Micro
import Lens.Micro.TH
import Linear.Matrix hiding (trace)
import Linear.V3
import Linear.V4
import Linear.Vector


{-
Let's design the state we want.

First, this thing is for 3D CAD -- so we're going for an interactive and
detailed view into a model, and we're likely to want measurements.

Some things we need:

1. Objects that occupy 3D space
2. Points with scoped names
3. Measurements between things
4. Calculated bounding boxes
5. Axis metadata
6. Viewport slicing
7. Perspective projection
8. Annotations

It would be _very_ cool if we could define data models and drop them into a
typeclass, then use lenses to access their elements.
-}

type Cur = RWST View [Picture] Cursor Identity

data View = V { _vm   :: !(M44 Double),
                _vbox :: !BoundingBox }

data Cursor = C { _cl    :: (V3 Double),
                  _cm    :: (M33 Double),
                  _cpath :: [Text],
                  _cbind :: (Map Text (V3 Double)) }

data BoundingBox = BB !(V3 Double) !(V3 Double)

makeLenses ''View
makeLenses ''Cursor
makeLenses ''BoundingBox

runCur :: Cur a -> Picture
runCur m = pictures $ snd $ execRWS m init_view init_cursor

instance Bounded Double where
  minBound = -(1/0)
  maxBound = 1/0

init_cursor :: Cursor
init_cursor = C (V3 0 0 1) identity [] M.empty

init_view :: View
init_view = V (V4 (V4 1080 0 0 0)
                  (V4 0 1080 0 0)
                  (V4 0 0 1 0)
                  (V4 0 0 1 0)) (BB minBound maxBound)


pp :: V3 Double -> Cur (Point, Double)
pp v = do
  vm <- asks _vm
  let V4 x y z w = point v *! vm
  return $ ((double2Float (x/z), double2Float (y/z)), z)


{-# INLINE bb_intersect #-}
bb_intersect :: BoundingBox -> V3 Double -> Bool
bb_intersect (BB (V3 x1 y1 z1) (V3 x2 y2 z2)) (V3 x y z) =
  x1 <= x && x <= x2 &&
  y1 <= y && y <= y2 &&
  z1 <= z && z <= z2

{-# INLINE jump #-}
jump :: V3 Double -> Cur ()
jump v = modify $ cl .~ v

{-# INLINE transform #-}
transform :: M33 Double -> Cur ()
transform m = modify $ cm %~ (!*! m)

d2r θ = θ / 180 * pi
cs θ = (cos r, sin r) where r = d2r θ

rx θ = transform (V3 (V3 1 0 0) (V3 0 c (-s)) (V3 0 s c)) where (c, s) = cs θ
ry θ = transform (V3 (V3 c 0 s) (V3 0 1 0) (V3 (-s) 0 c)) where (c, s) = cs θ
rz θ = transform (V3 (V3 c (-s) 0) (V3 s c 0) (V3 0 0 1)) where (c, s) = cs θ

bind :: Text -> Cur ()
bind t = do l <- gets _cl; modify $ cbind %~ M.insert t l

fork :: Text -> Cur a -> Cur a
fork p m = do
  c0 <- get
  modify $ cpath %~ (p :)
  v  <- m
  b  <- gets _cbind
  put $ c0 & cbind %~ M.union b
  return v

fline :: (Cursor -> V3 Double) -> Cur ()
fline f = do
  (v1, z1) <- gets _cl >>= pp
  v2 <- f <$> get
  when (z1 > 0) do
    (v2', z2') <- pp v2
    when (z2' > 0) $ tell [Line [v1, v2']]
  modify $ cl .~ v2

lx d = fline \(C cl m _ _) -> cl ^+^ m^._x ^* d
ly d = fline \(C cl m _ _) -> cl ^+^ m^._y ^* d
lz d = fline \(C cl m _ _) -> cl ^+^ m^._z ^* d


data TurtleState = TS { _ts_loc   :: !Point,
                        _ts_theta :: !Float }
makeLenses ''TurtleState

type CurM = RWST () Path TurtleState Identity

runT :: CurM a -> Picture
runT m = Line $ snd $ execRWS (do tell [(0, 0)]; m) () (TS (0, 0) 0)

fd :: Float -> CurM ()
fd d = do
  TS (x, y) t <- get
  let x' = x + d * cos t
      y' = y + d * sin t
  tell [(x', y')]
  modify $ ts_loc .~ (x', y')

rt :: Float -> CurM ()
rt d = modify $ ts_theta %~ (+ d * pi / 180)

lt :: Float -> CurM ()
lt = rt . negate
