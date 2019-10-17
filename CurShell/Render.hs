{-# LANGUAGE NamedFieldPuns, LambdaCase, BlockArguments, MultiWayIf #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module CurShell.Render where

import GHC.Float
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact as I
import Lens.Micro
import Linear.Matrix hiding (trace)
import qualified Linear.Metric as LM
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector
import Text.Printf

import Cur

import CurShell.View


screenify :: View -> [Element] -> Picture
screenify v = color (makeColor 0.8 0.8 0.9 0.8) . scale sz sz . pictures . map (render v')
  where sz = d2f (_vsz v)
        v' = update_cached_fields v


-- FIXME
-- This is a hamfisted and inefficient way to calculate the screen area of a
-- bounding box. We can do better with vector geometry.
--
-- We should also take clipping into account so we don't render offscreen
-- things.
screen_size :: View -> BoundingBox -> Double
screen_size v (BB vmin vmax) = foldl1 max [ LM.distance v0 v1,
                                            LM.distance v0 v2,
                                            LM.distance v0 v3 ]
  where (v0, _) = p32 v vmin
        (v1, _) = p32 v $ vmin & _x .~ vmax^._x
        (v2, _) = p32 v $ vmin & _y .~ vmax^._y
        (v3, _) = p32 v $ vmin & _z .~ vmax^._z


render :: View -> Element -> Picture
render v e | screen_size v (bb_of e) > _vlod v = render' v e
           | otherwise                         = Blank

render' :: View -> Element -> Picture
render' v (Multi bb es) = pictures $ map (render v) es

render' v (Shape bb m vs)
  | any ((<= 0) . snd) vs' = Blank
  | otherwise              = Line $ map (\(V2 x y, _) -> (d2f x, d2f y)) vs'
  where vs' = map (p32 v . inflate m) vs

render' v (Replicate bb n m e)
  | n == 0    = Blank
  | otherwise = pictures $ map (\m' -> render (v {_vm = m0 !*! m'}) e) vms
  where m0  = _vm v
        vms = scanl1 (!*!) $ replicate (n-1) m


{-# INLINE p32 #-}
p32 :: View -> V3 Double -> (V2 Double, Double)
p32 v p = (V2 (x/w) (y/w), z) where V4 x y z w = _vm v !* point p


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
