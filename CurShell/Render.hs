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
screenify v = scale sz sz . pictures . map (render v')
  where sz = d2f (_vsz v)
        v' = v & vm     .~ view_matrix v
               & vrs    .~ rs_matrix v
               & vclipc .~ withAlpha (_vclipa v) (_vclipc v)


screen_size :: View -> BoundingBox -> Double
screen_size v (BB vmin vmax) = foldl1 max [ LM.distance v0 v1,
                                            LM.distance v0 v2,
                                            LM.distance v0 v3 ]
  where (v0, _) = p32 v vmin
        (v1, _) = p32 v $ vmin & _x .~ vmax^._x
        (v2, _) = p32 v $ vmin & _y .~ vmax^._y
        (v3, _) = p32 v $ vmin & _z .~ vmax^._z


render :: View -> Element -> Picture
render v e | screen_size v (bb_of e) > 0.01 = render' v e
           | otherwise                      = Blank

render' :: View -> Element -> Picture
render' v (Multi bb es) = pictures $ map (render v) es
render' v (L3D c v1 v2)
  | z1 <= 0 || z2 <= 0 = Blank
  | otherwise          = color c' $ Line [vp v1', vp v2']

  where (v1', z1)        = p32 v v1
        (v2', z2)        = p32 v v2
        (_, _, (mx, my)) = _vmouse v

        vp (V2 x y) = (d2f x, d2f y)
        d           = ldist v1' v2' (V2 (f2d mx) (f2d my) ^/ _vsz v)
        clipz       = _vclipz v * _vz v
        c'          = if | abs (z1-1) > clipz || abs (z2-1) >= clipz -> _vclipc v
                         | d < _vhovd v                              -> _vhovc v
                         | otherwise                                 -> c


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
