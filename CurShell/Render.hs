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


render :: View -> Element -> Picture
render v (L3D c v1 v2)
  | z1 <= 0 || z2 <= 0 = Blank
  | otherwise          = color c' $ Line [xy1, xy2]

  where (xy1, z1) = p32 v v1
        (xy2, z2) = p32 v v2
        (_, _, m) = _vmouse v

        pv (x, y) = V2 x y
        d         = ldist (pv xy1) (pv xy2) (pv m ^/ d2f (_vsz v))
        clipz     = _vclipz v * _vz v
        c'        = if | abs (z1-1) > clipz || abs (z2-1) >= clipz -> _vclipc v
                       | d < _vhovd v                              -> _vhovc v
                       | otherwise                                 -> c


{-# INLINE p32 #-}
p32 :: View -> V3 Double -> (Point, Double)
p32 v p = ((d2f (x/w), d2f (y/w)), z) where V4 x y z w = _vm v !* point p


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
