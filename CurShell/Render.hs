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


-- NOTE
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


-- FIXME
-- We should render to something intermediate instead of Picture; Picture loses
-- Z info and doesn't provide any association, which means we can't handle mouse
-- interaction.
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
  | otherwise = pictures $ render v e : layers ++ connect
  where m0      = _vm v
        vs      = vertices e
        vms     = map (m0 !*!) $ scanl1 (!*!) $ replicate (n-1) m
        layers  = map (\m' -> render (v {_vm = m'}) e) vms
        vmpairs = tail $ scanl (\(_, x) y -> (x, y)) (m0, m0) vms

        bothzok ((_, z1), (_, z2)) = z1 > 0 && z2 > 0
        zipped (m, n) = filter bothzok $ map (p32 $ v {_vm = m}) vs `zip`
                                         map (p32 $ v {_vm = n}) vs

        cline ((V2 x1 y1, _), (V2 x2 y2, _)) = Line [(d2f x1, d2f y1),
                                                     (d2f x2, d2f y2)]
        connect = concatMap (map cline . zipped) vmpairs


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
