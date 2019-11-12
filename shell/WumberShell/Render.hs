{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module WumberShell.Render where

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

import Wumber

import WumberShell.View


screenify :: View -> [Element] -> Picture
screenify v = color init_color . scale sz sz . pictures . map (render v')
  where sz         = d2f (_vsz v)
        v'         = update_cached_fields v
        init_color = makeColor 0.8 0.8 0.9 0.8


-- NOTE
-- This is a hamfisted and inefficient way to calculate the screen area of a
-- bounding box. We can do better with Math (TM).
--
-- We should also take clipping into account so we don't render offscreen
-- things.
screen_size :: View -> BB3D -> Double
screen_size v (BB vmin vmax) = foldl1 max [ LM.distance v0 v1,
                                            LM.distance v0 v2,
                                            LM.distance v0 v3 ]
  where (v0, _) = p32 v vmin
        (v1, _) = p32 v $ vmin & _x .~ vmax^._x
        (v2, _) = p32 v $ vmin & _y .~ vmax^._y
        (v3, _) = p32 v $ vmin & _z .~ vmax^._z


-- TODO
-- Should we have a render monad? There are some reasons we might want this; for
-- example, we could drop Blank elements. We could also provide a more
-- structured approach to 3D-transform stuff before putting it onscreen; this
-- would help our Replicate logic out a bit.
--
-- Finally, I think we want a deeper context for things like "which element is
-- in focus". I can see a world where we have CLI-driven search and model
-- manipulation.


-- TODO
-- What kind of interfacing do we want to be able to interact with elements?
-- Should elements be able to observe things like the mouse position, or should
-- we reduce the interface to "you're focused" etc?
--
-- Let's figure out what we want to be able to do.
--
-- Ultimately, it's about a couple of things. First, we want to be able to view
-- things from different perspectives, enable measurements, that type of thing.
-- CAD-focused features for people to build stuff.
--
-- Second, and more interestingly, we want aspects of the model to interact with
-- degrees of freedom (likely via lenses). So we might have a pre-made component
-- like a hinge that provides actuation based on user interaction. Or maybe we
-- have a slide, etc. Then the user can manipulate the state of the model while
-- they're looking at it.
--
-- There are some other use cases like exploded views that are also worth
-- considering: maybe we have an "exploded axis" pseudo-component that shows the
-- travel path of each linear element.
--
-- The other big thing is that I think cursors should have a way to emit
-- view-planes that show detail for different parts. I'm not sure how this
-- should work. Are these view-planes also interactive? Do we show
-- rulers/grids/etc? Do we decompose things into subassemblies with different
-- manufacturing instructions, break stuff down into steps?

render :: View -> Element -> Picture
{-
render v e | screen_size v (bb_of e) > _vlod v = render' v e
           | otherwise                         = Blank
-}
render = render'

render' :: View -> Element -> Picture
render' v (Multi bb es) = pictures $ map (render v) es

render' v (Shape bb m vs) = line_from $ map (p32 v . inflate m) vs

render' v (Replicate bb n mb m e)
  | n == 0    = Blank
  | otherwise = pictures $ layers ++ connect

  where m0      = _vm v !*! mb
        vs      = vertices e
        vms     = scanl (!*!) m0 $ replicate (n-1) m
        layers  = map (\m' -> render (v {_vm = m'}) e) vms
        vmpairs = tail $ scanl (\(_, x) y -> (x, y)) (m0, m0) vms

        cline (a, b)  = line_from [a, b]
        zipped (m, n) = map (p32 $ v {_vm = m}) vs `zip`
                        map (p32 $ v {_vm = n}) vs
        connect = concatMap (map cline . zipped) vmpairs


{-# INLINE line_from #-}
line_from :: [(V2 Double, Double)] -> Picture
line_from vs
  | any ((<= 0) . snd) vs = Blank
  | otherwise             = Line $ map (\(V2 x y, _) -> (d2f x, d2f y)) vs


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
