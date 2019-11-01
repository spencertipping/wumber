{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}


-- NOTE
-- This code is inspired heavily by Matt Keeter's implementation at
-- https://www.mattkeeter.com/projects/contours/. I've extended it to 3D, but
-- otherwise most of the design is the same.

module Wumber.DualContour where

import Numeric.LinearAlgebra (linearSolveSVD)
import Lens.Micro
import Lens.Micro.TH
import Linear.V3

import Wumber.BoundingBox


type IsoFn = V3 Double -> Double


-- TODO
-- Generalize this across dimensionality so we can reuse the logic for 2D isos.
-- Haskell should give us a fairly straightforward way to do this.

data Tree a = Root { _txyz :: Tree a, _txyZ :: Tree a,
                     _txYz :: Tree a, _txYZ :: Tree a,
                     _tXyz :: Tree a, _tXyZ :: Tree a,
                     _tXYz :: Tree a, _tXYZ :: Tree a }
            | Empty
            | Full
            | Leaf a
  deriving (Show, Eq)

makeLenses ''Tree


build_tree :: BB3D -> Int -> Tree BB3D
build_tree b 0 = Leaf b
build_tree (BB (V3 xl yl zl) (V3 xu yu zu)) i =
  Root (build_tree (BB (V3 xl yl zl) (V3 xm ym zm)) (i-1))
       (build_tree (BB (V3 xl yl zm) (V3 xm ym zu)) (i-1))
       (build_tree (BB (V3 xl ym zl) (V3 xm yu zm)) (i-1))
       (build_tree (BB (V3 xl ym zm) (V3 xm yu zu)) (i-1))
       (build_tree (BB (V3 xm yl zl) (V3 xu ym zm)) (i-1))
       (build_tree (BB (V3 xm yl zm) (V3 xu ym zu)) (i-1))
       (build_tree (BB (V3 xm ym zl) (V3 xu yu zm)) (i-1))
       (build_tree (BB (V3 xm ym zm) (V3 xu yu zu)) (i-1))
  where xm = (xl + xu) / 2
        ym = (yl + yu) / 2
        zm = (zl + zu) / 2


collapse :: IsoFn -> Tree BB3D -> Tree BB3D
collapse i l@(Leaf (BB (V3 xl yl zl) (V3 xu yu zu)))
  | all (<= 0) corners = Empty
  | all (>  0) corners = Full
  | otherwise          = l
  where corners = map i [V3 x y z | x <- [xl, xu], y <- [yl, yu], z <- [zl, zu]]

collapse i (Root a b c d e f g h)
  | all (== Empty) s' = Empty
  | all (== Full)  s' = Full
  | otherwise         = let [a', b', c', d', e', f', g', h'] = s' in
                          Root a' b' c' d' e' f' g' h'
  where s' = map (collapse i) [a, b, c, d, e, f, g, h]

collapse _ t = t


δ = 2**(-12)

gradient :: IsoFn -> V3 Double -> V3 Double
gradient i (V3 x y z) = V3 (gx/δ) (gy/δ) (gz/δ)
  where gx = i (V3 (x+δ) y z) - i (V3 (x-δ) y z)
        gy = i (V3 x (y+δ) z) - i (V3 x (y-δ) z)
        gz = i (V3 x y (z+δ)) - i (V3 x y (z-δ))
