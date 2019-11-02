{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}


module Wumber.DualContour where

import Control.Applicative
import Lens.Micro
import Lens.Micro.TH
import Linear.Metric
import Linear.V3
import Linear.Vector
import Numeric.LinearAlgebra

import Wumber.BoundingBox


type IsoFn a   = a -> Double
type ErrorFn a = IsoFn a -> BoundingBox a -> Error
type Error     = Double
type Basis a   = [a]


data Tree a = Bisect a (Tree a) (Tree a)
            | Inside a
            | Outside a
            | Boundary a
  deriving (Show, Eq, Functor, Foldable)


corners :: Basis a -> BoundingBox a -> [a]
corners _ _ = []


bisect :: (Metric v, Fractional (v a), Fractional a)
       => v a -> BoundingBox (v a) -> (BoundingBox (v a), BoundingBox (v a))
bisect a (BB l u) = (BB l (l + mp/2 + mo), BB (l + mp/2) u)
  where d  = u - l
        mp = a `project` d
        mo = d - mp


-- TODO
-- Add the vertex locator logic to this function; that way we minimize function
-- re-evaluations.

build3d :: (Metric v, Fractional (v a), Fractional a)
        => IsoFn (v a) -> Basis (v a) -> BoundingBox (v a)
        -> ErrorFn (v a) -> Error -> Tree (BoundingBox (v a))
build3d f basis b ef e = go bvs b
  where bvs = cycle basis
        go (v:bvs') b
          | ef f b > e          = Bisect b (go bvs' b1) (go bvs' b2)
          | all ((>  0) . f) cs = Inside b
          | all ((<= 0) . f) cs = Outside b
          | otherwise           = Boundary b
          where (b1, b2) = bisect v b
                cs       = corners basis b





{-
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
-}
