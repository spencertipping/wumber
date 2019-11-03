{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}


module Wumber.DualContour where

import Control.Applicative
import Control.Monad.Zip (MonadZip, mzip)
import Data.Bifoldable (biList)
import Data.Foldable (toList)
import Data.Traversable (traverse)
import Lens.Micro
import Lens.Micro.TH
import Linear.Matrix
import Linear.Metric
import Linear.V2
import Linear.V3
import Linear.Vector
import Numeric.LinearAlgebra (linearSolveSVD)

import Wumber.BoundingBox


type IsoFn a   = a -> Double
type SplitFn a = IsoFn a -> BoundingBox a -> a -> [Double] -> Bool
type Basis a   = [a]


-- | A bounding volume hierarchy with one-dimensional bisections. If dual
--   contouring is used to find the surface of an object then bisections will be
--   densest around the boundary.
data Tree a = Bisect  { _t_meta  :: !(TreeMeta a),
                        _t_left  :: Tree a,
                        _t_right :: Tree a }
            | Inside  { _t_meta :: !(TreeMeta a) }
            | Outside { _t_meta :: !(TreeMeta a) }
            | Surface { _t_meta          :: !(TreeMeta a),
                        _t_vertex        :: a,
                        _t_intersections :: [a],
                        _t_normals       :: [a] }
  deriving (Show, Eq)

-- | Metadata stored on every tree element. We store this because we had to
--   compute it when we built the tree.
data TreeMeta a = TM { _tm_bound   :: !(BoundingBox a),
                       _tm_corners :: ![Double] }
  deriving (Show, Eq)

makeLenses ''Tree
makeLenses ''TreeMeta


-- | Constructs a tree for the given isofunction, guided by the output of the
--   'SplitFn'. 
build :: (Metric v, Fractional a, Traversable v, Applicative v,
          Fractional (v a), MonadZip v)
      => IsoFn (v a) -> BoundingBox (v a) -> SplitFn (v a) -> Tree (v a)

build f b sf = go bvs b
  where bvs = cycle basis
        go (v:bvs') b
          | sf f b v cs   = Bisect tm (go bvs' b1) (go bvs' b2)
          | all (>  0) cs = Inside tm
          | all (<= 0) cs = Outside tm
          | otherwise     = Surface tm v [] []
          where tm       = TM b cs
                (b1, b2) = bisect v b
                basis    = toList identity
                cs       = map f $ corners b


-- | Returns all 2ⁿ corners of a bounding box of dimension /n/.
--
--   NOTE to anyone reading this wondering what's up with all the class
--   qualifiers: all of them amount to stuff implemented by 'V2', 'V3', etc, but
--   I want to leave the types general across dimensionality. There's probably a
--   better way to specify these things.
corners :: (Traversable v, MonadZip v) => BoundingBox (v a) -> [v a]
corners (BB l u) = traverse biList $ l `mzip` u


-- | Bisect a bounding box along the specified axis vector. Your warranty is
--   void if you specify a vector that isn't axis-aligned.
bisect :: (Metric v, Fractional (v a), Fractional a)
       => v a -> BoundingBox (v a) -> (BoundingBox (v a), BoundingBox (v a))
bisect a (BB l u) = (BB l (l + mp/2 + mo), BB (l + mp/2) u)
  where d  = u - l
        mp = a `project` d
        mo = d - mp
