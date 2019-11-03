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

import Control.Monad.Zip (MonadZip, mzip)
import Data.Bifoldable (biList)
import Data.Foldable (toList)
import Data.Maybe (isJust, fromJust)
import Data.Traversable (traverse)
import Lens.Micro.TH (makeLenses)
import Linear.Matrix (identity)
import Linear.Metric (Metric, project)
import Linear.V2 (V2)
import Linear.V3 (V3)
import Linear.Vector (Additive, lerp)
import Numeric.LinearAlgebra (linearSolveSVD)

import Wumber.BoundingBox


type IsoFn a = a -> Double

-- | Determines whether, and if so, how, to split the specified bounding box.
--   Arguments to 'SplitFn' are 'iso', 'tree_meta', and 'default_split_axis'. To
--   split the bounding box, return 'Just split_axis'; otherwise, return
--   'Nothing'.
--
--   'default_split_axis' will cycle through the basis vectors of your vector
--   space. Returning 'Just default_split_axis' will guarantee that your
--   bisections have reasonable proportions.
type SplitFn a = IsoFn a -> TreeMeta a -> a -> Maybe a


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

-- Shortcut lenses
t_bound   = t_meta . tm_bound
t_corners = t_meta . tm_corners


-- | Constructs a tree whose structure is determined by the 'SplitFn'.
build :: (Metric v, Fractional a, Traversable v, Applicative v,
          Fractional (v a), MonadZip v)
      => IsoFn (v a) -> BoundingBox (v a) -> SplitFn (v a) -> Tree (v a)

build f b sf = go b (cycle basis)
  where go b (v:vs)
          | isJust split  = Bisect tm (go b1 vs) (go b2 vs)
          | all (>  0) cs = Inside tm
          | all (<= 0) cs = Outside tm
          | otherwise     = Surface tm v [] []
          where split    = sf f tm v
                tm       = TM b cs
                (b1, b2) = bisect (fromJust split) b
                cs       = map f $ corners b


-- | Finds the surface point of an 'IsoFn' along a bounded vector using Newton's
--   method.
surface_point :: (Additive f, Fractional a) => IsoFn (f a) -> f a -> f a -> f a
surface_point f a b = a


-- | Runs a single iteration of Newton's method on the given function.
newton_next :: (Double -> Double) -> Double -> Double
newton_next f x = x - y*δf
  where y  = f x
        δx = δ x
        δf = f (x + δx) - f (x - δx) / (2 * δx)


-- | Solves for a zero point by bisecting the function. We do this when Newton's
--   method fails. 'f' should be an /increasing/ function; if it isn't, compose
--   'negate' onto it before using this solver.
bisect_solve :: (Double -> Double) -> Double -> Double -> Double
bisect_solve f l u
  | u - l < δ m = m
  | f m > 0     = bisect_solve f l m
  | otherwise   = bisect_solve f m u
  where m = (l + u) / 2


-- | Calculates an appropriate numerical delta for the given value by
--   considering floating point precision limits. The goal is to put the delta
--   halfway into the mantissa, which for doubles is about 26 bits.
--
--   Deltas are always positive.
δ :: Double -> Double
δ x = max 1 (abs x) * 2**(-26)


-- | Returns a set of basis vectors for the given vector space.
basis :: (Num a, Traversable t, Applicative t) => [t a]
basis = toList identity
{-# SPECIALIZE INLINE basis :: [V3 Double] #-}
{-# SPECIALIZE INLINE basis :: [V2 Double] #-}


-- | Returns all 2ⁿ corners of a bounding box of dimension /n/.
--
--   NOTE to anyone reading this wondering what's up with all the class
--   qualifiers: all of them amount to stuff implemented by 'V2', 'V3', etc, but
--   I want to leave the types general across dimensionality. There's probably a
--   better way to specify these things.
corners :: (Traversable v, MonadZip v) => BoundingBox (v a) -> [v a]
corners (BB l u) = traverse biList $ l `mzip` u
{-# SPECIALIZE INLINE corners :: BoundingBox (V3 Double) -> [V3 Double] #-}
{-# SPECIALIZE INLINE corners :: BoundingBox (V2 Double) -> [V2 Double] #-}


-- | Bisect a bounding box along the specified axis vector. Your warranty is
--   void if you specify a vector that isn't axis-aligned.
bisect :: (Metric v, Fractional (v a), Fractional a)
       => v a -> BoundingBox (v a) -> (BoundingBox (v a), BoundingBox (v a))
bisect a (BB l u) = (BB l (l + mp/2 + mo), BB (l + mp/2) u)
  where d  = u - l
        mp = a `project` d
        mo = d - mp
