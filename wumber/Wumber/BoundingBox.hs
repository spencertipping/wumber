{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Wumber.BoundingBox where

import Control.Applicative
import Data.Binary (Binary(..))
import Data.Foldable
import GHC.Generics (Generic(..))
import Lens.Micro
import Lens.Micro.TH
import Linear.V2
import Linear.V3

import Prelude hiding (all)

import Wumber.ClosedComparable


-- | An axis-aligned bounding box specified by two vectors. Each coordinate of
--   '_bmin' is no larger than the corresponding coordinate in '_bmax'.
data BoundingBox a = BB { _bmin :: !a, _bmax :: !a }
  deriving (Show, Eq, Ord, Functor, Generic, Binary)

makeLenses ''BoundingBox


instance Applicative BoundingBox where
  pure v            = BB v v
  BB f g <*> BB a b = BB (f a) (g b)


-- | A supporting instance so we can construct limit cases.
instance Bounded Double where
  minBound = -1/0
  maxBound =  1/0


type BB2D = BoundingBox (V2 Double)
type BB3D = BoundingBox (V3 Double)


-- | A bounding box that contains nothing as it is completely inverted. 'exists'
--   will return 'False' for this box, as it contains no points.
empty :: (Applicative f, Bounded a) => BoundingBox (f a)
empty = BB (pure maxBound) (pure minBound)


-- | A bounding box that contains a single point. 'exists' will return 'True'
--   for this box.
singleton :: a -> BoundingBox a
singleton = pure


-- | The size of this bounding box, as a vector.
size :: Num a => BoundingBox a -> a
size (BB l u) = u - l

{-# SPECIALIZE INLINE size :: BB3D -> V3 Double #-}
{-# SPECIALIZE INLINE size :: BB2D -> V2 Double #-}


-- | Clips a point into the box.
clip :: ClosedComparable a => BoundingBox a -> a -> a
clip (BB l u) x = upper l (lower u x)

{-# SPECIALIZE INLINE clip :: BB3D -> V3 Double -> V3 Double #-}
{-# SPECIALIZE INLINE clip :: BB2D -> V2 Double -> V2 Double #-}


-- | Returns the center point of this bounding box.
center :: Fractional a => BoundingBox a -> a
center (BB l u) = (l + u) / 2

{-# SPECIALIZE INLINE center :: BB3D -> V3 Double #-}
{-# SPECIALIZE INLINE center :: BB2D -> V2 Double #-}


-- | Determine whether two bounding boxes intersect; i.e. whether they share any
--   common points.
intersects :: (Applicative f, Foldable f, Ord a, ClosedComparable a)
           => BoundingBox (f a) -> BoundingBox (f a) -> Bool
intersects a b = exists $ intersect a b

{-# SPECIALIZE INLINE intersects :: BB3D -> BB3D -> Bool #-}
{-# SPECIALIZE INLINE intersects :: BB2D -> BB2D -> Bool #-}


-- | Returns the number of dimension which become trivial when two bounding
--   boxes are intersected. For example, two adjacent 3D cubes would produce a
--   2D rectangle, so one dimension would have been trivialized.
collapsed_dimensions :: (Applicative f, Foldable f, Ord a, ClosedComparable a)
                     => BoundingBox (f a) -> BoundingBox (f a) -> Int
collapsed_dimensions a@(BB l u) b =
  length l - nontrivial_dimensions (a `intersect` b)

{-# SPECIALIZE INLINE collapsed_dimensions :: BB3D -> BB3D -> Int #-}
{-# SPECIALIZE INLINE collapsed_dimensions :: BB2D -> BB2D -> Int #-}


-- | Returns the number of axes along which this bounding box has nonzero size.
nontrivial_dimensions :: (Applicative f, Foldable f, Ord a)
                      => BoundingBox (f a) -> Int
nontrivial_dimensions (BB l u) = foldl' (\n b -> if b then n else n+1) 0 each
  where each = liftA2 (>=) l u

{-# SPECIALIZE INLINE nontrivial_dimensions :: BB3D -> Int #-}
{-# SPECIALIZE INLINE nontrivial_dimensions :: BB2D -> Int #-}


-- | Returns 'True' if this bounding box contains any points.
exists :: (Foldable f, Applicative f, Ord a) => BoundingBox (f a) -> Bool
exists b@(BB a _) = inside b a

{-# SPECIALIZE INLINE exists :: BB3D -> Bool #-}
{-# SPECIALIZE INLINE exists :: BB2D -> Bool #-}


-- | Constructs a minimal bounding box to contain the specified list of points.
of_points :: (Foldable f, Bounded a, ClosedComparable a) => f a -> BoundingBox a
of_points ps = BB l u where l = foldl' lower maxBound ps
                            u = foldl' upper minBound ps

{-# SPECIALIZE INLINE of_points :: [V3 Double] -> BB3D #-}
{-# SPECIALIZE INLINE of_points :: [V2 Double] -> BB2D #-}


-- | Point-inside-box check. Points on the boundaries are inside.
inside :: (Foldable f, Applicative f, Ord a) => BoundingBox (f a) -> f a -> Bool
inside (BB a b) x = and lower && and upper where lower = liftA2 (<=) a x
                                                 upper = liftA2 (<=) x b

{-# SPECIALIZE INLINE inside :: BB3D -> V3 Double -> Bool #-}
{-# SPECIALIZE INLINE inside :: BB2D -> V2 Double -> Bool #-}


union :: ClosedComparable a => BoundingBox a -> BoundingBox a -> BoundingBox a
union (BB a b) (BB c d) = BB (lower a c) (upper b d)
{-# INLINE union #-}


intersect :: ClosedComparable a => BoundingBox a -> BoundingBox a -> BoundingBox a
intersect (BB a b) (BB c d) = BB (upper a c) (lower b d)
{-# INLINE intersect #-}
