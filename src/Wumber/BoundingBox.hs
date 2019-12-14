{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Axis-aligned bounding volumes in /n/-dimensional space.
module Wumber.BoundingBox where


import Control.Applicative (pure, (<*>), liftA2)
import Control.Monad.Zip   (MonadZip, mzip)
import Data.Bifoldable     (biList)
import Data.Binary         (Binary(..))
import Data.Foldable       (foldl')
import GHC.Generics        (Generic, Generic1)
import Lens.Micro.TH       (makeLenses)
import Linear.V2           (V2(..))
import Linear.V3           (V3(..))

import Wumber.ClosedComparable
import Wumber.Numeric


-- | Type constraints that make bounding boxes Just Work (TM) for most cases.
--   Note that we don't require 'Ord' here because we don't want to presume that
--   @a@ is a concrete type -- e.g. it may be symbolic and therefore not
--   directly comparable. That means intersection-testing won't be covered by
--   this constraint.
type BoundingBoxC v a = (Applicative v,
                         Bounded a,
                         Bounded (v a),
                         ClosedComparable a,
                         MonadZip v)


-- | Objects whose extents are known.
class BoundedObject a v where bounding_box :: a -> BoundingBox v


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

instance Bounded Float where
  minBound = -1/0
  maxBound =  1/0


type BB2D = BoundingBox (V2 R)
type BB3D = BoundingBox (V3 R)


-- | A bounding box that contains nothing as it is completely inverted. 'exists'
--   will return 'False' for this box, as it contains no points.
empty :: (Applicative f, Bounded a) => BoundingBox (f a)
empty = BB (pure maxBound) (pure minBound)


-- | A bounding box that contains every point.
everything :: (Applicative f, Bounded a) => BoundingBox (f a)
everything = BB (pure minBound) (pure maxBound)


-- | A bounding box that contains a single point. 'exists' will return 'True'
--   for this box.
singleton :: a -> BoundingBox a
singleton = pure


-- | The size of this bounding box, as a vector.
size :: Num a => BoundingBox a -> a
size (BB l u) = u - l


-- | Clips a point into the box.
clip :: ClosedComparable a => BoundingBox a -> a -> a
clip (BB l u) x = upper l (lower u x)


-- | Returns the center point of this bounding box.
center :: Fractional a => BoundingBox a -> a
center (BB l u) = (l + u) / 2


-- | Determine whether two bounding boxes intersect; i.e. whether they share any
--   common points.
intersects :: (Applicative f, Foldable f, Ord a, ClosedComparable a)
           => BoundingBox (f a) -> BoundingBox (f a) -> Bool
intersects a b = exists $ intersect a b


-- | Returns all 2ⁿ corners of a bounding box of dimension /n/.
corners :: (MonadZip v, Traversable v) => BoundingBox (v a) -> [v a]
corners (BB l u) = traverse biList $ l `mzip` u


-- | Returns the number of dimensions which become trivial when two bounding
--   boxes are intersected. For example, two adjacent 3D cubes would produce a
--   2D rectangle, so one dimension would have been trivialized.
collapsed_dimensions :: (Applicative f, Foldable f, Ord a, ClosedComparable a)
                     => BoundingBox (f a) -> BoundingBox (f a) -> Int
collapsed_dimensions a@(BB l u) b =
  length l - nontrivial_dimensions (a `intersect` b)


-- | Returns the number of axes along which this bounding box has nonzero size.
nontrivial_dimensions :: (Applicative f, Foldable f, Ord a)
                      => BoundingBox (f a) -> Int
nontrivial_dimensions (BB l u) = foldl' (\n b -> if b then n else n+1) 0 each
  where each = liftA2 (>=) l u


-- | Returns 'True' if this bounding box contains any points.
exists :: (Foldable f, Applicative f, Ord a) => BoundingBox (f a) -> Bool
exists b@(BB a _) = inside b a


-- | Constructs a minimal bounding box to contain the specified list of points.
of_points :: (Foldable f, Bounded a, ClosedComparable a) => f a -> BoundingBox a
of_points ps = BB l u where l = foldl' lower maxBound ps
                            u = foldl' upper minBound ps


-- | Point-inside-box check. Points on the boundaries are inside.
inside :: (Foldable f, Applicative f, Ord a) => BoundingBox (f a) -> f a -> Bool
inside (BB a b) x = and lower && and upper where lower = liftA2 (<=) a x
                                                 upper = liftA2 (<=) x b


union :: ClosedComparable a => BoundingBox a -> BoundingBox a -> BoundingBox a
union (BB a b) (BB c d) = BB (lower a c) (upper b d)

unions = foldl' union empty


intersect :: ClosedComparable a => BoundingBox a -> BoundingBox a -> BoundingBox a
intersect (BB a b) (BB c d) = BB (upper a c) (lower b d)

intersections = foldl' intersect everything
