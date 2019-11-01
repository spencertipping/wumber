{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BlockArguments, TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Wumber.BoundingBox where

import Control.Applicative
import Data.Foldable
import Lens.Micro
import Lens.Micro.TH
import Linear.V2
import Linear.V3

import Prelude hiding (all)

import Wumber.ClosedComparable


-- | An axis-aligned bounding box specified by two vectors. Each coordinate of
--   '_bmin' is no larger than the corresponding coordinate in '_bmax'.
data BoundingBox a = BB { _bmin :: a, _bmax :: a }
  deriving (Show, Eq, Ord, Functor)

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


-- | Determine whether two bounding boxes intersect.
intersects :: (Applicative f, Foldable f, Ord a, ClosedComparable a)
           => BoundingBox (f a) -> BoundingBox (f a) -> Bool
intersects a b = exists $ intersect a b


-- | Returns 'True' if this bounding box contains any points.
{-# SPECIALIZE INLINE exists :: BB3D -> Bool #-}
{-# SPECIALIZE INLINE exists :: BB2D -> Bool #-}
exists :: (Foldable f, Applicative f, Ord a) => BoundingBox (f a) -> Bool
exists b@(BB a _) = inside b a


-- | Constructs a minimal bounding box to contain the specified list of points.
{-# SPECIALIZE INLINE of_points :: [V3 Double] -> BB3D #-}
{-# SPECIALIZE INLINE of_points :: [V2 Double] -> BB2D #-}
of_points :: (Foldable f, Bounded a, ClosedComparable a) => f a -> BoundingBox a
of_points ps = BB l u where l = foldl' lower maxBound ps
                            u = foldl' upper minBound ps


-- | Point-inside-box check. Points on the boundaries are inside.
inside :: (Foldable f, Applicative f, Ord a) => BoundingBox (f a) -> f a -> Bool
inside (BB a b) x = all id $ liftA2 (&&) lower upper
  where lower = liftA2 (<=) a x
        upper = liftA2 (<=) x b


{-# INLINE union #-}
union :: ClosedComparable a => BoundingBox a -> BoundingBox a -> BoundingBox a
union (BB a b) (BB c d) = BB (lower a c) (upper b d)


{-# INLINE intersect #-}
intersect :: ClosedComparable a => BoundingBox a -> BoundingBox a -> BoundingBox a
intersect (BB a b) (BB c d) = BB (upper a c) (lower b d)
