{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Wumber.GeometricConstraints where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Lens.Micro
import Lens.Micro.TH
import Linear.Metric
import Linear.Vector

import Wumber.BoundingBox
import Wumber.ClosedComparable
import Wumber.Constraint


-- | The real circle constant. We all know π was a mistake.
τ :: Floating a => a
τ = pi * 2


-- | An N-dimensional axis-aligned rectangle defined by a corner and a size. The
--   size need not be positive, although it should be if you plan to use
--   'inside' to assert that a point is contained within the rectangle. (A
--   negatively-sized rectangle contains no points.)
--
--   Although defined only in terms of start and size, 'rend' is a lens that
--   lets you address and modify a rectangle's endpoint. Moving the endpoint
--   moves the rectangle and preserves its size.

data Rect a = Rect { _rstart :: a, _rsize :: a } deriving (Show, Eq, Functor)
makeLenses ''Rect

rend :: Num a => Lens (Rect a) (Rect a) a a
rend = lens g s where g r    =        _rstart r + _rsize r
                      s r e' = r & rstart .~ e' - _rsize r


-- | Builds a rectangle from start+end instead of start+displacement.
rect_from_ends :: Num a => a -> a -> Rect a
rect_from_ends s e = Rect s (e - s)


-- | Objects that can provide bounding boxes. These bounding boxes should always
--   be proper.
class HasBoundingBox a v | a -> v where bounds :: a -> BoundingBox v

instance (ClosedComparable a, Num a, Bounded a) => HasBoundingBox (Rect a) a where
  bounds r = of_points [r^.rstart, r^.rend]


-- | An N-dimensional line defined by start and displacement vectors. Lines
--   provide two accessor lenses: one for the endpoint (which modifies the line
--   length), and one for the line length (which modifies the endpoint by
--   scaling the displacement).

data Line a = Line { _lstart :: a, _ldisp :: a } deriving (Show, Eq, Functor)
makeLenses ''Line

lend :: Num a => Lens (Line a) (Line a) a a
lend = lens g s where g (Line s d)    = s + d
                      s (Line s d) e' = Line s $ e' - s

llen :: (Metric f, Floating a) => Lens (Line (f a)) (Line (f a)) a a
llen = lens g s where g (Line _ d)    = norm d
                      s (Line s d) l' = Line s $ d ^* (l' / norm d)

instance (ClosedComparable a, Num a, Bounded a) => HasBoundingBox (Line a) a where
  bounds l = of_points [l^.lstart, l^.lend]


-- | Measure or constrain the cosine of the angle between A and C, relative to a
--   centerpoint B. 'inner_angle_cos' 'A' 'B' 'C' is 'vector_angle_cos' 'BA'
--   'BC'.
--
--   The only reason this function doesn't 'acos' for you is that it slows down
--   the solver by adding an extra step. Most of the time it's a lot faster to
--   'cos' the angle instead.

inner_angle_cos :: (Floating a, Num (f a), Metric f) => f a -> f a -> f a -> a
inner_angle_cos a b c = vector_angle_cos (a - b) (c - b)

vector_angle_cos :: (Floating a, Metric f) => f a -> f a -> a
vector_angle_cos a b = a `dot` b / sqrt (quadrance a * quadrance b)


-- | Constrain a vector value to a proper rectangle (one whose size is
--   nonnegative).
inside :: (Num a, CEq a) => Rect a -> a -> Constrained ()
inside r p = do p >-= r^.rstart; p <-= r^.rend


-- | Asserts that a list of quantities are all equal to each other. Internally
--   this is done by setting them all equal to the first one. Although this
--   results in uneven gradients, it should result in faster solutions for most
--   systems.
all_equal :: (Foldable f, CEq a) => f a -> Constrained ()
all_equal xs = case toList xs of h:t -> forM_ t (h =-=)
                                 _   -> return ()


-- | Asserts alignment along some lens. For example, 'aligned _x [u, v, w]'
--   asserts that three points have equal 'x' coordinates.
aligned :: (CEq a, Functor l, Foldable l)
        => SimpleGetter b a -> l b -> Constrained ()
aligned g = all_equal . fmap (^.g)
