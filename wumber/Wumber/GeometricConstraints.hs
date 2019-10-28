{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Wumber.GeometricConstraints where

import Control.Monad
import Data.Foldable
import Lens.Micro
import Lens.Micro.TH
import Linear.Metric
import Linear.Vector

import Wumber.Constraint


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

data Rect f a = Rect { _rstart :: f a, _rsize :: f a }
makeLenses ''Rect

rend :: (Num a, Functor f, Additive f) => Lens (Rect f a) (Rect f a) (f a) (f a)
rend = lens g s where g r    =        _rstart r ^+^ _rsize r
                      s r e' = r & rstart .~ e' ^-^ _rsize r


instance Functor f => Functor (Rect f) where
  fmap f (Rect s z) = Rect (fmap f s) (fmap f z)


-- | Measure or constrain the cosine of the angle between A and C, relative to a
--   centerpoint B. 'inner_angle_cos' 'A' 'B' 'C' is 'vector_angle_cos' 'BA'
--   'BC'.
--
--   The only reason this function doesn't 'acos' for you is that it slows down
--   the solver by adding an extra step. Most of the time it's a lot faster to
--   'cos' the angle instead.
inner_angle_cos :: (Floating a, Additive f, Metric f) => f a -> f a -> f a -> a
inner_angle_cos a b c = vector_angle_cos (a ^-^ b) (c ^-^ b)

vector_angle_cos :: (Floating a, Metric f) => f a -> f a -> a
vector_angle_cos a b = a `dot` b / (norm a * norm b)


-- | Constrain a vector value to a proper rectangle (one whose size is
--   nonnegative).
inside :: (Foldable f, Additive f) => Rect f CVal -> f CVal -> Constrained ()
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
