{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}


module Wumber.DualContour where

import Control.Monad.Zip     (MonadZip, mzip)
import Data.Bifoldable       (biList)
import Data.Bits             (xor, shiftL, (.&.))
import Data.Foldable         (toList)
import Data.Maybe            (isJust, fromJust)
import Data.Traversable      (traverse)
import Lens.Micro.TH         (makeLenses)
import Linear.Matrix         (identity)
import Linear.Metric         (Metric, project, dot, distance)
import Linear.V2             (V2(..))
import Linear.V3             (V3(..))
import Linear.Vector         (Additive, lerp, (^*))
import Numeric.LinearAlgebra ((!))

import qualified Linear.V as V
import qualified Numeric.LinearAlgebra as LA

import Wumber.BoundingBox
import Wumber.Element

import Debug.Trace


-- Isofunctions for testing
sphere :: V3 R -> IsoFn (V3 R)
sphere l v = 1 - distance v l

cube :: BB3D -> IsoFn (V3 R)
cube (BB (V3 x1 y1 z1) (V3 x2 y2 z2)) (V3 x y z) =
  foldl1 min [ x - x1, x2 - x, y - y1, y2 - y, z - z1, z2 - z ]


iunion     f g v = max (f v) (g v)
iintersect f g v = min (f v) (g v)
inegate    f v   = negate (f v)


type R       = Double
type IsoFn a = a -> R


-- | Determines whether to split the specified bounding box. Arguments to
--   'SplitFn' are 'iso', 'tree_meta', and 'default_split_axis'.
type SplitFn a = IsoFn a -> TreeMeta a -> a -> Bool


-- | A bounding volume hierarchy with one-dimensional bisections. If dual
--   contouring is used to find the surface of an object then bisections will be
--   densest around the boundary.
--
--   NOTE: each 'Bisect' node stores its axis, and the "right" side will always
--   dot higher along the axis than the "left" side. This invariant is important
--   for meshing, which happens in the 'outline' function.
--
--   NOTE: '_t_axis' must always be a unit basis vector; that is, exactly one
--   component should be 1 and the others should be 0. If this isn't true,
--   'outline' will fail unpredictably.

data Tree a = Bisect  { _t_meta  :: !(TreeMeta a),
                        _t_axis  :: a,
                        _t_left  :: Tree a,
                        _t_right :: Tree a }
            | Inside  { _t_meta :: !(TreeMeta a) }
            | Outside { _t_meta :: !(TreeMeta a) }
            | Surface { _t_meta :: !(TreeMeta a), _t_vertex :: a }
  deriving (Show, Eq)

-- | Metadata stored on every tree element. We store this because we had to
--   compute it when we built the tree.
data TreeMeta a = TM { _tm_bound   :: !(BoundingBox a),
                       _tm_corners :: ![R] }
  deriving (Show, Eq)

makeLenses ''Tree
makeLenses ''TreeMeta

-- Shortcut lenses
t_bound   = t_meta . tm_bound
t_corners = t_meta . tm_corners


-- | Traces an iso element to the specified resolution and returns an element to
--   represent it.
iso_contour :: IsoFn (V3 R) -> BB3D -> R -> Element
iso_contour f b r = trace (show (length o)) $ lines o
  where o               = outline t
        t               = build f b sf
        lines           = multi_of . map (\(v1, v2) -> shape_of identity [v1, v2])
        sf _ (TM b _) _ = any (>= r) $ size b

-- | Constructs a tree whose structure is determined by the 'SplitFn'.
build :: (Metric v, Traversable v, Applicative v, Fractional (v R), MonadZip v,
          FromStorableVector (v R))
      => IsoFn (v R) -> BoundingBox (v R) -> SplitFn (v R) -> Tree (v R)

build f b sf = go b (cycle basis)
  where go b (v:vs)
          | sf f tm v      = Bisect tm v (go b1 vs) (go b2 vs)
          | all (>  0) cfs = Inside tm
          | all (<= 0) cfs = Outside tm
          | otherwise      = Surface tm $ surface_vertex b surface normals

          where tm       = TM b cfs
                (b1, b2) = bisect v b
                cs       = corners b
                cfs      = map f cs
                surface  = map (surface_point f) $ crossing_edges cs cfs
                normals  = map (gradient f) surface

{-# SPECIALIZE build :: IsoFn (V3 R) -> BB3D -> SplitFn (V3 R) -> Tree (V3 R) #-}
{-# SPECIALIZE build :: IsoFn (V2 R) -> BB2D -> SplitFn (V2 R) -> Tree (V2 R) #-}


-- | Traces the surface of an isofn with lines. The idea here is to connect
--   vertices within bounding boxes that are adjacent to where we are. I'm using
--   an algorithm similar to the one by Matt Keeter in his blog post:
--
--   https://www.mattkeeter.com/projects/contours/
--
--   Our bookkeeping is a little different because we don't have a
--   statically-known number of dimensions to work with. That means instead of
--   having directional sub-functions, we need to generalize to geometric
--   properties.
--
--   It turns out this isn't very difficult. The basic premise is that if we
--   have a surface cell, we can connect it to any other surface cell that
--   intersects its bounding box. We can find the set of intersecting cells
--   quickly using the tree structure.
--
--   For simplicity and performance, we don't handle cases with degenerate
--   intersections, e.g. a corner point shared between rectangles or cubes. This
--   will cause defects in surfaces that are positioned at 45° and perfectly
--   aligned with the bounding structure.

outline :: (Foldable v, Eq (v R), Num (v R)) => Tree (v R) -> [(v R, v R)]
outline (Bisect _ a l r) = outline' a l r
outline _                = []

outline' :: (Foldable v, Eq (v R), Num (v R))
         => v R -> Tree (v R) -> Tree (v R) -> [(v R, v R)]
outline' a l r
  | any (> 1) a = []
  | otherwise   = case (l, r) of
      (Surface _ v1, Surface _ v2) -> [(v1, v2)]

      (Surface _ _,  Bisect _ a' l' r') ->
        outline' a l l' ++ outline' (a + a') l r'

      (Bisect _ a' l' r', Surface _ _) ->
        outline' (a + a') l' r ++ outline' a r' r

      -- The complicated case: connect within each bisection (easy), then find
      -- cells that bridge bisections and have nontrivial intersections. We know
      -- up front that the bisections share a bounding surface along axis 'a',
      -- and that left and right are ordered along that axis.
      (Bisect _ a' l1 r1, Bisect _ _ l2 r2) ->
        -- Adjacent because they share a Bisect node
        outline' a' l1 r1 ++
        outline' a' l2 r2 ++

        -- If a' == a, then we're in a one-dimensional system and don't have any
        -- crossings. Otherwise l1 and l2 are connected along axis 'a', as are
        -- r1 and r2.
        outline' a l1 l2 ++ outline' a r1 r2

      -- Inside/outside aren't connected to anything.
      _ -> []

{-# SPECIALIZE outline :: Tree (V3 R) -> [(V3 R, V3 R)] #-}
{-# SPECIALIZE outline :: Tree (V2 R) -> [(V2 R, V2 R)] #-}

{-# SPECIALIZE outline' :: V3 R -> Tree (V3 R) -> Tree (V3 R) -> [(V3 R, V3 R)] #-}
{-# SPECIALIZE outline' :: V2 R -> Tree (V2 R) -> Tree (V2 R) -> [(V2 R, V2 R)] #-}


-- | Locates the vertex within a 'Surface' cell. We do this by minimizing an
--   overspecified set of linear equations describing the distance between each
--   normal plane and the vertex in question.
--
--   If this function is called from 'build', then we're guaranteed to have the
--   vertex at least be fully specified. We know this because any edge that
--   crosses the surface will have a vertex that produces /n/ surface
--   intersections, where /n/ is the number of dimensions. So we don't need to
--   do any checking here, nor do we have any degenerate output cases.
--
--   The linear system is built from dot products: if we have a surface point
--   /s/ and a normal vector /v/, then we want to choose /x/ such that
--   '(s - x) · v == 0'. Rewriting in scalar terms (for instance, in three
--   dimensions), we get this for each equation:
--
--   @
--   xx·vx + xy·vy + xz·vz = sx·vx + sy·vy + sz·vz
--   @
--
--   The right-hand side collapses to a constant value.

surface_vertex :: (Metric f, Foldable f, FromStorableVector (f R))
               => BoundingBox (f R) -> [f R] -> [f R] -> f R
surface_vertex b s v = from_storable_vector x
  where m = LA.fromRows $ map (LA.fromList . toList) v
        y = LA.col $ zipWith dot s v
        x = head $ LA.toColumns $ LA.linearSolveSVD m y


-- | Things that can be converted from the storable vectors used by
--   'Numeric.LinearAlgebra'. I'm using this because I couldn't find a way to
--   convert storable vectors back to things like 'V2' or 'V3'.
--
--   If anyone knows of a better way to solve this problem, please let me know.
class FromStorableVector a where from_storable_vector :: LA.Vector R -> a

instance FromStorableVector (V3 R) where
  from_storable_vector v = V3 (v!0) (v!1) (v!2)

instance FromStorableVector (V2 R) where
  from_storable_vector v = V2 (v!0) (v!1)


-- | Finds the surface point of an 'IsoFn' along a bounded vector using Newton's
--   method. If that diverges then we fail over to bisection.
--
--   NOTE: arguments to 'lerp' are a little counterintuitive: 'lerp 0 a b == b'
--   and 'lerp 1 a b == a'.
surface_point :: Additive f => IsoFn (f R) -> (f R, f R) -> f R
surface_point f (a, b) = lerp (newton 0.5) b a
  where f' = if f a > f b
             then \x -> - (f (lerp x b a))
             else \x -> f (lerp x b a)
        newton x = let x' = newton_next x in
                     if | x' < 0 || x' > 1  -> bisect_solve 0 1
                        | abs (f' x') < δ 1 -> x'
                        | otherwise         -> newton x'

        newton_next x = x - y*δf
          where y  = f' x
                δx = δ x
                δf = f' (x + δx) - f' (x - δx) / (2 * δx)

        bisect_solve l u
          | u - l < δ m = m
          | f' m > 0    = bisect_solve l m
          | otherwise   = bisect_solve m u
          where m = (l + u) / 2


-- | Calculates an appropriate numerical delta for the given value by
--   considering floating point precision limits. The goal is to put the delta
--   halfway into the mantissa, which for doubles is about 26 bits.
--
--   Deltas are always positive.
--
--   NOTE: we want to fix these types to 'Double' instead of using 'R'. That way
--   the code will break if you change 'R', which is correct -- the delta would
--   need to be updated.

δ :: Double -> Double
δ x = max 1 (abs x) * 2**(-26)


-- | Returns the /n/-dimensional gradient vector of the isofunction at a given
--   point.
gradient :: (Traversable f, Applicative f, Num (f R))
         => IsoFn (f R) -> f R -> f R
gradient f v = sum [diff b | b <- basis]
  where δx     = δ 1            -- NOTE: suboptimal (should use vector coords)
        diff b = b ^* ((f (v + b^*δx) - f (v - b^*δx)) / (2*δx))

{-# SPECIALIZE INLINE gradient :: IsoFn (V3 R) -> V3 R -> V3 R #-}
{-# SPECIALIZE INLINE gradient :: IsoFn (V2 R) -> V2 R -> V2 R #-}


-- | Returns a set of basis vectors for the given vector space.
basis :: (Num a, Traversable t, Applicative t) => [t a]
basis = toList identity

{-# SPECIALIZE INLINE basis :: [V3 R] #-}
{-# SPECIALIZE INLINE basis :: [V2 R] #-}


-- | Returns all 2ⁿ corners of a bounding box of dimension /n/.
--
--   NOTE to anyone reading this wondering what's up with all the class
--   qualifiers: all of them amount to stuff implemented by 'V2', 'V3', etc, but
--   I want to leave the types general across dimensionality. There's probably a
--   better way to specify these things.
corners :: (Traversable v, MonadZip v) => BoundingBox (v a) -> [v a]
corners (BB l u) = traverse biList $ l `mzip` u

{-# SPECIALIZE INLINE corners :: BoundingBox (V3 R) -> [V3 R] #-}
{-# SPECIALIZE INLINE corners :: BoundingBox (V2 R) -> [V2 R] #-}


-- | Returns all axis-aligned edges of a bounding box that cross a surface
--   boundary. Mathematically, this is the set of all pairs of corners that
--   differ along exactly one axis, and whose function values differ in sign.
crossing_edges :: Foldable f => [f a] -> [R] -> [(f a, f a)]
crossing_edges cs xs = map pair $ filter crosses $ edge_pairs (length $ head cs)
  where crosses (i, j) = signum (xs !! i) /= signum (xs !! j)
        pair    (i, j) = (cs !! i, cs !! j)

{-# INLINE crossing_edges #-}


-- | /n/-dimensional cubes have n·2ⁿ⁻¹ edges: each vertex has /n/ edges
--   connected to it, there are 2ⁿ vertices, and each edge is covered twice
--   (once by each of its endpoint vertices). Our corners are always
--   consistently ordered, so we can find adjacent points by using list offsets.
--   In our case, those offsets are simple: they're the bit-xor of successive
--   powers of two.
edge_pairs :: Int -> [(Int, Int)]
edge_pairs n = [(i, i `xor` shiftL 1 x) | i <- [0..(1 `shiftL` (n-1))],
                                          x <- [0..(n-1)],
                                          i .&. shiftL 1 x == 0]
{-# INLINE edge_pairs #-}


-- | Bisect a bounding box along the specified axis vector. Your warranty is
--   void if you specify a vector that isn't axis-aligned.
bisect :: (Metric v, Fractional (v a), Fractional a)
       => v a -> BoundingBox (v a) -> (BoundingBox (v a), BoundingBox (v a))
bisect a (BB l u) = (BB l (l + mp/2 + mo), BB (l + mp/2) u)
  where d  = u - l
        mp = a `project` d
        mo = d - mp
