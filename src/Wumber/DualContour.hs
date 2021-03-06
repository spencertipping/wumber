{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ConstraintKinds #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}


-- | Boundary scanning for isosurfaces, implemented by dual contouring a binary
--   bounding volume hierarchy.
module Wumber.DualContour (
  Tree(..),
  TreeMeta(..),
  IsoFn,
  IsoGradient,
  SplitFn,
  DCVector,

  t_bound, t_corners, t_size,

  iso_contour,
  build,
  trace_cells,
  trace_lines
) where


import Control.Monad.Zip     (MonadZip, mzip)
import Data.Bifoldable       (biList)
import Data.Binary           (Binary(..))
import Data.Bits             (xor, shiftL, (.&.))
import Data.Foldable         (toList)
import Data.Maybe            (isJust, fromJust)
import Data.Traversable      (traverse)
import GHC.Generics          (Generic(..))
import Lens.Micro            ((^.))
import Lens.Micro.TH         (makeLenses)
import Linear.Matrix         (identity, (!!*))
import Linear.Metric         (Metric, project, dot, distance)
import Linear.V2             (V2(..))
import Linear.V3             (V3(..))
import Linear.Vector         (Additive, lerp, (^*))
import Numeric.LinearAlgebra ((!))

import qualified Data.Sequence         as SQ
import qualified Numeric.LinearAlgebra as LA

import Wumber.BoundingBox
import Wumber.Numeric
import Wumber.VectorConversion


-- | An isoshape function you want to evaluate.
type IsoFn       a = a -> R
type IsoGradient a = a -> a


-- | Determines whether to split the specified bounding box. Arguments to
--   'SplitFn' are 'iso', 'n_splits', 'tree_meta', and 'split_axis'.
type SplitFn a = IsoFn a -> IsoGradient a -> Int -> TreeMeta a -> a -> Bool


-- | A bounding volume hierarchy with one-dimensional bisections. If dual
--   contouring is used to find the surface of an object then bisections will be
--   densest around the boundary.
data Tree a = Bisect  { _t_meta  :: !(TreeMeta a),
                        _t_axis  :: !a,
                        _t_left  :: Tree a,
                        _t_right :: Tree a }
            | Inside  { _t_meta :: !(TreeMeta a) }
            | Outside { _t_meta :: !(TreeMeta a) }
            | Surface { _t_meta :: !(TreeMeta a), _t_vertex :: !a }
  deriving (Show, Ord, Eq, Generic, Binary)

-- | Metadata stored on every tree element. We store this because we had to
--   compute it when we built the tree.
data TreeMeta a = TM { _tm_bound   :: !(BoundingBox a),
                       _tm_corners :: ![R] }
  deriving (Show, Ord, Eq, Generic, Binary)

makeLenses ''Tree
makeLenses ''TreeMeta

-- Shortcut lenses
t_bound   = t_meta . tm_bound
t_corners = t_meta . tm_corners

t_size :: Tree a -> Int
t_size (Bisect _ _ l r) = 1 + t_size l + t_size r
t_size _                = 1


-- | Traces an iso element to the specified non-surface and surface resolutions
--   and returns a list of lines to contour it.
iso_contour :: DCVector v
            => IsoFn (v R) -> IsoGradient (v R) -> BoundingBox (v R)
            -> Int -> Int -> R -> SQ.Seq (v R, v R)
iso_contour f f' b minn maxn bias = trace_lines t
  where t = build f f' b sf bias
        sf _ _ n (TM b (v:vs)) _ | any ((/= signum v) . signum) vs = n < maxn
                                 | otherwise                       = n < minn


-- | Type constraints for vectors that can be used for dual contouring.
type DCVector v = (Metric v, Traversable v, Applicative v, Fractional (v R),
                   MonadZip v,
                   VectorConversion (LA.Vector R) (v R),
                   VectorConversion (v R) (LA.Vector R))


-- | Constructs a tree whose structure is determined by the 'SplitFn'.
--
--   TODO: almost every vertex is shared by more than one cell, but we
--   re-evaluate the function instead of reusing data.
--
--   TODO: infer crossing points using normals and 'max_gradient'.
--
--   TODO: let 'sf' specify which split it wants, then bisect down to that axis.

-- TODO
-- Completely redo this. There's no reason to build the tree up front and then
-- scan it as a separate step; we'll do better if we make scanning a single
-- reentrant process.

build :: DCVector v
      => IsoFn (v R) -> IsoGradient (v R) -> BoundingBox (v R) -> SplitFn (v R)
      -> R -> Tree (v R)

build f f' b sf bias = go b (cycle basis) 0
  where go b (v:vs) n
          | sf f f' n tm v = Bisect tm v (go b1 vs (n+1)) (go b2 vs (n+1))
          | all (> 0) cfs  = Inside tm
          | all (< 0) cfs  = Outside tm
          | otherwise      = Surface tm $ surface_vertex b surface normals bias

          where tm       = TM b cfs
                (b1, b2) = bisect v b
                cs       = corners b
                cfs      = map f cs
                surface  = map (surface_point f) $ crossing_edges cs cfs
                normals  = map f' surface


-- | Shows the outline of tree cells for debugging.
trace_cells :: DCVector v => Tree (v R) -> [(v R, v R)]
trace_cells (Bisect _ _ l r) = trace_cells l ++ trace_cells r
trace_cells t                = map pair $ edge_pairs (length l)
  where b@(BB l u)  = t ^. t_bound
        cs          = corners b
        pair (i, j) = (cs !! i, cs !! j)


-- | Uses dual contouring to find lines along an isosurface. The lines end up
--   forming a closed net unless your cells are too large.
trace_lines :: DCVector v => Tree (v R) -> SQ.Seq (v R, v R)
trace_lines (Bisect _ _ l r) = go l r SQ.>< trace_lines l SQ.>< trace_lines r
  where go l r
          | not (intersects (l^.t_bound) (r^.t_bound))         = SQ.empty
          | collapsed_dimensions (l^.t_bound) (r^.t_bound) > 1 = SQ.empty
          | otherwise = case (l, r) of
              (Bisect _ _ l' r', _) -> go l' r SQ.>< go r' r
              (_, Bisect _ _ l' r') -> go l l' SQ.>< go l r'
              (Surface _ v1, Surface _ v2) -> SQ.singleton (v1, v2)
              _ -> SQ.empty

trace_lines _ = SQ.empty


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
--
--   Boris the Brave mentions that it's worthwhile to insert biasing equations
--   to pull vertices closer to the cell centers
--   (https://www.boristhebrave.com/2018/04/15/dual-contouring-tutorial/), as
--   well as clipping the chosen vertices to be properly inside their cells. I
--   do both here. A bias equation set looks like this:
--
--   @
--   bias·xx = bias·centerx
--   bias·xy = bias·centery
--   bias·xz = bias·centerz
--   @

surface_vertex :: DCVector v => BoundingBox (v R) -> [v R] -> [v R] -> R -> v R
surface_vertex b s v bias = b `clip` vconvert x
  where m = LA.fromRows $ map vconvert $ v ++ toList (identity !!* bias)
        y = LA.col $ zipWith dot s v ++ toList (center b ^* bias)
        x = head $ LA.toColumns $ LA.linearSolveSVD m y


-- | Finds the surface point of an 'IsoFn' along a bounded vector using Newton's
--   method. If that diverges then we fail over to bisection.
--
--   NOTE: arguments to 'lerp' are a little counterintuitive: 'lerp 0 a b == b'
--   and 'lerp 1 a b == a'.

surface_point :: DCVector v => IsoFn (v R) -> (v R, v R) -> v R
surface_point f (a, b) = lerp (newton 0.5) b a
  where f' = if f a > f b
             then \x -> - (f (lerp x b a))
             else \x -> f (lerp x b a)

        newton x | x' < 0 || x' > 1  = bisect_solve 0 1
                 | abs (f' x') < δ 1 = x'
                 | otherwise         = newton x'
          where x' = x - y*δf
                y  = f' x
                δx = δ x
                δf = f' (x + δx) - f' (x - δx) / (2 * δx)

        bisect_solve l u | u - l < δ m = m
                         | f' m > 0    = bisect_solve l m
                         | otherwise   = bisect_solve m u
          where m = (l + u) / 2


-- | Returns a set of basis vectors for the given vector space.
basis :: DCVector v => [v R]
basis = toList identity


-- | Returns all axis-aligned edges of a bounding box that cross a surface
--   boundary. Mathematically, this is the set of all pairs of corners that
--   differ along exactly one axis, and whose function values differ in sign.
crossing_edges :: DCVector v => [v R] -> [R] -> [(v R, v R)]
crossing_edges cs xs = map pair $ filter crosses $ edge_pairs (length $ head cs)
  where crosses (i, j) = signum (xs !! i) /= signum (xs !! j)
        pair    (i, j) = (cs !! i, cs !! j)


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


-- | Bisect a bounding box along the specified axis vector. Your warranty is
--   void if you specify a vector that isn't axis-aligned.
bisect :: DCVector v
       => v R -> BoundingBox (v R) -> (BoundingBox (v R), BoundingBox (v R))
bisect a (BB l u) = (BB l (l + mp/2 + mo), BB (l + mp/2) u)
  where d  = u - l
        mp = project a d
        mo = d - mp
