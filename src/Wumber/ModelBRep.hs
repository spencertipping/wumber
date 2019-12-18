{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Objects that can describe themselves in terms of a boundary representation,
--   and geometric primitives to represent object boundaries.
--
--   Generally, BRep objects don't support CSG because calculating it this way
--   is slow and awkward. Instead, BRep is used in conjunction with FRep: FRep
--   for computational geometry and BRep as a projection for rendering.
--
--   Although boundary representations of solids are always closed loops, we
--   don't store them in a way that requires this to be the case: you could have
--   a solid with multiple independent loops, or you could have an object
--   modeled as a shell rather than as a solid. BRep is intentionally flexible
--   about this, as it's designed for rendering rather than for computation.

module Wumber.ModelBRep where


import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import Linear.V4 (V4(..))

import Wumber.Affine
import Wumber.Numeric


-- | The class of objects that can represent themselves in terms of boundary
--   elements.
class BReppable a v | a -> v where brep :: a -> [BElement v R]

-- FIXME
-- The above typeclass won't cut it at all because we need a way to vary LOD by
-- effective viewport distance, and ideally we'd also have some sort of space
-- partitioning.


-- | An /n/-dimensional boundary element specified as a simplex of some
--   dimension. For example, a 2D line would be 'BElement' 'V2' 'R'.
type BElement v n = v (v n)

type B2Line = BElement V2 R         -- ^ Type alias for a 2D bounding element
type B3Tri  = BElement V3 R         -- ^ Type alias for a 3D bounding element
type B4Tet  = BElement V4 R         -- ^ Type alias for a 4D bounding element
