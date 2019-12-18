{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Support for colors with semantic purpose, and to accommodate vision
--   deficiencies or other atypical spectral densities.
module Wumber.ViewColor where


import Data.Binary   (Binary)
import GHC.Generics  (Generic)
import Linear.Matrix (M33, identity)
import Linear.V3     (V3(..))

import Wumber.Numeric


-- | A color, specified one of several ways. In general you should use the
--   least-specific description that works for your use case because Wumber can
--   be configured to modify color spectra to accommodate colorblindness or
--   display deficiency (or accommodate reduced color spaces, e.g. from anaglyph
--   3D).
--
--   Color rendering modes are:
--
--   - 'ColorRGBA': please file a github issue if you find yourself using this,
--     as it gives Wumber no way to adjust color scales for people who prefer
--     reduced colorspaces or alternative color schemes. In general, Wumber
--     colors are more about semantics than display, and things like
--     'ColorScale1' are usually easier to work with.
--
--   - 'ColorScale0' @i@: a monochromatic presentation where the point's
--     importance is @i@. Wumber will render @i@ as brightness, line thickness,
--     opacity, or some mixture of those. If you just want everything in the
--     same color, set @i@ to 1.
--
--   - 'ColorScale1' @c@ @i@: a one-dimensional spectrum along the unit interval
--     @c@ with importance @i@. @c@ will be rendered in such a way that all
--     values have approximately equal visual prominence and are maximally
--     distinct for the user.
--
--   - 'ColorScale2' @c@ @d@ @i@: a two-dimensional color spectrum. Wumber will
--     choose orthogonal basis vectors within the user's preferred colorspace
--     and project @c@ and @d@ in such a way that they are jointly distinct if
--     possible. Failing that, Wumber will present one at a time or split the
--     view and present the two dimensions next to each other.
--
--   - 'ColorScale3': kinda what you think it would be.

data Color = ColorRGBA !R !R !R !R
           | ColorScale0 !R
           | ColorScale1 !R !R
           | ColorScale2 !R !R !R
           | ColorScale3 !R !R !R !R
  deriving (Show, Read, Eq, Ord, Generic, Binary)


-- | Coordinate spaces that describe the HSV perception of a given RGB color
--   given certain vision filters. 'CustomVision' lets you configure your own
--   RGB perception matrix.
--
--   'Protanomaly', 'Deuteranomaly', and 'Tritanomaly' model the three types of
--   single-cone deficiency with a unit scalar indicating admittance. So
--   @Deuteranomaly 0@ would represent deuteranopia, for example, and
--   @Deuteranomaly 1@ would be normal vision.

data ColorPerception = NormalVision
                     | Protanomaly   !R
                     | Deuteranomaly !R
                     | Tritanomaly   !R
                     | Achromatopsia
                     | CustomVision  !ColorPerceptionMatrix
  deriving (Show, Read, Eq, Ord, Generic, Binary)


-- | A color-parsing matrix for human vision. This operates as a confusion
--   matrix; the inputs are R, G, and B, and the outputs are perceived channels.
type ColorPerceptionMatrix = M33 R


-- | Returns the RGB perception matrix for a given perception model.
perception_matrix :: ColorPerception -> ColorPerceptionMatrix
perception_matrix NormalVision      = identity
perception_matrix Achromatopsia     = V3 (V3 1 1 1) (V3 1 1 1) (V3 1 1 1) / 3
perception_matrix (Protanomaly x)   = V3 (V3 x 0 0) (V3 0 1 0) (V3 0 0 1)
perception_matrix (Deuteranomaly x) = V3 (V3 1 0 0) (V3 0 x 0) (V3 0 0 1)
perception_matrix (Tritanomaly x)   = V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 x)
perception_matrix (CustomVision m)  = m
