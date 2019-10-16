{-# LANGUAGE NamedFieldPuns, LambdaCase, BlockArguments, TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module CurShell.View where

import GHC.Float
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact as I
import Lens.Micro
import Lens.Micro.TH
import Linear.Matrix hiding (trace)
import qualified Linear.Metric as LM
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector
import Text.Printf

import Cur


-- | NOTE
--   Not all fields here are canonical; '_vm', '_vrs', and the alpha component
--   of '_vclipc' are all calculated from other values. Call
--   'update_cached_fields' to get a 'View' with correctly computed values (e.g.
--   before you use it to render anything).
data View = V { _vt     :: !(V3 Double),    -- view settings
                _vry    :: !Double,
                _vrx    :: !Double,
                _vz     :: !Double,         -- zoom
                _vp     :: !Bool,           -- perspective (True) / ortho (False)
                _vlod   :: !Double,         -- level of detail (0 = everything)

                _vrs    :: !(M33 Double),   -- stored matrices
                _vm     :: !(M44 Double),

                _vclipz :: !Double,         -- clipping
                _vclipc :: !Color,
                _vclipa :: !Float,

                _vhovd  :: !Double,         -- hover interaction
                _vhovc  :: !Color,

                _vsz    :: !Double,         -- screen size/interaction
                _vmouse :: (Modifiers, Maybe MouseButton, Point) }
  deriving (Show)

makeLenses ''View


update_cached_fields :: View -> View
update_cached_fields v = v & vm     .~ view_matrix v
                           & vrs    .~ rs_matrix v
                           & vclipc .~ withAlpha (_vclipa v) (_vclipc v)


init_view :: Double -> View
init_view sz = V 0 0 0 1 True
                 0.01
                 identity
                 identity
                 maxBound
                 (makeColor 0.8 0.8 0.9 1)
                 0.05
                 0.01
                 (makeColor 1.0 0.7 0.2 1)
                 sz
                 (Modifiers Up Up Up, Nothing, (0, 0))


-- | Rotation and scale matrix, separate from projection and translation. We use
--   this to back-transform drag operations into spatial vectors.
rs_matrix :: View -> M33 Double
rs_matrix v = V3 (V3 1    0   0)
                 (V3 0   cx  sx)
                 (V3 0 (-sx) cx)
          !*! V3 (V3   cy  0 sy)
                 (V3    0  1  0)
                 (V3 (-sy) 0 cy)
          !*! identity !!* _vz v
  where (cy, sy) = cs (_vry v)
        (cx, sx) = cs (_vrx v)


-- | The full view matrix used for rendering.
view_matrix :: View -> M44 Double
view_matrix v = (identity & _w .~ (if _vp v then V4 0 0 1 0 else V4 0 0 0 1))
            !*! (identity & _z._w .~ 1)
            !*! (identity & _m33 .~ rs_matrix v)
            !*! transpose (identity & _w._xyz .~ _vt v)


f2d = float2Double
d2f = double2Float


translate_rel :: V3 Double -> View -> View
translate_rel d v = v & vt %~ (^+^ inv33 (rs_matrix v) !* (d & _xy %~ (^/ _vsz v)))
