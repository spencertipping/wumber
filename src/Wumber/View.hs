{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Rendering-oriented things for viewable objects; for example, viewable
--   objects may render parts of themselves at different resolutions depending
--   on where the user's viewport is focused.
module Wumber.View where


import Data.Binary   (Binary)
import GHC.Generics  (Generic)
import Lens.Micro.TH (makeLenses)
import Linear.Matrix (M33, M44)
import Linear.V2     (V2)
import Linear.V3     (V3)
import Linear.V4     (V4)

import Wumber.Affine
import Wumber.Fingerprint
import Wumber.Numeric


-- | View settings, from a rendering perspective.
data ViewSettings m = VS { _vs_viewport       :: Viewport m,
                           _vs_deadline_nanos :: Int }
  deriving (Generic)

deriving instance Show   (m R) => Show   (ViewSettings m)
deriving instance Eq     (m R) => Eq     (ViewSettings m)
deriving instance Binary (m R) => Binary (ViewSettings m)

instance Binary (m R) => Fingerprintable (ViewSettings m) where
  fingerprint = binary_fingerprint


-- | The pixel dimensions and camera matrix for a viewport.
data Viewport m = VP { _vp_pixel_size :: !(V2 Int),
                       _vp_cm_size    :: !(V2 R),
                       _vp_cursor_pos :: !(V2 Int),
                       _vp_bg_rgb     :: !(V3 R),
                       _vp_matrix     :: !(m R) }
  deriving (Generic)

deriving instance Show   (m R) => Show   (Viewport m)
deriving instance Eq     (m R) => Eq     (Viewport m)
deriving instance Binary (m R) => Binary (Viewport m)

instance Binary (m R) => Fingerprintable (Viewport m) where
  fingerprint = binary_fingerprint


makeLenses ''ViewSettings
makeLenses ''Viewport
