{-# LANGUAGE BlockArguments #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Functions to render things within a viewport and to support things like
--   variable level of detail.
module Wumber.ViewRender where


import Linear.Matrix ((!*))

import Wumber.Numeric


-- TODO
