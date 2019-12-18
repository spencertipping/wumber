{-# LANGUAGE BlockArguments #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Functions to render things within a viewport and to support things like
--   variable level of detail.
--
--   There are two reasons we need to support vLOD: one is rendering
--   performance, and the other is to optimize the visual presentation for the
--   user. The general principle is that we should aim to provide /x/ amount of
--   detail per pixel or screen area, where /x/ is user-configurable.
--
--   Beyond LOD, we also support view bounds and clipping. This is important for
--   perspective projection because negative /Z/ values will create lines that
--   appear to cross the origin, and zero /Z/ will produce infinite 2D
--   coordinates.
--
--   We can also save a lot of work by limiting the rendering scope in cases
--   where the user is zoomed into a small area: rather than pulling every
--   element and using a naive list filter, we can change our sketch request to
--   operate within a specific bounding frustum -- and within that, we can split
--   effective LOD by depth when the viewport has a perspective projection
--   matrix.

module Wumber.ViewRender where


import Linear.Matrix ((!*))

import Wumber.Numeric


-- TODO
