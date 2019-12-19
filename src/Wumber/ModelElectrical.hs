{-# LANGUAGE BlockArguments #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Types to model objects that have electrical connections and can participate
--   in circuits. These objects are assumed to obey Kirchhoff's circuit laws --
--   i.e. they can neither store nor supply absolute electrical charge. (You can
--   get around this restriction and simulate non-ideal components by using
--   parasitic elements.)
--
--   From Wumber's perspective, components are simulated by time-stepping a
--   constraint system that couples voltage and current.

module Wumber.ModelElectrical where


import Wumber.Constraint


-- TODO
