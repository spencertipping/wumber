{-# LANGUAGE BlockArguments #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Concrete volumetric representation for objects. This is used to construct
--   meshes and constraints for FEA. Unlike 'Wumber.ModelBRep', this module
--   emphasizes element-element /connectedness/. It should be possible to
--   construct a single stress/strain equation system from an object's VRep.
module Wumber.ModelVRep where


-- TODO
-- First, how do we represent elements?
--
-- Second, how do we specify the desired element resolution?
