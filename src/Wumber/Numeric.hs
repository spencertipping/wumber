{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | General-purpose numeric utilities.
module Wumber.Numeric where

import GHC.Float


-- Remedial Haskell functions
f2d = float2Double
d2f = double2Float
fi  = fromIntegral


-- | Real numbers within Wumber.
type R = Double


-- | The real circle constant. We all know π was a mistake.
τ :: Floating a => a
τ = pi * 2


-- | Calculates an appropriate numerical delta for the given value by
--   considering floating point precision limits. The goal is to put the delta
--   halfway into the mantissa, which for doubles is about 26 bits.
--
--   Deltas are always positive.
--
--   NOTE: these types are intentionally fixed to 'Double' instead of using 'R'.
--   That way the code will break if you change 'R', which is correct -- the
--   delta would need to be updated.

δ :: Double -> Double
δ x = max 1 (abs x) * 2**(-26)
