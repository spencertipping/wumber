{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Wumber.Numeric (
  R, δ
) where


-- | Real numbers within Wumber.
type R = Double


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
