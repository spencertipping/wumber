{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}

module Wumber.ClosedComparable where

import Control.Applicative
import Linear.V2
import Linear.V3


-- | Values that have upper and lower bounds despite possibly not being 'Ord'.
--   We need this class because 'min' and 'max' require operands to be
--   immediately comparable.
class ClosedComparable a where
  lower :: a -> a -> a
  upper :: a -> a -> a


nan_max x y | isNaN x = x
            | isNaN y = y
            | otherwise = max x y

nan_min x y | isNaN x = x
            | isNaN y = y
            | otherwise = min x y


instance ClosedComparable Double where
  lower = nan_min
  upper = nan_max

instance ClosedComparable Float where
  lower = nan_min
  upper = nan_max

instance {-# OVERLAPPABLE #-} Ord a => ClosedComparable a where
  lower = min
  upper = max

instance {-# OVERLAPPABLE #-} (Applicative f, ClosedComparable a)
      => ClosedComparable (f a) where
  {-# SPECIALIZE instance ClosedComparable (V3 Double) #-}
  {-# SPECIALIZE instance ClosedComparable (V2 Double) #-}
  lower = liftA2 lower
  upper = liftA2 upper
