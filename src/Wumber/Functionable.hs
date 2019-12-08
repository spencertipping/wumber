{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wincomplete-patterns #-}

-- | Support for things that can be converted to functions in different forms.
module Wumber.Functionable where


import Data.Binary  (Binary)
import Data.List    (intercalate)
import GHC.Generics (Generic)


-- | The class of objects @x@ that can be converted to functional type @t@. This
--   is intentionally general; it's used both by constant-folding logic and by
--   JIT machinery to select C function pointers.
class Functionable x t where fn :: x -> t

instance Functionable a a where fn = id

instance Functionable () a where
  fn _ = error "Functionable is disabled for the () instance"


-- | 'Show' functionality for functions. Most functions can either be shown with
--   their arguments infix (e.g. @+@), or prefix (e.g. @sin@).
class Show f => FnShow f where
  fshow_style :: f -> Maybe FnShowStyle
  fshow_fn    :: f -> String
  fshow       :: f -> [String] -> String

  fshow_fn   = show
  fshow f xs = case fshow_style f of
    Just ShowPrefix
      | [x] <- xs -> fshow_fn f ++ "(" ++ x ++ ")"
      | otherwise -> fshow_fn f ++ "(" ++ intercalate ", " xs ++ ")"
    Just ShowPostfix
      | [x] <- xs -> "(" ++ x ++ ")" ++ fshow_fn f
      | otherwise -> "(" ++ intercalate ", " xs ++ ")" ++ fshow_fn f
    Just ShowInfix -> "(" ++ intercalate (" " ++ fshow_fn f ++ " ") xs ++ ")"
    _              -> error "must implement fshow or use builtin style"

data FnShowStyle = ShowPrefix | ShowInfix | ShowPostfix
  deriving (Show, Eq, Ord, Bounded, Enum, Generic, Binary)
