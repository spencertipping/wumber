{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}


-- | Symbolic representation of closed-form numeric expressions. 'Sym a' is an
--   expression grammar whose terminals have type 'a'. 'Sym' is used both to
--   simplify linear equations for constraints, and to JIT-compile cost and
--   isosurface functions.
--
--   'Sym' constant-folds at construction time. If you don't want this behavior,
--   have 'is_const' return 'False' for all values. (See 'Constable', which 'a'
--   must implement.)
--
--   TODO: A-normal form?

module Wumber.Symbolic (
  Sym(..),
  SymTerm(..),
  SymExp(..),
  SymVar(..),
  VarID,
  SymFn1(..),
  SymFn2(..),
  Mod(..),
  Functionable(..),
  eval,
  args_in,
) where


import Data.Binary  (Binary(..))
import Data.Set     (Set(..), empty, singleton, union)
import Data.Vector  (Vector, (!))
import Foreign.Ptr  (FunPtr(..))
import GHC.Generics (Generic(..))
import Text.Printf  (printf)

import qualified Data.Set as S

import Wumber.ClosedComparable


-- | A symbolic expression in a normal form that's reasonably easy to work with.
--   'Sym' expressions simplify themselves as you build them up. This includes
--   constant folding, condensing common subexpressions (e.g. 'x + x' becomes
--   '2*x'), and keeping track of variable dependencies and invertibility.
--
--   TODO: add callbacks to Haskell and/or C functions
--   TODO: add cond/piecewise

data Sym a = [SymTerm a] :+ a
  deriving (Show, Ord, Eq, Generic, Binary)

data SymTerm a = a :* [SymExp a]
  deriving (Show, Eq, Generic, Binary)

data SymExp a = SymVar a :** a
  deriving (Show, Ord, Eq, Generic, Binary)

data SymVar a = Var !VarID
              | Fn1 !SymFn1 (Set VarID) (Sym a)
              | Fn2 !SymFn2 (Set VarID) (Sym a) (Sym a)
  deriving (Show, Ord, Eq, Generic, Binary)

type VarID = Int


infixl 6 :+
infixl 7 :*
infixl 8 :**


instance Ord a => Ord (SymTerm a) where
  (_ :* a) `compare` (_ :* b) = a `compare` b


p0 :: a -> Sym a
p0 = (:+ [])

pnull :: (Eq a, Num a) => Sym a -> Bool
pnull ([] :+ 0) = True
pnull _         = False


merge_with :: Ord a => (a -> b) -> (a -> a -> b) -> [a] -> [a] -> [b]
merge_with i f xs [] = map i xs
merge_with i f [] ys = map i ys
merge_with i f (x:xs) (y:ys) | x < y     = i x   : merge_with i f xs (y:ys)
                             | y < x     = i y   : merge_with i f (x:xs) ys
                             | otherwise = f x y : merge_with i f xs ys


padd :: (Eq a, Num a, Ord a) => Sym a -> Sym a -> Sym a
padd (xs :+ a) (ys :+ b) = concat (merge_with (: []) tadd xs ys) :+ (a + b)
  where tadd (a :* x) (b :* _) | a + b /= 0 = [(a + b) :* x]
                               | otherwise  = []

pmul :: (Eq a, Num a, Ord a) => Sym a -> Sym a -> Sym a
pmul (xs :+ a) (ys :+ b) = sum (p0 (a * b) : [tmul x y :+ 0 | x <- xs, y <- ys])


  
-- tmul :: 


instance (Ord a, Num a) => Num (Sym a) where
  fromInteger = p0 . fromInteger
  (+)   = padd
  a - b = a + (-1) * b
  (*)   = pmul


-- | Unary transcendental functions that would otherwise clutter up 'Sym'.
data SymFn1 = Abs
            | Signum
            | Log
            | Exp
            | Sin
            | Cos
            | Tan
            | Asin
            | Acos
            | Atan
            | Sinh
            | Cosh
            | Tanh
            | Asinh
            | Acosh
            | Atanh
  deriving (Show, Ord, Eq, Generic, Binary, Enum)

data SymFn2 = Mod
            | Upper
            | Lower
            | Atan2
  deriving (Show, Ord, Eq, Generic, Binary, Enum)


-- | Evaluate a symbolic quantity using Haskell math. To do this, we need a
--   function that handles 'Arg' values.
--
--   TODO: remove RealFloat constraint, or have Sym implement RealFloat. I want
--   to be able to use 'eval' to rewrite things.

eval :: (Floating a, RealFloat a, Mod a, ClosedComparable a)
     => (VarID -> a) -> Sym a -> a

eval f (ts :+ b) = b + sum (map eval_t ts)
  where eval_t (Term a x n)    = a * eval_symt x ** n

        eval_symt (Var i)      = f i
        eval_symt (Fn1 op x)   = fn op (eval f x)
        eval_symt (Fn2 op x y) = fn op (eval f x) (eval f y)


class Functionable x t | t -> x where
  fn :: x -> t

instance (Floating a, Mod a) => Functionable SymFn1 (a -> a) where
  fn Abs    = abs
  fn Signum = signum
  fn Log    = log
  fn Exp    = exp
  fn Sin    = sin
  fn Cos    = cos
  fn Tan    = tan
  fn Asin   = asin
  fn Acos   = acos
  fn Atan   = atan
  fn Sinh   = sinh
  fn Cosh   = cosh
  fn Tanh   = tanh
  fn Asinh  = asinh
  fn Acosh  = acosh
  fn Atanh  = atanh

instance (Floating a, Mod a, ClosedComparable a, RealFloat a) =>
         Functionable SymFn2 (a -> a -> a) where
  fn Mod      = (%)
  fn Upper    = nan_upper
  fn Lower    = nan_lower
  fn Atan2    = atan2


nan_upper x y | isNaN x = x
              | isNaN y = y
              | otherwise = upper x y

nan_lower x y | isNaN x = x
              | isNaN y = y
              | otherwise = lower x y


-- | Returns a set of all 'Var' indexes used by an expression.
vars_in :: Sym a -> Set VarID
vars_in (Poly _ xs _ _) = unions (map vars_in_t xs)
  where vars_in_t (Term _ (Var x)      _) = singleton x
        vars_in_t (Term _ (Fn1 vs _)   _) = vs
        vars_in_t (Term _ (Fn2 vs _ _) _) = vs


-- | Values that support floating-point 'mod', but without using Haskell's
--   numeric type hierarchy to do so.
--
--   The reason we can't use Haskell types for this is that it quantifies a
--   number of operators with 'forall b. Integral b => a -> b' -- requiring us
--   to provide function implementations that would coerce our abstract symbolic
--   quantities to concrete ones if the return type dictates it.
--
--   TODO
--   Use SHA and clown-car Integer embedding to work around this

class Mod a where (%) :: a -> a -> a
infixl 7 %

instance Mod Double where (%) = c_fmod
instance Mod Float  where (%) = c_fmodf

foreign import ccall unsafe "math.h fmod"  c_fmod  :: Double -> Double -> Double
foreign import ccall unsafe "math.h fmodf" c_fmodf :: Float  -> Float  -> Float


instance Bounded a => Bounded (Sym a) where
  minBound = N minBound
  maxBound = N maxBound

instance Fractional a => Fractional (Sym a) where
  fromRational = N . fromRational
  (/) = cf_binary (/) (Fn2 Divide)

instance Mod a => Mod (Sym a) where
  (%) = cf_binary (%) (Fn2 Mod)

instance Floating a => Floating (Sym a) where
  pi = N pi

  (**) = cf_binary (**) (Fn2 Pow)

  exp   = cf_unary exp   (Fn1 Exp)
  log   = cf_unary log   (Fn1 Log)
  sqrt  = cf_unary sqrt  (Fn1 Sqrt)
  sin   = cf_unary sin   (Fn1 Sin)
  cos   = cf_unary cos   (Fn1 Cos)
  tan   = cf_unary tan   (Fn1 Tan)
  asin  = cf_unary asin  (Fn1 Asin)
  acos  = cf_unary acos  (Fn1 Acos)
  atan  = cf_unary atan  (Fn1 Atan)
  sinh  = cf_unary sinh  (Fn1 Sinh)
  cosh  = cf_unary cosh  (Fn1 Cosh)
  tanh  = cf_unary tanh  (Fn1 Tanh)
  asinh = cf_unary asinh (Fn1 Asinh)
  acosh = cf_unary acosh (Fn1 Acosh)
  atanh = cf_unary atanh (Fn1 Atanh)

instance ClosedComparable a => ClosedComparable (Sym a) where
  lower = cf_binary lower (Fn2 Lower)
  upper = cf_binary upper (Fn2 Upper)
