{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

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
  vars_in,
) where


import Data.Binary  (Binary(..))
import Data.List    (intercalate, sort)
import Data.Set     (Set(..), empty, singleton, union, unions)
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
  deriving (Ord, Eq, Generic, Binary)

data SymTerm a = a :* [SymExp a]
  deriving (Ord, Eq, Generic, Binary)

data SymExp a = SymVar a :** a
  deriving (Ord, Eq, Generic, Binary)

data SymVar a = Var !VarID
              | Fn1 !SymFn1 (Set VarID) (Sym a)
              | Fn2 !SymFn2 (Set VarID) (Sym a) (Sym a)
  deriving (Ord, Eq, Generic, Binary)

type VarID = Int


instance (Num a, Eq a, Show a) => Show (Sym a) where
  show (xs :+ 0) = intercalate " + " (map show xs)
  show ([] :+ n) = show n
  show (xs :+ n) = intercalate " + " (map show xs ++ [show n])

instance (Num a, Eq a, Show a) => Show (SymTerm a) where
  show (1 :* es) = intercalate "·" (map show es)
  show (n :* []) = "NONNORM_TERM " ++ show n
  show (n :* es) = intercalate "·" (show n : map show es)

instance (Num a, Eq a, Show a) => Show (SymExp a) where
  show (x :** 0) = "NONNORM_EXP 1"
  show (x :** 1) = show x
  show (x :** 2) = show x ++ "²"
  show (x :** 3) = show x ++ "³"
  show (x :** 4) = show x ++ "⁴"
  show (x :** n) = "(" ++ show x ++ ")^" ++ show n

instance (Num a, Eq a, Show a) => Show (SymVar a) where
  show (Var 0)       = "x"
  show (Var 1)       = "y"
  show (Var 2)       = "z"
  show (Var i)       = "v" ++ show i
  show (Fn1 f _ x)   = show f ++ "(" ++ show x ++ ")"
  show (Fn2 f _ x y) = show f ++ "(" ++ show x ++ ", " ++ show y ++ ")"


infixl 6 :+
infixl 7 :*
infixl 8 :**


-- | Evaluate a symbolic quantity using Haskell math. To do this, we need a
--   function that handles 'Arg' values.
--
--   TODO: remove RealFloat constraint, or have Sym implement RealFloat. I want
--   to be able to use 'eval' to rewrite things.

eval :: (Floating a, RealFloat a, Mod a, ClosedComparable a)
     => (VarID -> a) -> Sym a -> a

eval f (ts :+ b) = b + sum (map eval_t ts)
  where eval_t (a :* xs)      = a * product (map eval_e xs)
        eval_e (x :** n)      = eval_v x ** n
        eval_v (Var i)        = f i
        eval_v (Fn1 op _ x)   = fn op (eval f x)
        eval_v (Fn2 op _ x y) = fn op (eval f x) (eval f y)


p0   = ([] :+)
t0   = (:* [])
vp x = [1 :* [x :** 1]] :+ 0
var  = vp . Var

fn1 f x   = Fn1 f (vars_in x) x
fn2 f x y = Fn2 f (vars_in x `union` vars_in y) x y

pt (xs :+ _) = xs
te (_ :* es) = es
ev (v :** _) = v


merge_with :: Ord o => (a -> o) -> (a -> b) -> (a -> a -> b) -> [a] -> [a] -> [b]
merge_with _ i _ xs [] = map i xs
merge_with _ i _ [] ys = map i ys
merge_with v i f (x:xs) (y:ys) | v x < v y = i x   : merge_with v i f xs (y:ys)
                               | v y < v x = i y   : merge_with v i f (x:xs) ys
                               | otherwise = f x y : merge_with v i f xs ys


padd :: (Eq a, Num a, Ord a) => Sym a -> Sym a -> Sym a
padd (xs :+ a) (ys :+ b) = concat (merge_with te (: []) tadd xs ys) :+ (a + b)
  where tadd (a :* x) (b :* _) | a + b /= 0 = [(a + b) :* x]
                               | otherwise  = []

pmul :: (Num a, Ord a) => Sym a -> Sym a -> Sym a
pmul (xs :+ a) (ys :+ b) = sum [tmul x y | x <- t0 a : xs, y <- t0 b : ys]

tmul :: (Num a, Ord a) => SymTerm a -> SymTerm a -> Sym a
tmul (a :* xs) (b :* ys) | null es    = []              :+ a * b
                         | a * b /= 0 = [(a * b) :* es] :+ 0
                         | otherwise  = 0
  where es = concat (merge_with ev (: []) emul xs ys)

emul :: (Num a, Ord a) => SymExp a -> SymExp a -> [SymExp a]
emul (x :** m) (y :** n) | x /= y     = sort [x :** m, y :** n]
                         | m + n /= 0 = [x :** (m + n)]
                         | otherwise  = []


instance (Ord a, Num a) => Num (Sym a) where
  fromInteger = p0 . fromInteger
  abs         = vp . fn1 Abs
  signum      = vp . fn1 Signum
  negate      = ((-1) *)
  (+)         = padd
  (*)         = pmul


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
            | Pow
            | Upper
            | Lower
            | Atan2
  deriving (Show, Ord, Eq, Generic, Binary, Enum)


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
  fn Pow      = (**)
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
vars_in (xs :+ b) = unions (map vars_in_t xs)
  where vars_in_t (a :* xs)     = unions (map vars_in_e xs)
        vars_in_e (x :** n)     = vars_in_v x
        vars_in_v (Var i)       = singleton i
        vars_in_v (Fn1 _ v _)   = v
        vars_in_v (Fn2 _ v _ _) = v


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
  minBound = p0 minBound
  maxBound = p0 maxBound

instance (Ord a, Floating a, Fractional a) => Fractional (Sym a) where
  fromRational = p0 . fromRational
  recip        = (** (-1))

instance (Ord a, Mod a, Num a) => Mod (Sym a) where
  a % b | a == b    = 0
        | otherwise = vp $ fn2 Mod a b

instance (Ord a, Floating a) => Floating (Sym a) where
  pi = p0 pi

  ([]        :+ x) ** ([] :+ n) = [] :+ x ** n
  ([a :* es] :+ 0) ** ([] :+ n) = [a**n :* map (\(v:**e) -> v:**(e*n)) es] :+ 0
  a                ** b         = vp $ fn2 Pow a b

  exp   = vp . fn1 Exp
  log   = vp . fn1 Log
  sin   = vp . fn1 Sin
  cos   = vp . fn1 Cos
  tan   = vp . fn1 Tan
  asin  = vp . fn1 Asin
  acos  = vp . fn1 Acos
  atan  = vp . fn1 Atan
  sinh  = vp . fn1 Sinh
  cosh  = vp . fn1 Cosh
  tanh  = vp . fn1 Tanh
  asinh = vp . fn1 Asinh
  acosh = vp . fn1 Acosh
  atanh = vp . fn1 Atanh

instance (Num a, ClosedComparable a) => ClosedComparable (Sym a) where
  lower a b = vp $ fn2 Lower a b
  upper a b = vp $ fn2 Upper a b
