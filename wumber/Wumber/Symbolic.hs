{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ConstraintKinds #-}

-- NOTE: this is required to avoid infinite loops when defining recip and negate
{-# LANGUAGE NegativeLiterals #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}


-- | Symbolic representation of closed-form numeric expressions. 'Sym a' is an
--   expression grammar whose terminals have type 'a'. 'Sym' is used both to
--   simplify linear equations for constraints, and to JIT-compile cost and
--   isosurface functions.
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
  Functionable(..),
  eval,
  vars_in,
) where


import Data.Binary  (Binary(..))
import Data.List    (intercalate, sort, sortBy)
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
--   NOTE: 'Sym's aren't 'Ord' in the sense you expect, so I've taken some
--   effort to make sure they don't claim to be (this way you can't use 'max'
--   and 'min' instead of 'upper' and 'lower'). The short version of this story
--   is that we use 'Ord' internally to do efficient term factoring, but this
--   definition has nothing to do with numerical comparisons.
--
--   TODO: add callbacks to Haskell and/or C functions
--   TODO: add cond/piecewise

data Sym a     = [SymTerm a] :+ a deriving (Eq, Generic, Binary)
data SymTerm a = a :* [SymExp a]  deriving (Ord, Eq, Generic, Binary)
data SymExp a  = SymVar a :** a   deriving (Ord, Eq, Generic, Binary)
data SymVar a  = Var !VarID
               | Fn1 !SymFn1 (Set VarID) (OrdSym a)
               | Fn2 !SymFn2 (Set VarID) (OrdSym a) (OrdSym a)
  deriving (Ord, Eq, Generic, Binary)

-- | The workaround we use to add an ordering to 'Sym' without exporting it
--   outside this module.
newtype OrdSym a = OS (Sym a) deriving (Eq, Generic, Binary)

type VarID = Int

infixl 6 :+
infixl 7 :*
infixl 8 :**

-- | All of the typeclasses 'a' will end up belonging to for 'Sym'. I'm aliasing
--   it because otherwise we'd end up writing it out a bunch of times.
type SymConstraints a = (Show a, StaticOrd a, Num a, RealFloat a, Floating a,
                         Integral a, ClosedComparable a)

instance Ord a => Ord (OrdSym a) where
  compare (OS ([] :+ a)) (OS ([] :+ b)) = compare a b
  compare (OS (xs :+ _)) (OS (ys :+ _)) = compare xs ys


-- | An 'Ord' replacement that works with our hidden 'Ord' thing. 'Sym'
--   implements 'Ord' separately in order to provide 'Real', but its ordering is
--   different and will fail at runtime if you try to compare undetermined
--   quantities.
class Eq a => StaticOrd a where
  compare' :: a -> a -> Ordering
  (<<)     :: a -> a -> Bool
  a << b = compare' a b == LT

instance {-# OVERLAPPING #-} (Eq a, Ord a) => StaticOrd (Sym a) where
  compare' a b = compare' (OS a) (OS b)

instance {-# OVERLAPPING #-} (Eq a, Ord a) => StaticOrd a where
  compare' = compare


instance (Num a, Eq a, Show a) => Show (OrdSym a) where
  show (OS x) = show x

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


-- | Evaluate a symbolic quantity using Haskell math. To do this, we need a
--   function that handles 'Var' values.
eval :: SymConstraints a => (VarID -> a) -> Sym a -> a
eval f (ts :+ b) = b + sum (map eval_t ts)
  where eval_t (a :* xs)                = a * product (map eval_e xs)
        eval_e (x :** n)                = eval_v x ** n
        eval_v (Var i)                  = f i
        eval_v (Fn1 op _ (OS x))        = fn op (eval f x)
        eval_v (Fn2 op _ (OS x) (OS y)) = fn op (eval f x) (eval f y)


class    ToSym a       where sym :: SymConstraints x => a x -> Sym x
instance ToSym Sym     where sym   = id
instance ToSym SymTerm where sym a = [a] :+ 0
instance ToSym SymExp  where sym a = sym $ 1 :* [a]
instance ToSym SymVar  where sym a = sym $ a :** 1


val :: a -> Sym a
val = ([] :+)

var :: SymConstraints a => VarID -> Sym a
var = sym . Var

fn1 :: SymConstraints a => SymFn1 -> Sym a -> Sym a
fn1 f ([] :+ x) = [] :+ fn f x
fn1 f x         = sym $ Fn1 f (vars_in x) (OS x)

fn2 :: SymConstraints a => SymFn2 -> Sym a -> Sym a -> Sym a
fn2 f ([] :+ x) ([] :+ y) = [] :+ fn f x y
fn2 f x y                 = sym $ Fn2 f (vars_in x `union` vars_in y) (OS x) (OS y)


-- | Merges sorted lists of things, whether those are terms or exponent things
--   or variables. You can use a view function to view each element, e.g. to
--   merge on term variables ignoring coefficients. This function gives us an
--   efficient way to keep polynomials factored.
merge_with :: StaticOrd o
           => (a -> o) -> (a -> b) -> (a -> a -> b) -> [a] -> [a] -> [b]
merge_with _ i _ xs [] = map i xs
merge_with _ i _ [] ys = map i ys
merge_with v i f (x:xs) (y:ys) | v x << v y = i x   : merge_with v i f xs (y:ys)
                               | v y << v x = i y   : merge_with v i f (x:xs) ys
                               | otherwise  = f x y : merge_with v i f xs ys


-- | Polynomial addition with term grouping.
padd :: SymConstraints a => Sym a -> Sym a -> Sym a
padd (xs :+ a) (ys :+ b) = concat (merge_with te (: []) tadd xs ys) :+ (a + b)
  where te (_ :* es) = es
        tadd (a :* x) (b :* _) | a + b /= 0 = [(a + b) :* x]
                               | otherwise  = []

-- | Polynomial multiplication with term grouping.
pmul :: SymConstraints a => Sym a -> Sym a -> Sym a
pmul (xs :+ a) (ys :+ b) = sum [tmul x y | x <- a :* [] : xs,
                                           y <- b :* [] : ys]
  where tmul (a :* xs) (b :* ys) | null es    = []              :+ a * b
                                 | a * b /= 0 = [(a * b) :* es] :+ 0
                                 | otherwise  = 0
          where ev (v :** _) = v
                es = concat (merge_with ev (: []) emul xs ys)

        emul (x :** m) (y :** n) | x /= y     = sortBy compare' [x :** m, y :** n]
                                 | m + n /= 0 = [x :** (m + n)]
                                 | otherwise  = []

-- | Polynomial exponentiation with term grouping. Uses repeated squaring if the
--   exponent is a positive integer constant.
ppow :: SymConstraints a => Sym a -> Sym a -> Sym a
ppow _ 0 = 1
ppow ([] :+ x) ([] :+ n) = val (x ** n)

ppow a ([] :+ n) | n == truncate n && n > 0 =
                   half * half * (if n `mod` 2 == 0 then 1 else a)
  where half = ppow a (val (n `quot` 2))

ppow ([a :* es] :+ 0) ([] :+ n) = [a**n :* map (\(v:**e) -> v:**(e*n)) es] :+ 0
ppow a b = fn2 Pow a b


instance SymConstraints a => Num (Sym a) where
  fromInteger = val . fromInteger
  abs         = fn1 Abs
  signum      = fn1 Signum
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

data SymFn2 = Quot
            | Rem
            | Pow
            | Upper
            | Lower
            | Atan2
  deriving (Show, Ord, Eq, Generic, Binary, Enum)


class Functionable x t | t -> x where
  fn :: x -> t

instance SymConstraints a => Functionable SymFn1 (a -> a) where
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

instance SymConstraints a => Functionable SymFn2 (a -> a -> a) where
  fn Pow   = (**)
  fn Quot  = quot
  fn Rem   = rem
  fn Upper = nan_upper
  fn Lower = nan_lower
  fn Atan2 = atan2


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


instance Bounded a => Bounded (Sym a) where
  minBound = val minBound
  maxBound = val maxBound

instance SymConstraints a => Fractional (Sym a) where
  fromRational = val . fromRational
  recip x      = ppow x (-1)

instance SymConstraints a => Floating (Sym a) where
  (**)  = ppow
  pi    = val pi
  exp   = fn1 Exp
  log   = fn1 Log
  sin   = fn1 Sin
  cos   = fn1 Cos
  tan   = fn1 Tan
  asin  = fn1 Asin
  acos  = fn1 Acos
  atan  = fn1 Atan
  sinh  = fn1 Sinh
  cosh  = fn1 Cosh
  tanh  = fn1 Tanh
  asinh = fn1 Asinh
  acosh = fn1 Acosh
  atanh = fn1 Atanh

instance SymConstraints a => ClosedComparable (Sym a) where
  lower a b = fn2 Lower a b
  upper a b = fn2 Upper a b


-- HERE THERE BE GENERAL WEIRDNESS
-- We need to lie a little about the nature of 'Double' and 'Float'. Basically,
-- these types need to be 'Integral' so they satisfy our 'SymConstraints'.

qr a b = (q, r) where q = truncate (a / b)
                      r = a - q*b

instance Integral Double where toInteger = truncate; quotRem = qr
instance Integral Float  where toInteger = truncate; quotRem = qr


-- HERE THERE BE PARTIAL INSTANCES
-- The goal here is to be able to use regular Haskell math functions, even
-- though many of them require us to pretend to support operations we have no
-- hope of providing. In my opinion, it's more useful to support these
-- operations than it is to be type safe.

instance SymConstraints a => Ord (Sym a) where
  compare ([] :+ a) ([] :+ b) = compare a b
  compare a b = error $ "can't compare undetermined Syms "
                        ++ show a ++ " and " ++ show b ++ " (did you want "
                        ++ "to use upper/lower instead of max/min?)"

instance SymConstraints a => Enum (Sym a) where
  toEnum     = fromInteger . toInteger
  fromEnum _ = error "can't collapse Sym to Integer via fromEnum"
  pred x     = x - 1
  succ x     = x + 1

instance SymConstraints a => Real (Sym a) where
  toRational _ = error "can't collapse Sym to Rational via toRational"

instance SymConstraints a => Integral (Sym a) where
  toInteger _ = error "can't collapse Sym to Integer via toInteger"
  quotRem a b = (fn2 Quot a b, fn2 Rem a b)

instance SymConstraints a => RealFrac (Sym a) where
  properFraction a = error "properFraction is currenty undefined on Syms"

instance SymConstraints a => RealFloat (Sym a) where
  -- Yes, I am implementing this whole typeclass just to get atan2.
  floatRadix _     = error "floatRadix is undefined on Syms"
  floatDigits _    = error "floatDigits is undefined on Syms"
  floatRange _     = error "floatRange is undefined on Syms"
  decodeFloat _    = error "decodeFloat is undefined on Syms"
  encodeFloat _ _  = error "decodeFloat is undefined on Syms"
  isNaN _          = error "isNaN is undefined on Syms"
  isInfinite _     = error "isInfinite is undefined on Syms"
  isDenormalized _ = error "isDenormalized is undefined on Syms"
  isNegativeZero _ = error "isNegativeZero is undefined on Syms"
  isIEEE _         = error "isIEEE is undefined on Syms"
  atan2 a b        = fn2 Atan2 a b
