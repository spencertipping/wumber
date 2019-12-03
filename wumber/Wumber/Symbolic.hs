{-# LANGUAGE BangPatterns #-}
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
{-# LANGUAGE GADTs #-}

-- NOTE: this is required to avoid infinite loops when defining recip and negate
{-# LANGUAGE NegativeLiterals #-}

{-# OPTIONS_GHC -funbox-strict-fields -Wincomplete-patterns #-}


-- TODO
-- Split this module into multiple parts:
--
-- 1. Data type definitions + 'Show'
-- 2. Polynomial math + term normalization
-- 3. 'Num' and 'RealFloat' shenanigans
-- 4. Symbolic derivatives
-- 5. Symbolic variable isolation


-- TODO
-- Replace IntSet with a bitset (can't use the one on hackage, but maybe port
-- it); then drop a bitset onto every level of this hierarchy.

-- TODO
-- Cache a 'profile' per element, something that keeps it indexed/located within
-- chains. Profiles should be unboxed hash-like things we can use to pattern
-- match things without visiting children.

-- TODO
-- Lazily calculate the 'Binary' representation for each element on each element
-- for fingerprinting/comparison purposes.

-- TODO
-- No more infix constructors here; the notation isn't useful and it prevents us
-- from having additional fields.

-- TODO
-- Replace lists with a sorted-merge-friendly linear structure, possibly wrapped
-- around Vector.

-- TODO
-- Use a shell 'newtype' for user-facing operations and 'Ord', then have that be
-- unwrappable to an internal thing that supports structural 'Ord', 'Eq', etc.
-- In other words, invert the current 'OrdSym' idea.


-- TODO
-- Implement polynomial long division as a way to get a factored representation.

-- TODO
-- Is term expansion ever useful from an algebraic perspective? Other options
-- would be to leave things factored by multiplying or applying ':**' to a
-- 'Poly' 'SymVar'. Then we could expand down the line if we wanted to, but we'd
-- still have the factored representation.
--
-- Do we want to prefer the factored representation until we see nontrivial
-- terms get merged? Or maybe we always treat the factored representation (if we
-- have one) as canonical, then try expanding terms when we want to match
-- things.


-- TODO
-- Replace 'Upper' and 'Lower' with 'IfPositive(x, a, b)' ternary. This will
-- make it possible for derivatives to use a closed set of operations; right now
-- 'Upper' and 'Lower' have no symbolic derivatives within Sym.


-- | Symbolic numerical quantities. @Sym f a@ is a symbolic quantity with
--   'Functionable' type @f@ (or @()@ if you don't need custom functions) and
--   numeric type @a@.
--
--   'Sym's behave like numbers in that they implement Haskell's full numeric
--   stack, sometimes incompletely. Internally they constant-fold when possible
--   and otherwise maintain polynomial normal form around variables.
--
--   Two functions, 'var' and 'val', are useful if you want to construct @Sym@s.
--   For example:
--
--   > circle_fn :: Double -> Sym () Double
--   > circle_fn r = val r ** 2 - (var 0 ** 2 + var 1 ** 2)
--
--   The meaning of @var@s depends on context. As far as @Sym@ is concerned,
--   they're just indexed arguments to the function that will be JIT-compiled,
--   and it doesn't really matter whether the function will be used as an
--   isosurface or for constraint minimization. Within an isosurface/implicit
--   context, vars @0..3@ are @x@, @y@, @z@, and @t@ (reflected by their @Show@
--   output). The above @circle_fn@ definition renders like this:
--
--   > -1.0·x² + -1.0·y² + 9.0
--
--   See 'Wumber.SymbolicAlgebra' for methods to isolate variables and solve
--   systems of equations.
--
--   NOTE: avoid using 'RealFrac' and 'RealFloat' methods, except for 'atan2'.
--   Many of the implementations either use 'unsafeCoerce' to work around
--   @forall@ type quantifiers, or will produce errors at runtime. Intuitively:
--   any function whose signature is @Sym -> not-Sym@ is likely to fail because
--   @Sym@ quantities aren't determinable until we have values for their
--   variables.

module Wumber.Symbolic (
  Sym(..),
  SymConstraints(..),
  FConstraints(..),
  NumConstraints(..),
  AlgConstraints(..),
  SymTerm(..),
  SymExp(..),
  SymVar(..),
  OrdSym(..),
  VarID,
  Eval(..),
  SymFn1(..),
  SymFn2(..),
  Functionable(..),
  vars_in,
  var,
  var_maybe,
  val,
  is_val,
  sym,
  normalize,
  rewrite_vars
) where


import Data.Binary   (Binary(..))
import Data.IntMap   (IntMap(..), (!?))
import Data.IntSet   (IntSet(..), empty, singleton, union, unions)
import Data.List     (intercalate, sort, sortBy)
import Data.Maybe    (fromMaybe)
import Foreign.Ptr   (FunPtr(..))
import GHC.Generics  (Generic(..))
import Text.Printf   (printf)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.IntSet as IS

import Wumber.ClosedComparable


-- | A symbolic expression in a normal form that's reasonably easy to work with.
--   'Sym' expressions simplify themselves as you build them up. This includes
--   constant folding, condensing common subexpressions (e.g. @x + x@ becomes
--   @2*x@), and keeping track of variable dependencies and invertibility.
--
--   NOTE: 'Sym' needs to be 'Ord' to implement 'Real', but unknown quantities
--   will fail at runtime if you try to compare them. 'SymTerm' etc are 'Ord'
--   because we use orderings to maintain sorted factor lists. 'Sym' instances
--   are ordered with 'OrdSym' for this purpose, but that isn't exposed to the
--   user.

data Sym f a     = [SymTerm f a] :+ !a  deriving (     Eq, Generic, Binary)
data SymTerm f a = !a :* [SymExp f a]   deriving (Ord, Eq, Generic, Binary)
data SymExp f a  = !(SymVar f a) :** !a deriving (Ord, Eq, Generic, Binary)

data SymVar f a = Var  !VarID
                | Poly !(OrdSym f a)
                | Fn1  !SymFn1 IntSet !(OrdSym f a)
                | Fn2  !SymFn2 IntSet !(OrdSym f a) !(OrdSym f a)
                | FnN  !f      IntSet [OrdSym f a]
  deriving (Ord, Eq, Generic, Binary)

infixl 6 :+
infixl 7 :*
infixl 8 :**

type VarID = Int


-- | The workaround we use to add an ordering to 'Sym' without exporting it
--   outside this module.
newtype OrdSym f a = OS { unOS :: Sym f a } deriving (Eq, Generic, Binary)

instance (Ord f, Ord a) => Ord (OrdSym f a) where
  -- Details don't matter here, as long as it's consistent.
  compare (OS (a :+ b)) (OS (c :+ d)) = compare (a, b) (c, d)


-- | All of the typeclasses 'a' will end up belonging to for 'Sym'. I'm aliasing
--   it because otherwise we'd end up writing it out a bunch of times.
type SymConstraints f a = (NumConstraints a, FConstraints f a)

type FConstraints f a = (Eq f,
                         Ord f,
                         Show f,
                         Functionable f ([a] -> a))

type NumConstraints a = (Show a,
                         StaticOrd a,
                         RealFloat a,
                         Integral a,
                         ClosedComparable a)

type AlgConstraints f a = (SymConstraints f a,
                           Functionable f ([Sym f a] -> Sym f a))


-- | An 'Ord' replacement that works with our hidden 'Ord' thing. 'Sym'
--   implements 'Ord' separately in order to provide 'Real', but its ordering is
--   different and will fail at runtime if you try to compare undetermined
--   quantities.
--
--   If this all sounds awful, that's because it is. I don't think Haskell gives
--   us any good options though.

class Eq a => StaticOrd a where
  compare' :: a -> a -> Ordering
  (<<)     :: a -> a -> Bool
  a << b = compare' a b == LT

instance {-# OVERLAPPABLE #-} (Eq a, Ord a) => StaticOrd a where compare' = compare
instance (Eq a, Eq f, Ord a, Ord f) => StaticOrd (Sym f a) where
  compare' a b = compare' (OS a) (OS b)


type ShowConstraints f a = (Num a, Eq a, Show a, Show f)

instance ShowConstraints f a => Show (OrdSym f a) where
  show (OS x) = show x

instance ShowConstraints f a => Show (Sym f a) where
  show ([] :+ n) = show n
  show (xs :+ 0) = intercalate " + " (map show xs)
  show (xs :+ n) = intercalate " + " (map show xs ++ [show n])

instance ShowConstraints f a => Show (SymTerm f a) where
  show (n :* []) = "NONNORM_TERM " ++ show n
  show (1 :* es) = intercalate "·" (map show es)
  show (n :* es) = intercalate "·" (show n : map show es)

instance ShowConstraints f a => Show (SymExp f a) where
  show (x :** 0) = "NONNORM_EXP 1"
  show (x :** 1) = show x
  show (x :** 2) = show x ++ "²"
  show (x :** 3) = show x ++ "³"
  show (x :** 4) = show x ++ "⁴"
  show (x :** n) = show x ++ "^" ++ show n

instance ShowConstraints f a => Show (SymVar f a) where
  show (Var 0)       = "x"
  show (Var 1)       = "y"
  show (Var 2)       = "z"
  show (Var 3)       = "t"
  show (Var i)       = "v" ++ show i
  show (Poly v)      = "(" ++ show v ++ ")"
  show (Fn1 f _ x)   = show f ++ "(" ++ show x ++ ")"
  show (Fn2 f _ x y) = show f ++ "(" ++ show x ++ ", " ++ show y ++ ")"
  show (FnN f _ xs)  = show f ++ "(" ++ intercalate ", " (map show xs) ++ ")"


-- | Evaluate a symbolic quantity using Haskell math. To do this, we need two
--   functions: one to handle terminals and one to handle 'Var' values.
--
--   'eval' is how all rewriting is handled, whether that's variable
--   substitution or reducing forms to constants.

class Eval t r a b | a b -> t, a b -> r where
  eval :: (t -> r) -> (VarID -> r) -> a -> b

type SymConstraints2 f a b = (SymConstraints f a, SymConstraints f b)

instance SymConstraints2 f a b => Eval a b (Sym f a) b where
  eval !t f (!ts :+ b) = t b + sum (map (eval t f) ts)

instance SymConstraints2 f a b => Eval a b (SymTerm f a) b where
  eval !t f (a :* (!xs)) = t a * product (map (eval t f) xs)

instance SymConstraints2 f a b => Eval a b (SymExp f a) b where
  eval !t f (x :** n) = eval t f x ** t n

instance SymConstraints2 f a b => Eval a b (SymVar f a) b where
  eval _ f (Var i)                  = f i
  eval t f (Poly (OS v))            = eval t f v
  eval t f (Fn1 op _ (OS x))        = fn op (eval t f x)
  eval t f (Fn2 op _ (OS x) (OS y)) = fn op (eval t f x) (eval t f y)
  eval t f (FnN op _ xs)            = fn op (map (eval t f . unOS) xs)


-- | Re-evaluates a symbolic structure, allowing it to potentially collapse
--   certain unreduced terms. It isn't normally necessary to do this.
normalize :: AlgConstraints f a => Sym f a -> Sym f a
normalize = eval val var


-- | Turns a partial variable resolver function into a full function, by leaving
--   unbound variables abstract.
var_maybe :: SymConstraints f a => (VarID -> Maybe (Sym f a)) -> VarID -> Sym f a
var_maybe f i = fromMaybe (var i) (f i)


-- | Resolves a specific set of variables, leaving others unmodified.
--
--   TODO: optimize by skipping if 'vars_in' is disjoint

rewrite_vars :: AlgConstraints f a => IntMap (Sym f a) -> Sym f a -> Sym f a
rewrite_vars m = eval val (var_maybe (m !?))


class    ToSym t       where sym :: SymConstraints f a => t f a -> Sym f a
instance ToSym Sym     where sym   = id
instance ToSym SymTerm where sym a = [a] :+ 0
instance ToSym SymExp  where sym a = sym $ 1 :* [a]
instance ToSym SymVar  where sym a = sym $ a :** 1


val :: a -> Sym f a
val = ([] :+)

is_val :: Sym f a -> Bool
is_val ([] :+ _) = True
is_val _         = False

var :: SymConstraints f a => VarID -> Sym f a
var = sym . Var

fn1 :: SymConstraints f a => SymFn1 -> Sym f a -> Sym f a
fn1 f ([] :+ x) = val $ fn f x
fn1 f x         = sym $ Fn1 f (vars_in x) (OS x)

fn2 :: SymConstraints f a => SymFn2 -> Sym f a -> Sym f a -> Sym f a
fn2 f ([] :+ x) ([] :+ y) = val $ fn f x y
fn2 Pow a b | is_val b    = ppow a b
fn2 f x y                 = sym $ Fn2 f (vars_in x `union` vars_in y) (OS x) (OS y)

fnn :: SymConstraints f a => f -> [Sym f a] -> Sym f a
fnn f xs | all is_val xs = val $ fn f (map (\([] :+ n) -> n) xs)
fnn f xs                 = sym $ FnN f (unions (map vars_in xs)) (map OS xs)


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
padd :: SymConstraints f a => Sym f a -> Sym f a -> Sym f a
padd (!xs :+ a) (!ys :+ b)
  | isNaN a || isNaN b || isInfinite a || isInfinite b = [] :+ (a + b)
  | otherwise = filter nonzero (merge_with te id tadd xs ys) :+ (a + b)
  where nonzero (n :* _) = n /= 0
        te (_ :* es) = es
        tadd (a :* x) (b :* _) | a + b /= 0 = (a + b) :* x
                               | otherwise  = 0 :* []


-- | Polynomial multiplication with term grouping.
pmul :: SymConstraints f a => Sym f a -> Sym f a -> Sym f a
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
ppow :: SymConstraints f a => Sym f a -> Sym f a -> Sym f a
ppow _ 0 = 1
ppow ([] :+ 1) _         = 1
ppow ([] :+ x) ([] :+ n) = val (x ** n)

ppow a ([] :+ n) | n == truncate n && n > 0 =
                   half * half * (if n `mod` 2 == 0 then 1 else a)
  where half = ppow a (val (n `quot` 2))

ppow ([a :* es] :+ 0) ([] :+ n) = [a**n :* map (\(v:**e) -> v:**(e*n)) es] :+ 0
ppow a ([] :+ n) = sym (Poly (OS a) :** n)

ppow a b = fn2 Pow a b


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
            | Truncate
            | Round
            | Ceiling
            | Floor

            -- These functions aren't used directly by Sym, but may be emitted
            -- during JIT compilation (and have corresponding C math functions).
            | Negate
            | Sqrt

  deriving (Show, Ord, Eq, Generic, Binary, Enum)

data SymFn2 = Quot
            | Rem
            | Pow       -- NOTE: just for non-constant exponents
            | Upper
            | Lower
            | Atan2

            -- Not used directly by Sym, but are useful when emitting JIT.
            | Add
            | Subtract
            | Multiply
            | Divide

  deriving (Show, Ord, Eq, Generic, Binary, Enum)


class Functionable x t where
  fn :: x -> t

instance Functionable () a where
  fn _ = error "Functionable is disabled for the () instance"

instance NumConstraints a => Functionable SymFn1 (a -> a) where
  fn Abs      = abs
  fn Signum   = signum
  fn Log      = log
  fn Exp      = exp
  fn Sqrt     = sqrt
  fn Negate   = negate
  fn Sin      = sin
  fn Cos      = cos
  fn Tan      = tan
  fn Asin     = asin
  fn Acos     = acos
  fn Atan     = atan
  fn Sinh     = sinh
  fn Cosh     = cosh
  fn Tanh     = tanh
  fn Asinh    = asinh
  fn Acosh    = acosh
  fn Atanh    = atanh
  fn Truncate = truncate
  fn Round    = round
  fn Ceiling  = ceiling
  fn Floor    = floor

instance NumConstraints a => Functionable SymFn2 (a -> a -> a) where
  fn Add      = (+)
  fn Subtract = (-)
  fn Multiply = (*)
  fn Divide   = (/)
  fn Pow      = (**)
  fn Quot     = quot
  fn Rem      = rem
  fn Upper    = upper
  fn Lower    = lower
  fn Atan2    = atan2


-- | Returns a set of all 'Var' indexes used by an expression.
class HasVars a where vars_in :: a -> IntSet

instance SymConstraints f a => HasVars (Sym f a) where
  vars_in (xs :+ b) = unions (map vars_in xs)

instance SymConstraints f a => HasVars (SymTerm f a) where
  vars_in (a :* xs) = unions (map vars_in xs)

instance SymConstraints f a => HasVars (SymExp f a) where
  vars_in (x :** n) = vars_in x

instance SymConstraints f a => HasVars (SymVar f a) where
  vars_in (Var i)       = singleton i
  vars_in (Poly (OS v)) = vars_in v
  vars_in (Fn1 _ v _)   = v
  vars_in (Fn2 _ v _ _) = v
  vars_in (FnN _ v _)   = v


instance Bounded a => Bounded (Sym f a) where
  minBound = val minBound
  maxBound = val maxBound

instance SymConstraints f a => Num (Sym f a) where
  fromInteger = val . fromInteger
  abs         = fn1 Abs
  signum      = fn1 Signum
  negate      = ((-1) *)
  (+)         = padd
  (*)         = pmul

instance SymConstraints f a => Fractional (Sym f a) where
  fromRational = val . fromRational
  recip x      = ppow x (-1)

instance SymConstraints f a => Floating (Sym f a) where
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

instance SymConstraints f a => ClosedComparable (Sym f a) where
  lower a b = fn2 Lower a b
  upper a b = fn2 Upper a b


-- HERE THERE BE GENERAL WEIRDNESS
-- We need to lie a little about the nature of 'Double' and 'Float'. Basically,
-- these types need to be 'Integral' so they satisfy our 'SymConstraints'.

-- | A quotient/remainder function we reuse in several places.
--
--   NOTE: this results in 'divMod' and 'quotRem' that behave identically,
--   including for negative divisors. This is technically not the way 'mod' and
--   'rem' should be defined, but it lets us keep things simple for now.
--
--   TODO: is the fix as simple as having 'qr' use 'truncate' and 'divmod' use
--   'floor'?

qr a b = (q, r) where q = truncate (a / b)
                      r = a - q*b

instance Integral Double where toInteger = truncate; quotRem = qr
instance Integral Float  where toInteger = truncate; quotRem = qr


-- HERE THERE BE PARTIAL INSTANCES
-- The goal here is to be able to use regular Haskell math functions, even
-- though many of them require us to pretend to support operations we have no
-- hope of providing. In my opinion, it's more useful to support these
-- operations than it is to be type safe.

instance SymConstraints f a => Ord (Sym f a) where
  compare ([] :+ a) ([] :+ b) = compare a b
  compare a b = error $ "can't compare undetermined Syms "
                        ++ show a ++ " and " ++ show b ++ " (did you want "
                        ++ "to use upper/lower instead of max/min?)"

instance SymConstraints f a => Enum (Sym f a) where
  toEnum     = fromInteger . toInteger
  fromEnum _ = error "can't collapse Sym to Integer via fromEnum"
  pred x     = x - 1
  succ x     = x + 1

instance SymConstraints f a => Real (Sym f a) where
  -- TODO
  -- toRational is fine when 'is_val' is true
  toRational _ = error "can't collapse Sym to Rational via toRational"

instance SymConstraints f a => Integral (Sym f a) where
  -- TODO
  -- toInteger is fine when 'is_val' is true
  toInteger _ = error "can't collapse Sym to Integer via toInteger"
  quotRem a b = (fn2 Quot a b, fn2 Rem a b)

instance SymConstraints f a => RealFrac (Sym f a) where
  -- FIXME
  -- This instance can segfault and stuff. Is there something similar to
  -- 'unsafeCoerce' but that errors out instead of doing arbitrarily bad things?
  -- (Data.Coerce.coerce isn't quite it because it fails the forall check.)
  properFraction a = (unsafeCoerce q, r) where (q, r) = qr a 1
  truncate         = unsafeCoerce . fn1 Truncate
  round            = unsafeCoerce . fn1 Round
  ceiling          = unsafeCoerce . fn1 Ceiling
  floor            = unsafeCoerce . fn1 Floor

instance SymConstraints f a => RealFloat (Sym f a) where
  -- TODO
  -- Most of these methods are fine if we know the 'Sym' is a constant. We
  -- should allow that to be consistent with some of the other hackery above.

  -- ...and yes, I am implementing this whole typeclass just to get atan2.
  floatRadix _     = error "floatRadix is undefined on Syms"
  floatDigits _    = error "floatDigits is undefined on Syms"
  floatRange _     = error "floatRange is undefined on Syms"
  decodeFloat _    = error "decodeFloat is undefined on Syms"
  encodeFloat _ _  = error "decodeFloat is undefined on Syms"
  isDenormalized _ = error "isDenormalized is undefined on Syms"
  isNegativeZero _ = error "isNegativeZero is undefined on Syms"
  isIEEE _         = error "isIEEE is undefined on Syms"

  -- NaN and infinity propagate upwards due to behavior in 'padd'. All we need
  -- to do is check the constant term.
  isNaN      (_ :+ n) = isNaN n
  isInfinite (_ :+ n) = isInfinite n
  atan2 a b           = fn2 Atan2 a b
