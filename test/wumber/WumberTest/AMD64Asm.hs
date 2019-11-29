{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module WumberTest.AMD64Asm where


import Control.Concurrent   (forkIO)
import Control.Monad        (replicateM, replicateM_, when)
import Data.Char            (chr)
import Data.Foldable        (foldl')
import Data.Maybe           (fromMaybe, fromJust, isNothing, isJust)
import Data.Set             (Set(..), lookupMax)
import Data.Vector.Storable (Vector(..), (!))
import Debug.Trace          (traceShowId)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Generic.Random       (genericArbitraryU)
import Linear.Matrix        ((*!))
import Linear.Metric        (distance)
import Linear.V3            (V3(..))
import System.IO            (hClose, hFlush, stdout)
import System.IO.Unsafe     (unsafePerformIO)
import System.Posix.Types
import System.Process
import Test.QuickCheck      (quickCheck, quickCheckAll,
                             Arbitrary(..), Gen(..), Property(..),
                             discard, counterexample, property, sized, (==>),
                             oneof, choose)

import qualified Data.Binary          as Bin
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Vector.Storable as VS

import Wumber.AMD64Asm
import Wumber.AMD64JIT
import Wumber.BoundingBox
import Wumber.ClosedComparable
import Wumber.Cursor
import Wumber.JIT
import Wumber.JITIR
import Wumber.Numeric
import Wumber.Symbolic
import Wumber.SymbolicJIT


-- Debug stuff
show_expressions = False
show_disassembly = False

bulletproof_jit = False


-- Test model
model :: FConstraints f R => V3 (Sym f R) -> Sym f R
model = moved_by (V3 0 1.1 0) (bolt 0.5 0.4) `iunion` scs
  where
    threads p d v@(V3 x y z) = p ρ z'
      where θ  = atan2 x y
            ρ  = sqrt (x**2 + y**2)
            z' = ((z * d + θ/τ) `mod` 1 + 1) `mod` 1

    t45 od ρ z = od - sin (τ/6) * ρ + abs (z - 0.5)

    x_lt l (V3 x _ _) = l - x
    z_lt l (V3 _ _ z) = l - z

    hex_cap r v = foldl' lower maxBound
      $ map (\θ -> x_lt r (v *! rotate_z_m (val θ))) [0, 60 .. 300]


    bolt od ts = thread_part `iunion` head_part
      where thread_part = (threads (t45 od) 2 . (/ ts)) `iintersect` z_lt 0
            head_part   = hex_cap od `iintersect`
                          cube (BB (V3 minBound minBound 0) (V3 maxBound maxBound 0.5))

    sphere l v = 0.8 - distance v l

    cube (BB (V3 x1 y1 z1) (V3 x2 y2 z2)) (V3 x y z) =
      foldl' lower maxBound [ x - x1, x2 - x, y - y1, y2 - y, z - z1, z2 - z ]


    iunion     f g v = upper (f v) (g v)
    iintersect f g v = lower (f v) (g v)
    inegate    f v   = negate (f v)

    spheres = sphere 0 `iunion` sphere 0.9
    scs     = spheres `iunion` cube (BB (-1.5) (-0.5))
                      `iunion` cube (BB (-1.2) (-0.2))

    moved_by t f v = f (v - t)


show_model :: IO ()
show_model =
  putStrLn (show (model (V3 (var 0) (var 1) (var 2)) :: Sym () Double))

show_model_graph :: IO ()
show_model_graph =
  putStrLn (show (thread (model (V3 (var 0) (var 1) (var 2)) :: Sym () Double)))

show_model_asm :: IO ()
show_model_asm =
  ndisasm (jit_as_machinecode (model (V3 (var 0) (var 1) (var 2)) :: Sym () Double))
    >>= BS.hPut stdout


debug :: VS.Vector Double -> Sym () Double -> BS.ByteString -> IO ()
debug v sym machinecode = do
  when show_expressions $ putStrLn (show (VS.length v, sym))
  when show_disassembly $ BS.hPut stdout =<< ndisasm machinecode

ndisasm :: BS.ByteString -> IO BS.ByteString
ndisasm code = do
  (Just i, Just o, _, p) <- createProcess
                            (proc "ndisasm" ["-b64", "-"]) { std_in  = CreatePipe,
                                                             std_out = CreatePipe }
  forkIO do BS.hPut i code
            hClose i
  !b <- BS.hGetContents o
  waitForProcess p
  return b


instance Arbitrary SymFn1 where arbitrary = genericArbitraryU
instance Arbitrary SymFn2 where arbitrary = genericArbitraryU

instance Arbitrary (Sym () Double) where
  arbitrary = oneof [ terminal, nonterminal ]
    where terminal    = val <$> arbitrary
          nonterminal = sized \n -> do
            v <- genericArbitraryU
            sym <$> case v of Var _     -> Var <$> choose (0, n - 1)
                              FnN _ _ _ -> Var <$> choose (0, n - 1)
                              _         -> return v

instance Arbitrary (OrdSym () Double) where arbitrary = OS <$> arbitrary

{- TODO: port this
  shrink (Fn2 f _ x y) = [x, y]
  shrink (Fn1 f _ x)   = [x]
  shrink _             = []
-}


instance Arbitrary (Vector Double) where
  arbitrary = sized \n -> VS.fromList <$> replicateM n arbitrary


foreign import ccall unsafe "unistd.h fork"      c_fork    :: IO Pid
foreign import ccall unsafe "unistd.h _exit"     c_exit    :: Int -> IO ()
foreign import ccall unsafe "sys/wait.h waitpid" c_waitpid
  :: Pid -> Ptr CInt -> Int -> IO Pid


-- FIXME
-- This function spinloops in the fork sometimes. Possibly a confused select()
-- call given that we've forked. On the bright side, it can be useful if you're
-- trying to track down segfaults in the JIT (when it doesn't hang, it helps
-- quickcheck trim down your test cases).

bulletproof :: Bin.Binary a => IO a -> IO (Maybe a)
bulletproof thing = do
  (r, w) <- createPipe
  p <- c_fork
  if p /= 0
    then do p_wstat <- malloc
            hClose w
            !b <- BS.hGetContents r
            c_waitpid p p_wstat 0
            stat <- peek p_wstat
            free p_wstat
            return if stat /= 0
                   then Nothing
                   else Just $ Bin.decode $ BL.fromStrict b

    else do x <- thing
            hClose r
            BL.hPut w (Bin.encode x)
            hFlush w
            hClose w
            c_exit 0
            return Nothing      -- unreachable


forkjit :: BS.ByteString -> Sym () Double -> Vector Double -> IO (Maybe Double)
forkjit code s v = do
  debug v s code
  do_jit do f <- compile_machinecode dblfn code
            VS.unsafeWith v f
  where do_jit = if bulletproof_jit
                 then bulletproof
                 else fmap Just


prop_symjit :: Sym () Double -> Vector Double -> Property
prop_symjit s v = size_ok && bounds_ok ==> property test
  where l         = VS.length v
        size_ok   = l > 0 && l <= 2047
        bounds_ok = fromMaybe 0 (lookupMax (vars_in s)) < l

        test | isNaN x || isNaN y' = discard
             | isInfinite x        = discard
             | isInfinite y'       = discard
             | otherwise           = counterexample help test_ok

        test_ok = isJust y && close_enough x y'
        x       = eval (v !) s
        code    = assemble_graph (PM 0) (thread s)
        y       = unsafePerformIO $ forkjit code s v
        y'      = fromMaybe (1/0) y
        help    = "\n\n" ++ show (s, x, y) ++ "\n\n"
                         ++ BU.toString (unsafePerformIO (ndisasm code))


-- TODO
-- Our tests fail a lot because we have steep-sloped functions that can compound
-- rounding errors, which differ slightly between Haskell and JIT. No human is
-- likely to write functions as numerically unstable as the ones we get from QC,
-- but that's what we're working with.
--
-- Anyway, I need to fix this at some point but the failure rate isn't too high
-- right now, maybe 1/10k tests.

close_enough :: Double -> Double -> Bool
close_enough x y | x == 0 = abs y < 1e-8
                 | y == 0 = abs x < 1e-8
                 | otherwise = abs (x - y) <= max (abs x) (abs y) * 1e-8


return []
runTests = $quickCheckAll
