{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}

module WumberTest.AMD64Asm where


import Control.Monad        (replicateM, when)
import Data.Char            (chr)
import Data.Vector.Storable (Vector(..), (!))
import Debug.Trace          (traceShowId)
import Generic.Random       (genericArbitraryU)
import System.IO            (hClose)
import System.IO.Unsafe     (unsafePerformIO)
import System.Process
import Test.QuickCheck

import qualified Data.ByteString      as BS
import qualified Data.Vector.Storable as VS

import Wumber.AMD64Asm
import Wumber.JIT
import Wumber.JITIR
import Wumber.Symbolic


show_expressions = True
show_disassembly = True


debug :: VS.Vector Double -> Sym Double -> BS.ByteString -> IO ()
debug v sym machinecode = do
  when show_expressions $ putStrLn (show (VS.length v, sym))
  when show_disassembly do
    (Just i, _, _, p) <- createProcess
                         (proc "ndisasm" ["-b64", "-"]) { std_in = CreatePipe }
    BS.hPut i machinecode
    hClose i
    waitForProcess p
    return ()


instance Arbitrary MathFn where
  arbitrary = genericArbitraryU

instance Arbitrary (Sym Double) where
  arbitrary = oneof [ terminal, nonterminal ]
    where terminal    = N <$> arbitrary
          nonterminal = sized \n -> do v <- genericArbitraryU
                                       case v of Arg _ -> Arg <$> choose (0, n)
                                                 _     -> return v

instance Arbitrary (Vector Double) where
  arbitrary = sized \n -> VS.fromList <$> replicateM n arbitrary


prop_symjit :: Sym Double -> Vector Double -> Property
prop_symjit s v = VS.length v > 0 && VS.length v <= 2047 ==> property do
  let code = assemble_ssa $ linearize s
      fn   = unsafePerformIO do debug v s code; return $! VS.unsafeWith v
      m    = with_jit dblfn code fn
      x    = eval (v !) s
      y    = unsafePerformIO m
      diff = abs (x - y)
  if isNaN x || isNaN y
    then discard
    else counterexample (show (s, x, y)) $ diff < 1e-8


return []
runTests = $quickCheckAll
