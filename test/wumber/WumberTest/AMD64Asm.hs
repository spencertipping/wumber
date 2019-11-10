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


debug :: Sym Double -> BS.ByteString -> IO ()
debug sym machinecode = do
  when show_expressions $ putStrLn (show sym)
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


gen_symtree :: Int -> Gen (Sym Double)
gen_symtree maxarg = resize maxarg arbitrary


prop_symjit :: Vector Double -> Property
prop_symjit v = VS.length v > 0 && VS.length v <= 2048 ==> property do
  s <- gen_symtree (VS.length v - 1)
  let code = assemble_ssa $ linearize s
      fn   = unsafePerformIO do debug s code; return $! VS.unsafeWith v
      m    = with_jit dblfn code fn
      x    = eval (v !) s
  if isNaN x
    then discard
    else return $ counterexample (show s) $ x === unsafePerformIO m


return []
runTests = $quickCheckAll
