{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}

module WumberTest.AMD64Asm where


import Control.Concurrent   (forkIO)
import Control.Monad        (replicateM, when)
import Data.Char            (chr)
import Data.Maybe           (fromMaybe, fromJust, isNothing, isJust)
import Data.Set
import Data.Vector.Storable (Vector(..), (!))
import Debug.Trace          (traceShowId)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Generic.Random       (genericArbitraryU)
import System.IO            (hClose, hFlush, stdout)
import System.IO.Unsafe     (unsafePerformIO)
import System.Posix.Types
import System.Process
import Test.QuickCheck

import qualified Data.Binary          as Bin
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Vector.Storable as VS

import Wumber.AMD64Asm
import Wumber.JIT
import Wumber.JITIR
import Wumber.Symbolic


-- Debug stuff
show_expressions = False
show_disassembly = False


debug :: VS.Vector Double -> Sym Double -> BS.ByteString -> IO ()
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


instance Arbitrary MathFn where
  arbitrary = genericArbitraryU

instance Arbitrary (Sym Double) where
  arbitrary = oneof [ terminal, nonterminal ]
    where terminal    = N <$> arbitrary
          nonterminal = sized \n -> do v <- genericArbitraryU
                                       case v of Arg _ -> Arg <$> choose (0, n)
                                                 _     -> return v

  shrink x = case binary x of
    Just (x, y) -> [x, y]
    _           -> []

instance Arbitrary (Vector Double) where
  arbitrary = sized \n -> VS.fromList <$> replicateM n arbitrary


foreign import ccall unsafe "unistd.h fork"      c_fork    :: IO Pid
foreign import ccall unsafe "unistd.h _exit"     c_exit    :: Int -> IO ()
foreign import ccall unsafe "sys/wait.h waitpid" c_waitpid
  :: Pid -> Ptr CInt -> Int -> IO Pid


bulletproof :: Bin.Binary a => IO a -> IO (Maybe a)
bulletproof thing = do
  (r, w) <- createPipe
  p <- c_fork
  if p /= 0
    then do p_wstat <- malloc
            hClose w
            c_waitpid p p_wstat 0
            stat <- peek p_wstat
            free p_wstat
            if stat /= 0
              then return Nothing
              else Just <$> Bin.decode <$> BL.hGetContents r

    else do x <- thing
            hClose r
            BL.hPut w (Bin.encode x)
            hFlush w
            hClose w
            c_exit 0
            return Nothing      -- unreachable


forkjit :: BS.ByteString -> Sym Double -> Vector Double -> IO (Maybe Double)
forkjit code s v = do debug v s code
                      bulletproof $ with_jit dblfn code $ VS.unsafeWith v


prop_symjit :: Sym Double -> Vector Double -> Property
prop_symjit s v = size_ok && bounds_ok ==> property test
  where l         = VS.length v
        size_ok   = l > 0 && l <= 2047
        bounds_ok = fromMaybe 0 (lookupMax (args_in s)) < l
        test | isNaN x || isNaN y' = discard
             | otherwise = counterexample help $ isJust y && abs (x - y') < 1e-8

          where x    = eval (v !) s
                code = assemble_ssa $ linearize s
                y    = unsafePerformIO $ forkjit code s v
                y'   = fromMaybe (1/0) y
                help = "\n\n" ++ show (s, x, y) ++ "\n\n"
                       ++ BU.toString (unsafePerformIO (ndisasm code))


return []
runTests = $quickCheckAll
