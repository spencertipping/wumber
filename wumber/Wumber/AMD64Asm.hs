{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Wumber.AMD64Asm where


import Control.Monad.RWS (RWS, runRWS)
import Data.Bits
import GHC.Word          (Word16(..))

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as BL
import qualified Foreign.Ptr             as P

import Wumber.JIT
import Wumber.Symbolic


type RegisterState = ()
type AMD64Asm a = RWS () RegisterState B.Builder a
