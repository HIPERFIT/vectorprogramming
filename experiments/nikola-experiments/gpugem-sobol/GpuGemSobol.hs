{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module GpuGemSobol where

{- High level description:
 -
 - map (unfold 100 (sobolGem dirv 16)) (map (sobolInd dirv) iota(16))
 -
 - Resulting Matrix:
 -
 - sr_0   sr_1  ... sr_15
 - sg_16  sg_17 ... sg_31
 - ...    ...
 - ...    ...
 - ...    ...
 - sg_1600
 -}

import Data.Int(Int32)
import Data.Array.Nikola.Backend.CUDA hiding (mapNest)
import Data.Array.Nikola.Backend.CUDA.Haskell
import Prelude hiding (map)

greyCode :: Exp Int32 -> Exp Int32
greyCode = undefined

type Vec r = Array r (Z :. Exp Ix) Int32

bitVec   :: Exp Int32 -> Vec D
bitVec = undefined

-- Should be an argument.
dirv     :: Vec G
dirv = undefined

-- @sobol dirv m p == {m*2^p sobol numbers}@
-- We can't reshape because it returns a push array :-(
sobol :: Vec r -> Exp Int32 -> Exp Int32 -> Array PSH (Z :. Exp Ix :. Exp Ix) (Exp Int32)
sobol dirv m p = mapNest (Z :. m)
                     (\ m' x -> unfold (Z :. m) (\ (Z :. i) -> sobolRecGem dirv p (m'+i)) (x ! Z ) )
                     (map (sobolInd dirv) (iota p))
  where
    iota :: Exp Int32 -> Array D (Z :. Exp Ix) (Exp Int32)
    iota n = fromFunction (Z :. n) (toIndex $ Z :. n)

-- | @sobolInd dirv n == y_n@.
sobolInd :: Vec r -> Exp Int32 -> Exp Int32
sobolInd = undefined

-- | @sobolGem dirv jumpExp n y_n == y_{n+2^jumpExp}@
sobolRecGem :: Vec r -> Exp Int32 -> Exp Int32 -> Exp Int32 -> Exp Int32
sobolRecGem = undefined

-- sobolRecGem dirv jmp yn = yn `xor` (dirv ! jmp) `xor` (dirv ! leastUunsetBit(n `or` 2**jmp - 1))
--   where leastUnsetBit x = leastSetBit $ 2^32-1 `xor` x -- anyone knows of a better way to get all ones? ((-1) | 1) perhaps?
--         leastSetBit x = __ffs x

type family (:+:) a b :: *
type instance sh :+: Z = sh
type instance sh :+: (sh' :. Exp Ix) = (sh :. Exp Ix) :+: sh'

mapNest ::
  Shape sh =>
  IsElem a =>
  IsElem b =>
  Source r a =>
      sh' -> (Exp Ix -> Array r sh a -> Array r' sh' b)
          -> Array r  (sh :. Exp Ix) a
          -> Array r' ((sh :. Exp Ix) :+: sh')   b
mapNest = undefined

unfold :: sh -> (sh -> Exp a -> Exp a) -> Exp a -> Array PSH sh (Exp a)
unfold = undefined
