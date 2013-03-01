{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RebindableSyntax #-}

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

import Data.Word (Word32)
import Data.Int (Int32)
import Data.Array.Nikola.Combinators
import Data.Array.Nikola.Backend.CUDA
import Data.Array.Nikola.Backend.CUDA.Haskell
import Prelude hiding (map, zipWith, replicate, (^))

grayCode :: Exp Int32 -> Exp Int32
grayCode n = ((fromIntegral n) `xor` ((fromIntegral n) `shiftR` 1))

type Vec r = Array r (Z :. Exp Ix) (Exp Int32)

fromBool :: Unlift CUDA (Exp a) => Num (Exp a) => Exp Bool -> Exp a
fromBool b = if b then 1 else 0

bit x = 1 `shiftL` x

bitVec :: Exp Int32 -> Array D DIM1 (Exp Int32)
bitVec e = map (fromBool . (/=* 0)) $ zipWith (&*) pow2s $ replicate 32 e
  where
    pow2s :: Array D DIM1 (Exp Int32)
    pow2s = fromFunction (Z :. 32) (\(Z :. i) -> bit $ fromInt i)

-- Should be an argument.
-- dirv     :: Vec G
-- dirv = undefined

-- @sobol dirv m p == {m*2^p sobol numbers}@
sobol :: Array G DIM1 (Exp Int32) -> Exp Int32 -> Exp Int32
               -> Array PSH (Z :. Exp Ix) (Exp Int32) --Array PSH (Z :. Exp Ix :. Exp Ix) (Exp Int32)
sobol dirv m p = reshapePSH (Z :. m*p) $ mapNest (Z :. m)
                     (\ m' x -> unfoldP (Z :. m) (\ (Z :. i) -> sobolRecGem dirv p (m'+i)) (x ! Z ) )
                     (map (sobolInd dirv) (iota p))
  where
    iota :: Exp Int32 -> Array D (Z :. Exp Ix) (Exp Int32)
    iota n = fromFunction (Z :. n) (toIndex $ Z :. n)

-- | @sobolInd dirv n == y_n@. (unnormalised)
sobolInd :: Source r (Exp Int32) => Array r DIM1 (Exp Int32) -> Exp Int32 -> Exp Int32
sobolInd dirVs ix = (fold xor 0 xs) ! Z
  where
    xs :: Array D DIM1 (Exp Int32)
    xs = zipWith (*) dirVs (bitVec $ grayCode ix :: Vec D)

-- | @sobolGem dirv jumpExp n y_n == y_{n+2^jumpExp}@
sobolRecGem :: Source r (Exp Int32) => Array r DIM1 (Exp Int32)  -> Exp Int32 -> Exp Int32 -> Exp Int32 -> Exp Int32
sobolRecGem dirv jmp n yn =
  (yn `xor` (dirv ! (Z:. jmp)))
  `xor` (dirv ! (Z:. leastUnsetBit (n |* (1 `shiftL` jmp) - 1)))

leastUnsetBit :: Exp Int32 -> Exp Int32
leastUnsetBit x = leastSetBit $ (bit 32 - 1) `xor` x -- anyone knows of a better way to get all ones? ((-1) | 1) perhaps?

leastSetBit :: IsBits a => Exp a -> Exp Int32
leastSetBit x = 6666 -- undefined -- __ffs x

