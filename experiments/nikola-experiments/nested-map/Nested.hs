{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Nested where

import Prelude hiding (enumFromTo)

-- import qualified Data.Array.Nikola.Exp as E
import Data.Array.Nikola.Backend.CUDA
import Data.Int

mapNest ::
  IsArray r a =>
  Shape sh    =>
  Source r a  =>
  IsElem a    =>
  IsElem b    =>
  sh -> (a -> Array D sh b) -> Array r (Z :. Exp Ix) a -> Array D (sh :. Exp Ix) b
mapNest sh f arr =
    let (Z :. i) = extent arr
    in fromFunction (sh :. i) (\ (sh :. ix) ->
      let (_, lookup) = toFunction (f (arr ! (Z :. ix)))
      in lookup sh)


enumFromTo :: Exp Int32 -> Exp Int32 -> Array D (Z :. Exp Ix) (Exp Int32)
enumFromTo from to = fromFunction (Z :. (to - from)) (\ (Z :. i) -> from + i)

adds :: Exp Ix -> Exp Int32 -> Array D (Z :. Exp Ix) (Exp Int32)
adds sh offset = fromFunction (Z :. sh) (\ (Z :. ix) -> offset + ix)

mapTest :: Source r (Exp Int32) => Array r (Z :. Exp Ix) (Exp Int32) ->  Array D (Z :. Exp Ix :. Exp Ix) (Exp Int32)
mapTest arr = let sh = Z :. 10
 in mapNest sh (adds 10) arr -- (fromFunction (Z :. 4) (\ (Z :. i) -> i))

look3 :: Source r (Exp Int32)  => Array r (Z :. Exp Ix) (Exp Int32) -> Exp Int32
look3 arr = arr ! (Z :. 3)
