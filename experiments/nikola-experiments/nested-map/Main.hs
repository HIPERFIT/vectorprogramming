{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Data.Array.Nikola.Backend.CUDA.Haskell as NH
import qualified Data.Array.Nikola.Language.Reify as R
import qualified Data.Array.Nikola.Language.Syntax as S
-- import Data.Int
import qualified Data.Vector.CUDA.Storable as CV
import qualified Data.Vector as V

import qualified Nested as M

import Data.Array.Nikola.Backend.CUDA
import Data.Int

-- mapTest :: CV.Vector Int32-> CV.Vector Int32
-- mapTest = NH.compile M.mapTest

look3Test :: CV.Vector Int32 -> Int32 -- V.Vector Int32 -> Int32
look3Test = NH.compile (M.look3 :: Array G (Z :. Exp Ix) (Exp Int32) -> Exp Int32)

main = do print $ look3Test $ CV.fromList [1..10]
