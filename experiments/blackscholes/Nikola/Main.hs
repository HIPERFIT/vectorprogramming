module Main where

import Data.Array.Nikola.Backend.CUDA
import Data.Array.Nikola.Backend.Main
import Prelude hiding (map)




type F = Float

foo :: Array D DIM1 (Exp F) -> Array D DIM1 (Array D DIM1 (Exp F))
foo x = map (const x) x

double :: Exp F -> Exp F
double x = 2*x

main = defaultMain $ foo (fromFunction (Z :. 5) (const 2))