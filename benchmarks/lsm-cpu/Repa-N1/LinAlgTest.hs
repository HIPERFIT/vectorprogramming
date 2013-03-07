{-# LANGUAGE FlexibleContexts #-}
module Main where

import Test.Framework (interpretArgsOrExit, defaultMainWithOpts, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import qualified Test.HUnit as H

import Data.Array.Repa

import Prelude hiding (and, zipWith)

import LinAlg

main = do opts <- interpretArgsOrExit ["--plain"]
          defaultMainWithOpts [tests] opts

tests = testGroup "Data.Array.Repa LinAlg"
          [testCase "cholesky0" choleskytest0,
           testCase "cholesky1" choleskytest1,
           testCase "cholesky2" choleskytest2,
           testCase "forwardSubst0" forwardSubstTest0,
           testCase "test0" test0]

approxEq :: Ord e => Fractional e => Source r1 e => Source r2 e => Shape sh => Array r1 sh e -> Array r2 sh e -> Bool
approxEq a b = foldAllS (&&) True $ zipWith isEq a b
  where
    isEq x y = abs (x - y) < epsilon
    epsilon = 0.1

--- Cholesky tests
choleskytest0 = mkCholeskyTest $ fromListUnboxed (Z :. 3 :. 3) [25, 15, -5, 15, 18, 0, -5, 0, 11]
choleskytest1 = mkCholeskyTest $ fromListUnboxed (Z :. 4 :. 4) [18, 22, 54, 42, 22, 70, 86, 62, 54, 86, 174, 134, 42, 62, 134, 106]

-- Expected cholesky matrix: [2.0,0.0,0.0,
--                            6.0,1.0,0.0,
--                           -8.0,5.0,3.0]
choleskytest2 = mkCholeskyTest $ fromListUnboxed (Z :. 3 :. 3) [4,12,-16,12,37,-43,-16,-43,98]

mkCholeskyTest :: Array U DIM2 Double -> H.Assertion
mkCholeskyTest m = do
  l <- cholesky (delay m)
  prod <- matProd (delay l) (transpose l)
  if approxEq m prod
    then return ()
    else H.assertFailure $ show prod

-- Forward substitution testing

forwardSubstTest0 = mkForwardSubstTest 
                      (fromListUnboxed (Z :. 3 :. 3) [1,0,0,3,1,0,1,-1,1])
                      (fromListUnboxed (Z :. 3) [2,8,0])
                      (fromListUnboxed (Z :. 3) [2,2,0])
mkForwardSubstTest :: Array U DIM2 Double -> Array U DIM1 Double -> Array U DIM1 Double -> H.Assertion
mkForwardSubstTest l b result = do
  let x = computeUnboxedS $ forwardSubstitute (delay l) (delay b)
  if approxEq x result
    then return ()
    else H.assertFailure $ show x
         

test0 = mkTest (fromListUnboxed (Z :. 3 :. 3) [25,-5,10,-5,17,10,10,10,62])
               (fromListUnboxed (Z :. 3) [55,-19,114])

mkTest :: Array U DIM2 Double -> Array U DIM1 Double -> H.Assertion
mkTest m b = do
  l <- cholesky (delay m)
  let y = computeUnboxedS $ forwardSubstitute (delay l) (delay b)
  let x = computeUnboxedS $ backwardSubstitute (delay l) (delay y)
  prod <- computeUnboxedS `fmap` ((delay m) `matVecProd` (delay x))
  if approxEq prod b
    then return ()
    else H.assertFailure $ show y
                  
         
-- testx :: Array U DIM1 Double
-- testx = (fromListUnboxed (Z :. 3) [2,8,0])