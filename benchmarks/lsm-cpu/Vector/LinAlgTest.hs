module Main where

import Test.Framework (interpretArgsOrExit, defaultMainWithOpts, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import qualified Test.HUnit as H

import qualified Data.Vector as B
import Data.Vector.Unboxed hiding ((++))

import Prelude hiding (and, zipWith)

import LinAlg

main = do opts <- interpretArgsOrExit ["--plain"]
          defaultMainWithOpts [tests] opts

tests = testGroup "Data.Vector LinAlg"
          [testCase "cholesky0" choleskytest0,
           testCase "cholesky1" choleskytest1]

choleskytest0 = mkCholeskyTest $ B.fromList [fromList [25, 15, -5], fromList [15, 18, 0], fromList [-5, 0, 11]]
choleskytest1 = mkCholeskyTest $ B.fromList [fromList [18, 22, 54, 42], fromList [22, 70, 86, 62], fromList [54, 86, 174, 134], fromList [42, 62, 134, 106]]

mkCholeskyTest :: B.Vector (Vector Double) -> H.Assertion
mkCholeskyTest m = H.assertBool (show prod) $ approxEq m prod
  where
  l = cholesky m
  prod = matProd l (transpose l)


approxEq :: B.Vector (Vector Double) -> B.Vector (Vector Double) -> Bool
approxEq a b = B.and $ B.zipWith (\x y -> and $ zipWith isEq x y) a b
  where
    isEq x y = abs (x - y) < epsilon
    epsilon = 0.1
    