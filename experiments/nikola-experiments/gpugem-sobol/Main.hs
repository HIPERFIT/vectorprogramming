{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Array.Nikola.Backend.Main(defaultMain)
import GpuGemSobol

-- There should really be a nikola debug module containing all these imports and definitions.

-- For inspecting reification
import Data.Int(Int32)
import Data.Typeable(Typeable(..))
import Control.Applicative

import Data.Array.Nikola.Array
import Data.Array.Nikola.Language.Check
import Data.Array.Nikola.Language.Monad
import Data.Array.Nikola.Language.Reify
import Data.Array.Nikola.Language.Optimize as O
import Data.Array.Nikola.Language.Sharing as Share
import Data.Array.Nikola.Language.Syntax as S
import Data.Array.Nikola.Language.Generic
import Data.Array.Nikola.Backend.CUDA as C
import Data.Array.Nikola.Backend.C.Codegen
import Data.Array.Nikola.Backend.Flags
import qualified Data.Array.Nikola.Exp as E
import Data.Array.Nikola.Combinators
import Text.PrettyPrint.Mainland

import qualified Prelude as P
import Prelude (($), Num(..), IO, snd, Monad(..), Show(..), Read(..), putStrLn, (.), undefined)
import Control.Monad.Identity

u = undefined

-- usefull alias. we write this all the time..
type N = C.Exp Int32

runR' :: R a a -> IO a
runR' m = snd <$> runR m defaultREnv

reify' :: Reifiable a S.Exp => a -> P S.Exp
reify' x = reify x

printR :: Reifiable a S.Exp => a -> IO ()
printR x = runR' (reify' x) >>= putStrLn . show . ppr


-- Here starts actual debugging

main = defaultMain (sobol)

-- Important knowledge! : in order to compile an 'iterate(While)?', we rely on
-- the fact that Share.detectSharing eliminates the DelayedE.
itExp = iterate (10 :: N) ((1 ::N) + ) (0 :: N)

sh10 = Z :. (10 ::N)

-- With the presence of mapNest, fold doesn't have to be so complicated.
fold3 ::
  forall a b t r .
  IsElem (E.Exp t Ix) =>
  IsElem (E.Exp t a) =>
  IsElem (E.Exp t b) =>
  (E.Exp t a -> E.Exp t b -> E.Exp t a) -> E.Exp t a -> Array PSH (Z :. E.Exp t Ix) (E.Exp t b) -> E.Exp t a
fold3 f a bs =
  E.E $ S.DelayedE $ do
    let APush sh bsM = bs
    vTmp <- gensym "tmpIn"
    loop <- reset $ do
      (i,x) <- bsM
      return $
        bindE vTmp tau (ReturnE $ VarE vTmp)
          $ bindE vTmp tau (ReturnE $ unE $ f (varE $ V vTmp) x) (ReturnE UnitE ) -- $ VarE vTmp)

      --   | LetE Var Type Occ Exp Exp

    return $ S.bindE vTmp tau (ReturnE $ unE a) ((sequentializeFor loop) `SeqE` (ReturnE $ VarE vTmp))
    -- return $ S.LetE vTmp tau S.Many (unE a) ((sequentializeFor loop) `SeqE` (VarE vTmp))

  where
    tau :: S.Type
    tau = S.ScalarT $ E.typeOf (undefined :: E.Exp t a)

fold3Exp :: Array G (Z :. E.Exp CUDA Ix) (E.Exp CUDA Int32) -> E.Exp CUDA Int32
fold3Exp x = fold3 (+) (1::N) (push x)-- (push $ fromFunction sh10 (toIndex sh10))

unfoldExp :: E.Exp CUDA Int32 -> Array PSH　(Z :. E.Exp CUDA Ix) (E.Exp CUDA Int32)
unfoldExp x = unfoldP (Z :. (10::N)) (\_ x -> x + 1) x

-- Sequentialize the outermost for-loop.
sequentializeFor :: S.Exp -> S.Exp
sequentializeFor loop = runIdentity $ go ExpA loop
  where
    go :: Traversal AST Identity
    go ExpA (ForE _ vs is body) = return $ ForE SeqParFor vs is body
    go x loop = traverseFam go x loop


