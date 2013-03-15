{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Parallel.SeqM where

import Data.Vector

-- | In this modue we try to define a monad that captures our intentions regarding the `sequential` construct.

data SeqM e a = SeqM a

instance Monad (SeqM e) where
  return a = SeqM a
  (SeqM a) >>= f = f a

data SeqPar
data Seq
-- data Par

sequential :: SeqM e a -> SeqM Seq a
sequential (SeqM a) = (SeqM a)

run :: SeqM e a -> IO a
run = undefined

class Compute e e' where
  compute :: SeqM e (Array r a) -> SeqM e' (Array G a)

instance Compute Seq SeqPar where
  compute = undefined

instance Compute Seq Seq where
  compute = undefined

class IsArray r a where
  data Array r a

data D e
data G

--data Delayed a = Delayed Int (Int -> a)

instance IsArray (D e) a where
  data Array (D e) a = Delayed Int (Int -> SeqM e a)

fromFunction :: Int -> (Int -> SeqM e a) -> SeqM e (Array (D e) a)
fromFunction n f = return $ Delayed n f

instance IsArray G a where
  data Array G a = Vector a

class Map e e' where
  mapS :: (a -> SeqM e b) -> Array r a -> SeqM e' (Array (D e') b)

-- map :: (a -> SeqM Seq b)    -> SeqM Seq (Array a)    -> SeqM Seq (Array b)
-- map :: (a -> SeqM Seq b)    -> SeqM Seq (Array a)    -> SeqM SeqPar (Array b)
-- map :: (a -> SeqM SeqPar b) -> SeqM Seq (Array a)    -> SeqM SeqPar (Array b)
-- map :: (a -> SeqM Seq b)    -> SeqM SeqPar (Array a) -> SeqM SeqPar (Array b)
-- map :: (a -> SeqM SeqPar b) -> SeqM SeqPar (Array a) -> SeqM SeqPar (Array b)

-- --------
-- map :: ( -> e ) -> e' -> e''
-- got to have e'' >= e and e'' >= e'
--
-- do we need e >= e' ?? -- Doesn't seem so
--
-- (where relation (•>=•) is defined by: SeqPar >= SeqPar and SeqPar >= Seq)


-- Problems:
--
-- * We have nothing to force SeqPar in the absence of Sequential.
-- * Even in the case of Sequential, it seems

-- | Examples

-- ex1 :: Int -> Double -> SeqM e (Array D Double)
-- ex1 n x = fromFunction n (\i -> (fromIntegral i) * x)
