{-# OPTIONS -fwarn-incomplete-patterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}

module HOAS where

import Deconstrain

data PrimOps :: * -> * where
  PlusOp        :: Num a => PrimOps (a -> a -> a)
  MinusOp       :: Num a => PrimOps (a -> a -> a)
  MultOp        :: Num a => PrimOps (a -> a -> a)
  EqOp          :: Eq a  => PrimOps (a -> a -> Bool)
  NotOp         ::          PrimOps (Bool -> Bool)
  AbsOp         :: Num a => PrimOps (a -> a)
  SignOp        :: Num a => PrimOps (a -> a)
  FromIntegerOp :: Num a => PrimOps (Integer -> a)

data NestOps :: * -> * where
  Map           :: NestOps ((a -> b) -> [a] -> [b])
  Foldl         :: NestOps ((a -> b -> a) -> a -> [b] -> a)
  Reduce        :: NestOps ((a -> a -> a) -> a -> [a] -> a)
  Unfold        :: NestOps (Int -> (Int -> a -> a) -> a -> [a])
  Sequential    :: NestOps (a -> a)

data FlatOps :: * -> * where
  MapP          :: FlatOps ((a -> b) -> [a] -> [b])
  MapB          :: FlatOps ((a -> b) -> [a] -> [b])
  MapSeq        :: FlatOps ((a -> b) -> [a] -> [b])
  MapSeqPar     :: FlatOps ((a -> b) -> [a] -> [b])
  FoldlSeqPar   :: FlatOps ((a -> b -> a) -> a -> [b] -> a)
  FoldlSeq      :: FlatOps ((a -> b -> a) -> a -> [b] -> a)
  ReduceP       :: FlatOps ((a -> a -> a) -> a -> [a] -> a)
  ReduceB       :: FlatOps ((a -> a -> a) -> a -> [a] -> a)
  UnfoldSeq     :: FlatOps (Int -> (Int -> a -> a) -> a -> [a])
  UnfoldSeqPar  :: FlatOps (Int -> (Int -> a -> a) -> a -> [a])
  

data Exp op as a where
  ValueE  :: (Elem a as) => a -> Exp op as a
  ListE   :: [Exp op as a] -> Exp op as [a]
  PrimOpE :: PrimOps (a -> b) -> Exp op as (a -> b)
  HOOpE   :: op (a -> b) -> Exp op as (a -> b)
  CondE   :: (Elem Bool as) => Exp op as Bool -> Exp op as a 
                            -> Exp op as a -> Exp op as a
  LamE    :: (Exp op as a -> Exp op as b) -> Exp op as (a -> b)
  AppE    :: Exp op as (a -> b) -> Exp op as a -> Exp op as b
  VarE    :: String -> Exp op as a
  Hax     :: a -> Exp op as a -- This should not be here, we hope
                              -- to avoid this by conversion to DeBruijn
                              -- representation
  Tag :: Int -> Exp op as a

instance (Num a, Elem a as, Elem Integer as) => Num (Exp op as a) where
  x + y         = PrimOpE PlusOp  `AppE` x `AppE` y
  x - y         = PrimOpE MinusOp `AppE` x `AppE` y
  x * y         = PrimOpE MultOp  `AppE` x `AppE` y
  abs x         = PrimOpE AbsOp   `AppE` x
  signum x      = PrimOpE SignOp  `AppE` x
  fromInteger x = PrimOpE FromIntegerOp `AppE` ValueE x

ifThenElse b x1 x2 = CondE b x1 x2
notE x = PrimOpE NotOp `AppE` x
x ==* y = PrimOpE EqOp `AppE` x `AppE` y
x /=* y = notE $ PrimOpE EqOp `AppE` x `AppE` y
