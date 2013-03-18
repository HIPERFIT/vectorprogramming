{-# OPTIONS -Wall #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
--import Deconstrain
--import HOAS hiding (Exp)

-- data PrimOps where
--   PlusOp        :: PrimOps
--   MinusOp       :: PrimOps
--   MultOp        :: PrimOps
--   EqOp          :: PrimOps
--   NotOp         :: PrimOps
--   AbsOp         :: PrimOps
--   SignOp        :: PrimOps
--   FromIntegerOp :: PrimOps

data NestOps where
  Map           :: NestOps
  Foldl         :: NestOps
  -- Reduce        :: NestOps
  -- Unfold        :: NestOps
  Sequential    :: NestOps

data FlatOps where
  MapP          :: FlatOps
  MapB          :: FlatOps
  MapSeq        :: FlatOps
  MapSeqPar     :: FlatOps
  FoldlSeqPar   :: FlatOps
  FoldlSeq      :: FlatOps
  -- ReduceP       :: FlatOps
  -- ReduceB       :: FlatOps
  -- UnfoldSeq     :: FlatOps
  -- UnfoldSeqPar  :: FlatOps
  
data Val = VInt Int | VBool Bool

data NExp where
  NValue      :: Val -> NExp
  NVar        :: String -> NExp
  NLam        :: String -> NExp -> NExp
  NApp        :: NExp -> NExp -> NExp

  NList       :: [NExp]-> NExp
  NSequential :: NExp -> NExp
  NMap        :: NExp -> NExp -> NExp
  NFoldl      :: NExp -> NExp -> NExp -> NExp

-- "PotPar" is potentially parallel
data S = Seq | PotPar

data PExp where
  PValue      :: Val -> PExp
  PVar        :: String -> PExp
  PLam        :: String -> PExp -> PExp
  PApp        :: PExp -> PExp -> PExp

  PList       :: [PExp]-> PExp
  PMap        :: S -> PExp -> PExp -> PExp
  PFoldl      :: S -> PExp -> PExp -> PExp -> PExp

allocSeq :: NExp -> Reader S PExp
allocSeq (NValue v)        = return $ PValue v
allocSeq (NVar str)        = return $ PVar str
allocSeq (NLam str e)      = PLam str <$> allocSeq e
allocSeq (NApp e1 e2)      = PApp <$> allocSeq e1 <*> allocSeq e2
allocSeq (NList es)        = PList <$> mapM allocSeq es
allocSeq (NSequential e1)  = local (const Seq) $ allocSeq e1
allocSeq (NMap e1 e2)      = PMap <$> ask <*> allocSeq e1 <*> allocSeq e2
allocSeq (NFoldl e1 e2 e3) = PFoldl <$> ask <*> allocSeq e1 <*> allocSeq e2 <*> allocSeq e3

data FExp where
  FValue       :: Val -> FExp
  FVar         :: String -> FExp
  FLam         :: String -> FExp -> FExp
  FApp         :: FExp -> FExp -> FExp

  FList        :: [FExp]-> FExp
  FMapP        :: FExp -> FExp -> FExp
  FMapB        :: FExp -> FExp -> FExp
  FMapSeq      :: FExp -> FExp -> FExp
  FMapSeqPar   :: FExp -> FExp -> FExp
  FFoldlSeq    :: FExp -> FExp -> FExp -> FExp
  FFoldlSeqPar :: FExp -> FExp -> FExp -> FExp


data S2 = SeqS | PotParP | PotParB

allocPar :: PExp -> State S2 FExp
allocPar (PValue v)               = return $ FValue v
allocPar (PVar str)               = return $ FVar str
allocPar (PLam str e)             = FLam str <$> allocPar e
allocPar (PApp e1 e2)             = FApp <$> allocPar e1 <*> allocPar e2
allocPar (PList es)               = FList <$> mapM allocPar es
allocPar (PMap PotPar e1 e2)      = do
  e1' <- allocPar e1
  s1 <- get
  e2' <- allocPar e2
  s2 <- get
  case (s1, s2) of
    (SeqS, SeqS)       -> return $ FMapB e1' e2'
    (PotParP, PotParP) -> return $ FMapSeqPar e1' e2'
    (PotParB, PotParB) -> return $ FMapP e1' e2'
    (SeqS, _)          -> error "We should make a decision here"
    (_, SeqS)          -> error "We should make a decision here"
    (PotParP, PotParB) -> return $ FMapSeqPar e1' e2'
    (PotParB, PotParP) -> return $ FMapSeqPar e1' e2'
allocPar (PMap Seq e1 e2)         = do
  ret <- FMapSeq <$> allocPar e1 <*> allocPar e2
  put SeqS
  return ret

allocPar (PFoldl Seq e1 e2 e3)    = do
  ret <- FFoldlSeq <$> allocPar e1 <*> allocPar e2 <*> allocPar e3
  put SeqS
  return ret
allocPar (PFoldl PotPar e1 e2 e3) = do
  e1' <- allocPar e1
  e2' <- allocPar e2
  e3' <- allocPar e3
  return $ FFoldlSeq e1' e2' e3'
