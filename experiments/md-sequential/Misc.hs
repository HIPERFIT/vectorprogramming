{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Misc where

import Deconstrain
import HOAS

import Data.List (intersperse)
import qualified Control.Monad.State as MS (State, get, put, evalState) 
import Control.Applicative
import Text.Printf (printf)
import Text.Parsec


-- Pretty print
instance forall as a. All Show as =>  Show (Exp NestOps as a) where
  show x = MS.evalState (showExp x) 0

getFreshVar :: Num a => MS.State a a
getFreshVar = do
  freshVar <- MS.get
  MS.put (freshVar + 1)
  return freshVar

showExp :: forall as a. All Show as => Exp NestOps as a -> MS.State Int String
showExp (CondE e1 e2 e3) = printf "if %s then %s else %s" <$> showExp e1 <*> showExp e2 <*> showExp e3
showExp (AppE e1 e2)     = printf "(%s %s)" <$> showExp e1 <*> showExp e2
showExp (VarE str)       = return str
showExp (PrimOpE op)     = return $ showOp op
showExp (HOOpE op)       = return $ showHOOp op
showExp (Hax e)          = error "nooooo"
showExp (ValueE e) = return $ withElem (Proxy :: Proxy as) (showIt e)
 where
  showIt :: b -> Trap Show b -> String
  showIt x Trap = show x
showExp (ListE xs) = do
  xs' <- mapM showExp xs
  return $ "[" ++ (concat $ intersperse "," xs') ++ "]"
showExp (LamE f) = do
  freshVar <- getFreshVar
  let varString = "x" ++ show freshVar
  body <- showExp (f (VarE varString))
  return $ printf "(\\%s -> %s) " varString body

showOp :: PrimOps (a -> b) -> String
showOp PlusOp        = "(+)"
showOp MinusOp       = "(-)"
showOp MultOp        = "(*)"
showOp AbsOp         = "abs"
showOp SignOp        = "signum"
showOp FromIntegerOp = "fromInteger"
showOp EqOp          = "(==)"
showOp NotOp         = "not"

showHOOp Map           = "map"
showHOOp Foldl         = "foldl"
showHOOp Reduce        = "reduce"
showHOOp Unfold        = "unfold"
showHOOp Sequential    = "sequential"

-- Interpret
evalExp :: Exp NestOps as a -> a
evalExp (ValueE e)       = e
evalExp (Hax e)       = e
evalExp (ListE es)       = map evalExp es
evalExp (CondE e1 e2 e3) = if evalExp e1 then evalExp e2 else evalExp e3
evalExp (LamE f)         = evalExp . f . Hax
evalExp (AppE e1 e2)     = evalExp e1 $ evalExp e2
evalExp (PrimOpE op)     = evalOp op
evalExp (HOOpE op)       = evalHOOp op
evalExp (VarE x)         = error "VarE is not to be used"

evalOp :: PrimOps (a -> b) -> a -> b
evalOp PlusOp        = (+)
evalOp MinusOp       = (-)
evalOp MultOp        = (*)
evalOp AbsOp         = abs
evalOp SignOp        = signum
evalOp FromIntegerOp = fromInteger
evalOp EqOp          = (==)
evalOp NotOp         = not

evalHOOp :: NestOps (a -> b) -> a -> b
evalHOOp Map           = map
evalHOOp Foldl         = foldl
evalHOOp Reduce        = foldl
evalHOOp Unfold        = unfold
evalHOOp Sequential    = id

unfold :: Int -> (Int -> a -> a) -> a -> [a]
unfold 0 f x = []
unfold n f x = x' : unfold (n-1) f x'
  where x' = f n x

-- Parse
parser :: ParseExp hoops as a => String -> Either ParseError (Exp hoops as a)
parser = parse parseit ""

class ParseExp hoops as a where
  parseit :: Parsec String () (Exp hoops as a)

instance (Elem a as, Read a) => ParseExp hoops as a where
  parseit = ValueE . read <$> many1 digit
