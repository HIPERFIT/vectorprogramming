{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module ToySequential where

-- Here we experiment with a typed, first order representation.

import Control.Applicative hiding (Const)
import Control.Monad.State
import Text.PrettyPrint
import Data.Typeable
import qualified Data.Bits as Bits

type Array a = [a]

data Exp :: * -> * where
  Const :: String -> a -> Exp a
  Var   :: String -> Exp a -- Only for printing
  Let   :: String -> Exp a -> (Exp a -> Exp b) -> Exp b
  Lam   :: String -> (Exp a -> Exp b) -> Exp (a -> b)
  App   :: Exp (a -> b) -> Exp a -> Exp b

  If :: Exp Bool -> Exp a -> Exp a -> Exp a

  -- Const, but for potentially parallel higher-order constructs.
  SeqParPrim :: ExecMode -> String -> a -> Exp a

  Sequential :: Exp a -> Exp a

data ExecMode = SeqPar | Par | Seq | Unassigned

interpret :: Exp a -> a
interpret (Const _ x)   = x
interpret (Lam v f)   = \x -> interpret $ f (Const v x)
interpret (Var s)     = error "free variable in expression!"
interpret (App f a)   = interpret f (interpret a)
interpret (Let _ a f) = interpret (f a)

ppExp :: Exp a -> Doc
ppExp (Const s a)   = text $ s
ppExp l@(Lam x f)   = let (vs,b) = nestLam [] l
                       in if vs == [] then b
                        else
                         text "λ" <> hsep (map text $ reverse vs) <+> text "→" <+> b
  where
  -- compress nested lambdas, and produce eta-short form if applicable
  nestLam :: [String] -> Exp a -> ([String],Doc)
  nestLam vs (Lam x f) = nestLam (x:vs) (f $ Var x)
  nestLam vs e = etaShort vs e

  etaShort :: [String] -> Exp c -> ([String], Doc)
  etaShort (v:vs) (App f (Var v')) | v == v' = etaShort vs f
  etaShort vs x@(Lam _ _) = (vs, ppExp x)
  etaShort vs x@(Const _ _) = (vs, ppExp x)
  etaShort vs x@(SeqParPrim _ _ _) = (vs, ppExp x)
  etaShort vs x = (vs, ppExp x)

ppExp (Var s)     = text s
ppExp (App (App f a) b) | isInfixOp f = parenP (precAppR a) (ppExp a) <+> ppExp f <+> parenP (precAppR b) (ppExp b)
ppExp (App f a)   = (parenP (precAppL f) $ ppExp f) <+>(parenP (precAppR a) $ ppExp a)
ppExp (Let x a f) = text "let" <+> text x <+> text "=" <+> ppExp a $$ text "in" <+> ppExp (f $ Var x)
ppExp (SeqParPrim m n _ ) = text n <> ppExec m
ppExp (If c t e)   = text "if" <+> ppExp c <+> text "then" <+> ppExp t <+> text "else" <+> ppExp e
ppExp (Sequential e) = text "sequential" <+> ppExp e

ppExec :: ExecMode -> Doc
ppExec Par = text "P"
ppExec Seq = text "S"
ppExec SeqPar = text "SP"
ppExec Unassigned = text "U"

-- Stuff with lower precedence than function application. Require parenthesis if lower
precAppL (Lam _ _)  = True
precAppL (If _ _ _) = True
precAppL  _         = False
precAppR (App _ _)  = True
precAppR (Lam _ _)  = True
precAppR (If _ _ _) = True
precAppR _          = False

parenP p = if p then parens else id

ppPrint :: Exp a -> IO ()
ppPrint = putStrLn . render . ppExp

isInfixOp :: Exp a -> Bool
isInfixOp (Const (n:_) _) = n `elem` specChars
isInfixOp (SeqParPrim _ (n:_) _) = n `elem` specChars
isInfixOp _ = False
specChars = ".^!<>=&/*?-:@#$|~+%"

-- Constructors, embedding syntax

const' :: Show a => a -> Exp a
const' x = Const (show x) x

id' :: Exp (a -> a)
id' = Const "id" id

lam :: String -> (Exp a -> Exp b) -> Exp (a -> b)
lam v f = Lam v f

infixl 0 $.
($.) :: Exp (a -> b) -> Exp a -> Exp b
f $. x = App f x

let' :: String -> Exp a -> (Exp a -> Exp b) -> Exp b
let' v x f = Let v x f

map' f xs = (SeqParPrim Unassigned "map" map)
        $. f $. xs

fold' f b a = (SeqParPrim Unassigned "foldl" foldl)
              $. f $. b $. a

unfold' n f s = (SeqParPrim Unassigned "unfold" unfold)
          $. n $. f $. s

zipWith' f x y= (SeqParPrim Unassigned "zipWith" zipWith)
            $. f $. x $. y

unfold 0 _ _ = []
unfold n f s =
  let (r,s') = f s
  in r : unfold (n-1) f s'

generate n f = (SeqParPrim Unassigned "generate" (\n f -> unfold n (\ix -> (f ix, succ ix)) 0))
                $. n $. f

sequential = Sequential

ifThenElse :: Exp Bool -> Exp a -> Exp a -> Exp a
ifThenElse c t e = If c t e
if' = ifThenElse

instance Num a => Num (Exp a) where
  (+) a b = (Const "+" (+)) $. a $. b
  (*) a b = (Const "*" (*)) $. a $. b
  (-) a b = (Const "-" (-)) $. a $. b
  negate a = (Const "negate" negate) $. a
  abs a = (Const "abs" abs) $. a
  signum a = (Const "signum" signum) $. a
  --fromInteger a = (Const "fromInteger" fromInteger) $. Const (show a) a
  fromInteger a = Const (show a) (fromInteger a)

instance Fractional a => Fractional (Exp a) where
  (/) a b = (Const "/" (/)) $. a $. b
  recip x = (Const "recip" recip) $. x
  fromRational x = (Const "fromRational" fromRational) $. (Const (show x) x)

{-
class (Real a, Enum a) => Integral a where
  quot :: a -> a -> a
  rem :: a -> a -> a
  div :: a -> a -> a
  mod :: a -> a -> a
  quotRem :: a -> a -> (a, a)
  divMod :: a -> a -> (a, a)
  toInteger :: a -> Integer
-}
-- Examples

toDouble :: Exp (Int -> Double)
toDouble = Const "toDouble" (fromRational . fromIntegral)

ex1 :: Exp (Int -> Array Int)
ex1 = lam "x" (\x -> generate x id')

bitVec :: Exp Int -> Exp (Array Int)
bitVec x = (lam "x" $ \x -> generate 32 (lam "ix" $ \ix -> x .&. (shiftL 1 ix)))
           $. x

(.&.) :: Exp Int -> Exp Int -> Exp Int
x .&. y = Const ".&." (Bits..&.) $. x $. y

shiftL :: Exp Int -> Exp Int -> Exp Int
shiftL x l = (Const "shiftL" Bits.shiftL) $. x $. l


xor :: Exp (Int -> Int -> Int)
xor = Const "xor" Bits.xor

xor' :: Exp Int -> Exp Int -> Exp Int
xor' x y  = xor $. x $. y

(*.) :: Num a => Exp (a -> a -> a)
(*.) = lam "x" $ \x -> lam "y" $ \y -> x *y

sobolInd :: Exp (Array Int -> Int -> Double)
sobolInd = lam "dirV" $ \dirV ->
           lam "ix" $ \ix -> normalise $. (fold' xor 0
                                          (zipWith' (*.) dirV (bitVec ix)))
  where
    normalise :: Exp (Int -> Double)
    -- normalise = lam "x" $ \x -> (toDouble $. x) / (toDouble $. shiftL 1 32)
    normalise = Const "normalise" (\x -> fromIntegral x / fromIntegral (2^32))


type PA a = State ExecMode a
-- Remove 'sequential', and assign execution modes
parAlloc :: Exp a -> PA (Exp a)
parAlloc a@(Const _ _) = put Seq >> return a
parAlloc a@(Var _)     = return a
parAlloc a@(Lam v f)   = parAlloc (f $ Var v) >>= return . (Lam v) . (abstract v)
parAlloc l@(App f v)   = App <$> parAlloc f <*> parAlloc v
parAlloc a@(Let n x f) = parAlloc (f $ Var n) >>= \a' -> return (Let n x (abstract n a'))
parAlloc l@(If c t e)  = If <$> parAlloc c <*> parAlloc t <*> parAlloc e
-- implement changing logic
parAlloc a@(SeqParPrim e n f) = return a
parAlloc a@(Sequential e)     = put Seq >> parAlloc e


abstract :: String -> Exp b -> Exp a -> Exp b
abstract = undefined
