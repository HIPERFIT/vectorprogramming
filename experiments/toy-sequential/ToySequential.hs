-- vim: set foldmethod=marker :
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module ToySequential where

-- Here we experiment with a typed, first order representation.

-- {{{ Import statements

import Control.Applicative hiding (Const)
import Control.Monad.State
import Text.PrettyPrint
import Data.Maybe
import Data.Typeable
import qualified Data.Bits as Bits

-- }}}

-- {{{ datatype definitions

type Array a = [a]

data Exp :: * -> * where
  Const :: String -> a -> Exp a
  Var   :: Typeable a => String -> Exp a -- mostly for printing
  Let   :: (Typeable a, Typeable b) => String -> Exp a -> (Exp a -> Exp b) -> Exp b
  Lam   :: (Typeable a, Typeable b) => String -> (Exp a -> Exp b) -> Exp (a -> b)
  App   :: Exp (a -> b) -> Exp a -> Exp b

  If :: Exp Bool -> Exp a -> Exp a -> Exp a

  -- Const, but for potentially parallel higher-order constructs.
  SeqParPrim :: ExecMode -> String -> a -> Exp a

  Sequential :: Exp a -> Exp a
  deriving (Typeable)


data ExecMode = SeqPar | Par | Seq | Unassigned
 deriving (Show, Eq)

succEM Unassigned = Seq
succEM Seq = Par
succEM Par = SeqPar
succEM SeqPar = SeqPar

prevEM Unassigned = Unassigned
prevEM Seq = Unassigned
prevEM Par = Seq
prevEM SeqPar = Par

instance Ord ExecMode where
  compare Unassigned Unassigned = EQ
  compare Unassigned _ = LT
  compare Seq Unassigned = GT
  compare Seq Seq = EQ
  compare Seq Par = LT
  compare Seq SeqPar = LT
  compare Par Unassigned = GT
  compare Par Seq = GT
  compare Par Par = EQ
  compare Par SeqPar = LT
  compare SeqPar Unassigned = GT
  compare SeqPar Seq = GT
  compare SeqPar Par = GT
  compare SeqPar SeqPar = EQ

-- }}}

-- {{{ Interpreter

-- ( '{{' '}}' braces are just vim specific stuff for outlining.)

interpret :: Exp a -> a
interpret (Const _ x)   = x
interpret (Lam v f)   = \x -> interpret $ f (Const v x)
interpret (Var s)     = error "free variable in expression!"
interpret (App f a)   = interpret f (interpret a)
interpret (Let _ a f) = interpret (f a)
interpret (If c t e) = if interpret c then interpret t else interpret e
interpret (SeqParPrim _ _ e) = e
interpret (Sequential e) = interpret e

-- }}}

-- {{{ Pretty printer
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
ppExp (App f a) | isInfixOp f = (parenP (precAppR a) $ ppExp a) <+> (ppExp f)
ppExp (App f a) = (parenP (precAppL f) $ ppExp f) <+>(parenP (precAppR a) $ ppExp a)
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
precAppL (Sequential _) = True
precAppL (Lam _ _)  = True
precAppL (If _ _ _) = True
precAppL  _         = False
precAppR (App _ _)  = True
precAppR (Sequential _) = True
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

-- }}}

-- {{{ Constructors, embedding syntax

const' :: Show a => a -> Exp a
const' x = Const (show x) x

id' :: Exp (a -> a)
id' = Const "id" id

lam :: (Typeable a, Typeable b) => String -> (Exp a -> Exp b) -> Exp (a -> b)
lam v f = Lam v f

infixl 0 $.
($.) :: Exp (a -> b) -> Exp a -> Exp b
f $. x = App f x

let' :: (Typeable a, Typeable b) => String -> Exp a -> (Exp a -> Exp b) -> Exp b
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

toDouble :: Exp (Int -> Double)
toDouble = Const "toDouble" (fromRational . fromIntegral)

-- }}}

-- {{{ Allocation of parallel resources

-- | Allocation of parallelism proceeds in 4 stages:
--
-- 1. 'float parallelism inwards'. The innermost potentially parallel ops are
-- identified, an `sequential` placed on their arguments to make them parallel.
-- This we may do by lambda-elaborating: map == \f x -> map (sequential f) x To
-- simplify, we only mark the lexically (?) innermost. For example in `(\f ->
-- map f [..]) (map g)` we would have `map g` be the innermost map, but instead
-- we mark `map f` innermost, and thus force `f` to be sequential, and through
-- bubbling also force `map g`. this way we avoid having to spill constraints.
--
-- 2. 'top-down bubbling of sequential'. Appliction with `sequential` on rhs
-- bubble into the corresponding function-expression.
--
-- 3. 'bottom-up bubbling of sequential'. Variables enclosed by `sequential`
-- makes `sequential` bubble up to the application binding the affected
-- variables. If the top-level type is a function type, we may have a surplus
-- constraints limiting the possible uses of the expression. For now, lets just
-- observe it and output it (by expanding to eta-long and applying sequential
-- where constrained). (We should probably argue that step 2-3 is a fix point
-- of `sequential-bubbling`)
--
-- 4. 'Allocate execution modes'. All execution modes of SeqParPrim's are
-- according to their application-`sequential` context. This is readily
-- available as `sequential` constraint bubbling has been resolved.

parAlloc :: Exp a -> Exp a
parAlloc = markExec . botUpSeq . topDownSeq . initialSeq

initialSeq :: Exp a -> Exp a
initialSeq = fst . initialSeq'

-- We Assume absence of higher-order functions except for SeqParPrim
initialSeq' :: Exp a -> (Exp a, Bool) -- Return True if we inserted Sequential
initialSeq' e@(Const _ _) = (e,False)
initialSeq' e@(Var v') = (e,False)
initialSeq' (Lam v' f) =
  let
    (e,s) = initialSeq' (f $ Var v')
  in (Lam v' $ abstract v' e, s)
initialSeq' (Let n x f) =
  let
    (f',fs) = initialSeq' (f $ Var n)
    (x',xs) = initialSeq' x
  in (Let n x' $ abstract n f', fs || xs)

-- Because of absence of higher order functions, all function-arguments to a
-- SeqParPrim must not be variables (note: There is nothing prohibiting : `\f
-- -> map ((\x->x) f)` or similar from passing this check. you are on your own
-- if you divulge into this)
-- initialSeq' (App (SeqParPrim _ n _) (Var v)) =
--   error $ "higher order argument " ++ v ++ " given to " ++ n

initialSeq' (App (App spp@(SeqParPrim _ "generate" _) x) f) =
  let
    (f', fs) = initialSeq' f
    (x', _) = initialSeq' x
  in if not fs then
       (App (App spp x') (Sequential f'), True)
     else
       (App (App spp x') f', True)

initialSeq' (App (App spp@(SeqParPrim _ "map" _) f) x) =
  let
    (f', fs) = initialSeq' f
    (x', xs) = initialSeq' x
  in if not fs && not xs then
       (App (App spp (Sequential f')) x', True)
     else
       (App (App spp f') x', True)

initialSeq' (App spp@(SeqParPrim _ "map" _) f) =
  let
    (f', fs) = initialSeq' f
  in if not fs then
       (App spp (Sequential f'), True)
     else
       (App spp f', True)

-- There should also be some more specific cases for zipWith dealing with
-- exec-mode of `xs` and `ys` in `zipWith f xs ys`.  how should (or can) we
-- compile `zipWith f (mapSP ..) (mapP ..)` (not so important at this stage, as
-- we just conclude that `f` should not be sequential.
initialSeq' (App spp@(SeqParPrim _ "zipWith" _) f) =
  let
    (f', fs) = initialSeq' f
  in if not fs then
       (App spp (Sequential f'), True)
     else
       (App spp f', True)

-- we can't fuse : `foldl f a (mapP ..)`
-- but `foldl f a (mapSP ..)` is alright though.
-- however, it seems that `initialSeq` is unable to distinguish :-(
initialSeq' (App (App (App spp@(SeqParPrim _ "foldl" _) f) a) xs) =
  let
    (f',  fs)  = initialSeq' f
    -- If 'a' is an array (and 'f' thereby an array function), 'f' and 'a' might undergo fusion.
    -- But this we don't account for here though. It also depends on whether 'f' needs to index arbitrarily.
    (a',  as)  = initialSeq' a
  in if fs then
    (App (App (App spp f') a') (Sequential xs), True)
     else
       -- 'f' is all sequential
    let
    (xs', xss) = initialSeq' xs
    in (App (App (App spp f') a') xs', as||xss)

initialSeq' (SeqParPrim _ n _) =
  error $ "unapplied occurence of SeqParPrim " ++ n

-- default case for Spps
-- initialSeq' (App spp@(SeqParPrim _ _ _) l) =
--   let (l', s) = initialSeq' l
--   in if not s then
--        (App spp (Sequential l), True)
--      else
--        (App spp l, True)

initialSeq' (App f a) =
  let
    (f',fs) = initialSeq' f
    (a',as) = initialSeq' a
  in (App f' a', fs || as)

initialSeq' (If c t e) =
  let
    (c',cs) = initialSeq' c
    (t',ts) = initialSeq' t
    (e',es) = initialSeq' e
  in (If c' t' e', cs || ts || es)

initialSeq' e@(Sequential _) = (e, True)

topDownSeq :: Exp a -> Exp a
topDownSeq = id -- undefined

botUpSeq :: Exp a -> Exp a
botUpSeq = id -- undefined

markExec :: Exp a -> Exp a
markExec ev@(Lam v f)  = Lam v (abstract v (markExec $ f $ Var v))
markExec ev@(Let n y f) = Let n (markExec y) (abstract n (markExec $ f $ Var n))
-- lots and lots of specific cases:
markExec (App (App (SeqParPrim _ "map" spp) f) xs) =
  let mode = case (f, xs) of
              (Sequential _, Sequential _) -> Seq
              (Sequential _,  _) -> Par
              (_,  Sequential _) -> SeqPar
              (_, _) -> SeqPar
  in App (App (SeqParPrim mode "map" spp) (markExec f)) (markExec xs)

{- Really should not occur, bubbling should make sure to be eta-long
markExec (App (SeqParPrim _ "map" spp) f) =
  let mode = case f of
    Sequential _ -> Par
    _  -> SeqPar
  in App (SeqParPrim mode "map" spp) (markExec f)
-}

markExec (App (App (SeqParPrim _ "generate" spp) x) f) =
  let mode = case (f, x) of
              (Sequential _, Sequential _) -> Par
              (Sequential _,  _) -> Par
              (_,  Sequential _) -> SeqPar
              (_, _) -> SeqPar
  in App (App (SeqParPrim mode "generate" spp) (markExec x)) (markExec f)

markExec (App (App (App (SeqParPrim _ "zipWith" spp) f) xs) ys) =
  let mode = case (f, xs, ys) of
              (Sequential _, Sequential _,            _) -> Seq
              (Sequential _,            _, Sequential _) -> Seq
              (           _, Sequential _,            _) -> SeqPar
              (           _,            _, Sequential _) -> SeqPar
              (           _,            _,            _) -> SeqPar
  in App (App (App (SeqParPrim mode "zipWith" spp) (markExec f)) (markExec xs)) (markExec ys)

-- markExec (App (App (App spp@(SeqParPrim _ "unfoldl" _) f) a) xs) = undefined
markExec (App (App (App (SeqParPrim _ "foldl" spp) f) a) xs) =
  let mode = case (f, xs) of
              (Sequential _,            _) -> Seq
              (           _, Sequential _) -> SeqPar
              (           _,            _) -> SeqPar
  in App (App (App (SeqParPrim mode "foldl" spp) (markExec f)) (markExec a)) (markExec xs)

markExec ev@(App f a)   = App (markExec f) (markExec a)
markExec ev@(If c t e)  = If (markExec c) (markExec t) (markExec e)
markExec ev@(Const _ _) = ev
markExec ev@(Var _)     = ev
markExec ev@(Sequential e) = allSeq e
  where
    -- And now it would be nice to have some generics..
    allSeq :: Exp a -> Exp a
    allSeq (SeqParPrim _ n f) = SeqParPrim Seq n f
    allSeq (Lam v f)   = Lam v (abstract v (allSeq $ f $ Var v))
    allSeq (Let n y f) = Let n (allSeq y) (abstract n (allSeq $ f $ Var n))
    allSeq (App f a)   = App (allSeq f) (allSeq a)
    allSeq (If c t e)  = If (allSeq c) (allSeq t) (allSeq e)
    allSeq ev@(Const _ _) = ev
    allSeq ev@(Var _)     = ev
    allSeq (Sequential e) = e

-- Abstract a variable from a first order representation back to HOAS
-- So, HOAS didn't save us from implementing capture-avoiding substitution
abstract :: Typeable a => String -> Exp b -> (Exp a -> Exp b)
abstract v ev@(Var v') | v == v' = \x -> maybe ev id (cast x)
abstract v ev@(Lam v' _) | v == v' = \x -> ev
abstract v ev@(Lam v' f) | v /= v' = \x -> Lam v' (\x' -> abstract v' (abstract v (f $ Var v') x) x')
abstract v ev@(Let n y f) | v == n = \x -> ev
abstract v ev@(Let n y f) | v /= n = \x -> Let n (abstract v y x) (\x' -> abstract n (abstract v (f $ Var n) x) x')
abstract v ev@(App f a) = \x -> App (abstract v f x) (abstract v a x)
abstract v ev@(If c t e) = \x -> If (abstract v c x) (abstract v t x) (abstract v e x)
abstract v ev@(Sequential e) = \x -> Sequential (abstract v e x)
-- cases for SeqParPrim and Const
abstract v ev = \x -> ev

-- }}}

-- {{{ Examples
ex1 :: Exp (Int -> Array Int)
ex1 = lam "x" (\x -> generate x id')

length' = (Const "length" length)

ex2 :: Exp (Array (Array Int) -> Array Int)
ex2 = lam "xs" $ \xs -> map' (sequential length') xs

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

(*.) :: Typeable a => Num a => Exp (a -> a -> a)
(*.) = lam "x" $ \x -> lam "y" $ \y -> x *y

sobolInd :: Exp (Array Int -> Int -> Double)
sobolInd = lam "dirV" $ \dirV ->
           lam "ix" $ \ix -> normalise $. (fold' xor 0
                                          (zipWith' (*.) dirV (bitVec ix)))
  where
    normalise :: Exp (Int -> Double)
    -- normalise = lam "x" $ \x -> (toDouble $. x) / (toDouble $. shiftL 1 32)
    normalise = Const "normalise" (\x -> fromIntegral x / fromIntegral (2^32))

-- }}}
