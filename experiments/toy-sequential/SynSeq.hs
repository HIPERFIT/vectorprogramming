{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
module SynSeq where

-- Here we try to use Syntactic

import Data.Typeable
import Language.Syntactic
import Text.PrettyPrint as PP

type Array a = [a]

data Base :: * -> * where
  Const :: String -> a -> Base (Full a)
  Var   :: Typeable a => String -> Base (Full a)
  Lam   :: Typeable a => String -> Base (b :-> Full (a -> b))
  App   :: Base ((a -> b) :-> a :-> Full b)

data NestOps :: * -> * where
  Map :: NestOps ((a :-> b) :-> Array a :-> Full (Array b))
  Sequential :: NestOps (a :-> Full a)

data FlatOps :: * -> * where
  MapP :: FlatOps ((a :-> b) :-> Array a :-> Full (Array b))
  MapS :: FlatOps ((a :-> b) :-> Array a :-> Full (Array b))

type Base' a = ASTF Base a
type NestOps' a = ASTF NestOps a

type NestLang a = ASTF (Base :+: NestOps) a
type FlatLang a = ASTF (Base :+: FlatOps) a

-- Wooo. Syntactic rocks my world <3
const' :: (Base :<: lang, Show a) => a -> ASTF lang a
const' x = Sym . inj $ Const (show x) x

var' :: (Base :<: lang, Typeable a) => String -> ASTF lang a
var' = Sym . inj . Var

lam' :: (Base :<: lang, Typeable a) => String -> (ASTF lang a -> ASTF lang b) -> ASTF lang (a->b)
lam' v f = (Sym . inj $ (Lam v)) :$ (f $ var' v)

app' :: (Base :<: lang) => ASTF lang (a-> b) -> ASTF lang a -> ASTF lang b
app' f x = (Sym . inj $ App) :$ f :$ x

ex1 :: ASTF Base Int
ex1 = (lam' "x" (\x -> x)) `app'` (const' 5)

instance Eval (AST Base) where
  evaluate (Sym (Const _ x)) = x
  evaluate (Sym (Var v)) = error "Encountered free variable!"
  evaluate l@(Sym (Lam v) :$ body) = \x -> evaluate (subst v body (Sym $ Const (error "no string while evaluating") x))
    where
      subst :: Typeable x => String -> ASTF Base b -> ASTF Base x -> ASTF Base b
      subst v body x = match (subst' v x) body

      subst' = undefined

  evaluate (Sym App :$ f :$ a) = evaluate f $ evaluate a

class PP lang a where
  pp :: ASTF lang a -> Doc

instance PP Base a where
  pp (Sym (Const s _)) = text s
  pp (Sym (Var v)) = text v
  pp (Sym (Lam v) :$ body) = text "λ" <> text v <+> text "→" <+> pp body
  pp (Sym App :$ f :$ x) = prec (isLam f) (pp f) <+> prec (isLam x) (pp x)

prec :: Bool -> Doc -> Doc
prec p = if p then parens else id

isLam :: (Base :<: lang) => ASTF lang a -> Bool
isLam = simpleMatch go
  where
    go (prj -> Just (Lam _)) _ = True
    go _       _ = False

ppPrint :: ASTF Base a -> IO ()
ppPrint ast = putStrLn $ PP.render $ pp ast

parAlloc :: NestLang a -> FlatLang a
parAlloc = undefined
