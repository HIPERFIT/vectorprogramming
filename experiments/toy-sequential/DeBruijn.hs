{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module DeBruijn where

import Text.PrettyPrint

-- Here we give λ-calculus with typed de-bruijn numbers

data Exp :: * -> * where
  IntE :: Int -> Exp Int
  Var :: String -> Exp a
  Lam :: String -> Exp b -> Exp (a,(b,()))
  App :: Exp d -> Exp ((DbHead d),t:) -> Exp (DbTail d)


type family DbHead d
type family DbTail d

type instance DbHead (h,t) = h
type instance DbTail (h,t) = t

ppExp :: Exp a -> Doc
ppExp (Var x) = text x
ppExp (Lam x f) = text ("λ" ++ x) <+> text "→" <+> ppExp f
ppExp (App f x) = parens (ppExp f) <+> parens (ppExp x)

interpret :: Exp a -> a
interpret = undefined

-- wants:

-- ex1 = Exp (
-- ex1 = App (Lam "x" (Var "x")) (Lam "x" (Var "x"))
--
-- ex2 : Exp a
-- ex2 = App (Lam "x" (Var "x")) (Var "a")
