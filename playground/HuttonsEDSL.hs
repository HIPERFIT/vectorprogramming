-- vim: set foldmethod=marker
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module HuttonsEDSL where

import Text.PrettyPrint
import Unsafe.Coerce

-- {{{ A minimalistic EDSL.
-- | Expressions. More simple at the top, more specific structures below.
data Exp a where
  EBool      :: Bool -> Exp Bool
  EInt       :: Int -> Exp Int
  EDouble    :: Double -> Exp Double

  EPlusI     :: Exp (Int, Int) -> Exp Int
  EMultI     :: Exp (Int, Int) -> Exp Int
  EEqI       :: Exp (Int, Int) -> Exp Bool
  ELessTI    :: Exp (Int, Int) -> Exp Bool

  EPlusD     :: Exp (Double, Double) -> Exp Double
  EMultD     :: Exp (Double, Double) -> Exp Double
  EEqD       :: Exp (Double, Double) -> Exp Bool
  ELessTD    :: Exp (Double, Double) -> Exp Bool

  EArrFun    :: Exp Int -> (Exp Int -> Exp a) -> Exp (Arr a)
  ET2        :: (Exp a, Exp b) -> Exp (a,b)
  ET3        :: (Exp a, Exp b, Exp c) -> Exp (a,b,c)

  -- ELam      :: (Exp a -> Exp b) -> Exp (a -> b)
  EVar       :: Name a -> Exp a
  EApp       :: Fun a b -> Exp a -> Exp b
  ELetIn     :: Name a -> Exp a -> (Exp a -> Exp b) -> Exp b

  -- For compositionality, all these might be Fun-wrapped in actual use.

  E1of2      :: Exp (a,b) -> Exp a
  E2of2      :: Exp (a,b) -> Exp b
  E1of3      :: Exp (a,b,c) -> Exp a
  E2of3      :: Exp (a,b,c) -> Exp b
  E3of3      :: Exp (a,b,c) -> Exp c


  -- Note how EMapP is the only one in curried style to reflect lack of closures.
  EIf        :: Exp Bool -> Exp a -> Exp a -> Exp a
  EMapP      :: (Exp a -> Exp b) -> Exp (Arr a) -> Exp (Arr b)
  ECombine   :: Exp (Arr Bool, Arr a, Arr a) -> Exp (Arr a)
  EConcat    :: Exp (Arr (Arr a)) -> Exp (Arr a)
  EReplicate :: Exp (Int, a) -> Exp (Arr a)
  EUnconcat  :: Exp (Arr (Arr a), Arr b) -> Exp (Arr (Arr b))
  EZip2      :: Exp (Arr a, Arr b) -> Exp (Arr (a,b))
  EZip3      :: Exp (Arr a, Arr b, Arr c) -> Exp (Arr (a,b,c))
  EUnzip2    :: Exp (Arr (a,b)) -> Exp (Arr a, Arr b)
  EIndex     :: Exp (Arr Int, Arr a) -> Exp (Arr a)

  -- Bigger, temporaray building blocks
  EReduceP   :: (Fun (a, a) a, Exp (Arr a)) -> Exp a
  EScanP     :: (Fun (a, a) a, Exp (Arr a)) -> Exp (Arr a)


-- Only allow top-level functions by having user defined functions as a
-- distinct type.
data Fun a b where Fun  :: Name a -> (Exp a -> Exp b) -> Fun a b
data Arr a   where Arr  :: [a] -> Arr a
data Name a  where Name :: String -> Name a

var :: String -> Name a
var nm = Name nm
varI :: String -> Name Int
varI nm = Name nm
varD :: String -> Name Double
varD nm = Name nm
varB :: String -> Name Bool
varB nm = Name nm

-- }}}
-- {{{ Vectorisation :
-- | Lift a name to vectorised type.
varL :: Name a -> Name (Arr a)
varL (Name n) = Name n

-- | Vectorise a function (Fun). No support for recursion yet.
vectoriseF :: Int -> Fun a b -> Fun (Arr a) (Arr b)
vectoriseF n (Fun argNm fn) = Fun (varL argNm) $ vectoriseArrow n argNm fn

vectoriseArrow :: Int -> Name a -> (Exp a -> Exp b) -> (Exp (Arr a) -> Exp (Arr b))
vectoriseArrow n argNm fn = abstract (varL argNm) (vectoriseE n $ fn $ EVar argNm)

-- | Vectorise an expression (Exp). No support for function recursion yet.
vectoriseE :: Int -> Exp a -> Exp (Arr a)
vectoriseE n exp =
  case exp of
    EBool x -> EReplicate (ET2 (EInt n, EBool x))
    EInt x  -> EReplicate (ET2 (EInt n, EInt x))
    EDouble x -> EReplicate (ET2 (EInt n, EDouble x))

    ET2 (f, s) -> EZip2 $ ET2 (vectoriseE n f, vectoriseE n s)
    EArrFun n' f -> EReplicate (ET2 (EInt n, EArrFun n' f))

    EPlusI  a -> vecOp EPlusI  a
    EMultI  a -> vecOp EMultI  a
    EEqI    a -> vecOp EEqI    a
    ELessTI a -> vecOp ELessTI a

    EPlusD  a -> vecOp EPlusD  a
    EMultD  a -> vecOp EMultD  a
    EEqD    a -> vecOp EEqD    a
    ELessTD a -> vecOp ELessTD a

    EVar x            -> EVar $ varL x
    EApp f e          -> EApp (vectoriseF n f) (vectoriseE n e)
    ELetIn nm val exp -> ELetIn (varL nm) (vectoriseE n val) (abstract (varL nm) (vectoriseE n $ exp (EVar nm)))

    -- For compositionality, these should be Fun-wrapped in actual use.
    EIf c t e  -> ECombine (ET3 (vectoriseE n c, vectoriseE n t, vectoriseE n e))
    E1of2 p    -> vecOp E1of2 p
    E2of2 p    -> vecOp E2of2 p
    EMapP f a  -> let freshVar = var "#sharedVar"
                  in ELetIn freshVar (vectoriseE n a) (\v -> EUnconcat $ ET2(v, vectoriseArrow n freshVar f $ EConcat v))
    ECombine cte -> vecOp ECombine cte
    EConcat a    -> vecOp EConcat a
    EReplicate a -> vecOp EReplicate a
    EUnconcat a  -> vecOp EUnconcat a
    EZip2 a      -> vecOp EZip2 a
    EZip3 a      -> vecOp EZip3 a
    EUnzip2 a    -> vecOp EUnzip2 a
    EIndex a     -> vecOp EIndex a

    -- Bigger, temporaray building blocks
    EReduceP (f, a) -> EMapP (\a -> EReduceP (f,a)) $ vectoriseE n a
    EScanP (f,a)    -> EMapP (\a -> EScanP   (f,a)) $ vectoriseE n a
  where
    vecOp :: (Exp a -> Exp b) -> Exp a -> Exp (Arr b)
    vecOp op arg = EMapP op $ vectoriseE n arg

-- }}}
-- {{{ Free variable handling
-- | Abstract a free variable expression to a lambda.
abstract :: Name a -> Exp b -> (Exp a -> Exp b)
abstract nm exp = \v -> subst v nm exp

-- | Perform variable substitution in expressions. Var is the only interesting case...
subst :: Exp a -> Name a -> Exp b -> Exp b
subst e1 nm@(Name nm') e2 =
  case e2 of
    EPlusI  a -> EPlusI  $ rec a

    EMultI  a -> EMultI  $ rec a
    EEqI    a -> EEqI    $ rec a
    ELessTI a -> ELessTI $ rec a

    EPlusD  a -> EPlusD  $ rec a
    EMultD  a -> EMultD  $ rec a
    EEqD    a -> EEqD    $ rec a
    ELessTD a -> ELessTD $ rec a


    EArrFun n fn -> let freeV = varI "#freshVar"
                    in EArrFun (rec n) (abstract freeV (rec $ fn $ EVar freeV))
    ET2(a,b) -> ET2(rec a, rec b)
    ET3(a,b,c) -> ET3(rec a, rec b, rec c)

    -- Won't work without extensive copying or De Bruijn indexing of free variables
    -- ... So we just unsafeCoerce for now.
    v@(EVar (Name nm'')) -> if nm' == nm'' then unsafeCoerce e1 else v
    EApp f a -> EApp f (rec a)
    ELetIn v val exp -> let freeV = var "#freshVar"
                        in ELetIn v (rec val) (abstract freeV (rec $ exp $ EVar freeV))

    -- For compositionality, all these might be Fun-wrapped in actual use.

    E1of2 x -> E1of2 (rec x)
    E2of2 x -> E2of2 (rec x)
    E1of3 x -> E1of3 (rec x)
    E2of3 x -> E2of3 (rec x)
    E3of3 x -> E3of3 (rec x)

    -- Note how EMapP is the only one in curried style to reflect lack of closures.
    EIf c t e -> EIf (rec c) (rec t) (rec e)
    EMapP f a -> let freeV = var "#freshVar"
                 in EMapP (abstract freeV (rec $ f $ EVar freeV)) (rec a)
    ECombine a -> ECombine $ rec a
    EConcat a  -> EConcat $ rec a
    EReplicate a -> EReplicate $ rec a
    EUnconcat a  -> EUnconcat$ rec a
    EZip2   a -> EZip2 $ rec a
    EZip3   a -> EZip3 $ rec a
    EUnzip2 a -> EUnzip2 $ rec a
    EIndex  a -> EIndex $ rec a

    -- Bigger, temporaray building blocks
    EReduceP (f, a) -> EReduceP (f, rec a)
    EScanP (f, a) -> EScanP (f, rec a)

    -- Constants don't do anything. Maybe would be nicer with explicit patterns to warn when extending...
    const -> const
  where
    rec :: Exp c -> Exp c
    rec x = subst e1 nm x

-- }}}

-- {{{ Pretty printing:

ppFun :: Fun a b -> Doc
ppFun (Fun nm fn) = "\\" <> ppName nm <+> "->" <+> (ppExp $ fn $ EVar nm)

ppName :: Name a -> Doc
ppName (Name nm) = text nm

ppExp :: Exp a -> Doc
ppExp x = case x of
  EBool b   -> text $ show b
  EInt i    -> text $ show i
  EDouble d -> text $ show d

  EPlusI (ET2(a,b))  -> ppExp a <+> "+" <+> ppExp b
  EMultI (ET2(a,b))  -> ppExp a <+> "*" <+> ppExp b
  EEqI   (ET2(a,b))  -> ppExp a <+> "=" <+> ppExp b
  ELessTI (ET2(a,b)) -> ppExp a <+> "<=" <+> ppExp b

  EPlusD (ET2(a,b))  -> ppExp a <+> "+"  <+> ppExp b
  EMultD (ET2(a,b))  -> ppExp a <+> "*"  <+> ppExp b
  EEqD   (ET2(a,b))  -> ppExp a <+> "="  <+> ppExp b
  ELessTD (ET2(a,b)) -> ppExp a <+> "<=" <+> ppExp b

  EPlusI a  -> "(+)" <+> ppExp a
  EMultI a  -> "(*)" <+> ppExp a
  EEqI   a  -> "(=)" <+> ppExp a
  ELessTI a -> "(<=) " <+> ppExp a

  EPlusD a  -> "(+)" <+> ppExp a
  EMultD a  -> "(*)" <+> ppExp a
  EEqD   a  -> "(=)" <+> ppExp a
  ELessTD a -> "(<=)" <+> ppExp a

  EArrFun n f -> parens $ ppExp n <+> "§" <+> (ppExp $ f $ EVar (var "$ix"))
  ET2 (a,b)   -> parens $ ppExp a <+> "," <+> ppExp b
  ET3 (a,b,c) -> parens $ ppExp a <+> "," <+> ppExp b <+> "," <+> ppExp c

  EVar a -> ppName a
  EApp f a -> "⟨fn⟩" <+> "$" <+> ppExp a
  ELetIn nm val exp -> "let" <+> ppName nm <+> "=" <+> ppExp val $$ "in" <+> nest 2 (ppExp $ exp $ EVar nm)

  E1of2 x -> "#1" <+> (parens $ ppExp x)
  E2of2 x -> "#2" <+> (parens $ ppExp x)
  E1of3 x -> "#1" <+> (parens $ ppExp x)
  E2of3 x -> "#2" <+> (parens $ ppExp x)
  E3of3 x -> "#3" <+> (parens $ ppExp x)


  -- Note how EMapP is the only one in curried style to reflect lack of closures.
  EIf c t e -> "if " <+> ppExp c <+> "then" $$ (nest 2 $ ppExp t) $$ "else" $$ (nest 2 $ ppExp e)
  EMapP f a -> "mapP" <+> nest 2 (parens (ppExp (f $ EVar $ var "$x")) $$ parens (ppExp a) )
  ECombine a -> "combine" <+> parens (ppExp a)
  EConcat a -> "concat" <+> parens (ppExp a)
  EReplicate a -> "replicate" <+> parens (ppExp a)
  EUnconcat  a -> "unconcat" <+> parens (ppExp a)
  EZip2      a -> "zip2" <+> parens (ppExp a)
  EZip3      a -> "zip3" <+> parens (ppExp a)
  EUnzip2    a -> "unzip2" <+> parens (ppExp a)
  EIndex     a -> "index" <+> parens (ppExp a)

  -- Bigger, temporaray building blocks
  EReduceP(f,a)-> "reduce" <+> parens ("⟨fn⟩" <+> "," <+> ppExp a)
  EScanP  (f,a)-> "scan" <+> parens ("⟨fn⟩" <+> "," <+> ppExp a)


instance (Show a, Show b) => Show (Fun a b) where
  show x = render $ ppFun x
instance Show (Name a) where
  show x = render $ ppName x
instance Show a => Show (Exp a) where
  show x = render $ ppExp x

--- }}}

-- {{{ Sparse matrix-vector multiplication

type SparseMat = Arr (Arr (Int,Double))

sparseMatMult :: Fun (SparseMat, Arr Double) (Arr Double)
sparseMatMult =
  Fun (var "matVec") (\matVec ->
    ELetIn (var "mat") (E1of2 matVec)
    (\mat ->
      ELetIn (var "vec") (E2of2 matVec)
      (\vec -> EMapP (reducePPlus)
                     (EMapP (colProd vec) mat)
      ))
  )
  where
    reducePPlus ::Exp (Arr Double) -> Exp Double
    reducePPlus x = EReduceP (plusDFn, x)
    colProd :: Exp (Arr Double) -> Exp (Arr (Int,Double)) -> Exp (Arr Double)
    colProd vec mat = ELetIn (var "colsVals") (EUnzip2 mat)
      (\colsVals -> ELetIn (var "cols") (E1of2 colsVals)
      (\cols -> ELetIn (var "cells") (E2of2 colsVals)
      (\cells -> EMapP (\cellVec ->
                         ELetIn (var "cell") (E1of2 cellVec)
                         (\cell ->
                         ELetIn (var "vec") (E2of2 cellVec)
                         (\vec  -> EMultD $ ET2 (cell, vec)
                         )))
                 (EZip2 $ ET2(cells, (EIndex $ ET2(cols, vec))))
      )))

plusIFn :: Fun (Int, Int) Int
plusIFn = Fun (var "ops") EPlusI

plusDFn :: Fun (Double, Double) Double
plusDFn = Fun (var "ops") EPlusD

sparseMatMultVec = vectoriseF 10 sparseMatMult

{- Segmented array style: (using `[: [: (F,Int) :] :]` for matrix)

    matVecMult mat vec =
       mapP rowSum (mapP colProd mat)
       where
         rowSum = reduceP (+)
         colProd row = let
           (cells,cols) = unzipP row
           in mapP (uncurry (*)) (zipP cells (indexP cols vec))


Flattening:
-----------

    ⟨reduceP op arr⟩ → ⟨reduceP op arr⟩  -- (primitive for now)
    (mapP_s: ) ⟨mapP f arr⟩ → ⟨f⟩ arr    -- dph also need to handle closure environment extension
    (mapP_L: ) ⟨mapP...

    mapP   :: (a -> b) -> PA a -> PA b
    ⟨mapP⟩ :: PA (a -> b) -> PA (PA a) -> PA (PA b)

    ⟨mapP f arr⟩ → unconcatP arr (⟨f⟩ (concatP arr))

    unzipP :: PA (a, b) -> (PA a, PA b)
    ⟨unzipP⟩ :: PA (PA (a,b)) -> PA (PA a, PA b)

    flatten matVecMult →
      let
        matVecMult_s :: ([:[:(F,Int):]:], [:F:]) -> [:F:]
        matVecMult_s (mat, vec) =
        matVecMult_p :: [: ( [: [: (F,Int) :] :], [: F :] ) :] -> [: [:F:] :]
        matVecMult_p matVecs =
          let colProds = unconcatP matVecs (colProd_p (concatP matVecs))
           in unconcatP colProds (rowSum_p (concatP colProds))
          where
            rowSum_p  = ⟨reduceP (+)⟩
            colProd_p rows = let
              cellsCols_p :: PA (PA a, PA b)
              cellsCols_p = ⟨unzipP⟩ rows -- possibly the same as ⟨mapP⟩ unzipP ?
              ...
              in ...
      in (matVecMult_s, matVecMult_p


-}

-- }}}
