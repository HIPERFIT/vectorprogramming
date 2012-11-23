-- vim: set foldmethod=marker :
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module HuttonsEDSL where

import Control.Applicative
import Control.Monad.State

import Data.Maybe
import Data.Unique
import Data.Typeable
import Text.PrettyPrint as PP

-- {{{ A minimalistic EDSL.
-- | Expressions. More simple at the top, more specific structures below.
data Exp a where
  EBool      :: Bool -> Exp Bool
  EInt       :: Int -> Exp Int
  EDouble    :: Double -> Exp Double

  EArrFun    :: Typeable a => Exp Int -> Name Int -> (Exp Int -> Exp a) -> Exp (Arr a)
  ET2        :: (Typeable a, Typeable b) => (Exp a, Exp b) -> Exp (a,b)
  ET3        :: (Typeable a, Typeable b, Typeable c) => (Exp a, Exp b, Exp c) -> Exp (a,b,c)

  -- ELam      :: (Exp a -> Exp b) -> Exp (a -> b)
  EVar       :: Name a -> Exp a
  EApp       :: (Typeable a, Typeable b) => Fun a b -> Exp a -> Exp b
  ELetIn     :: (Typeable a, Typeable b) => Name a -> Exp a -> (Exp a -> Exp b) -> Exp b

  -- For compositionality, all these might be Fun-wrapped in actual use.

  E1of2      :: (Typeable a, Typeable b) => Exp (a,b) -> Exp a
  E2of2      :: (Typeable a, Typeable b) => Exp (a,b) -> Exp b
  E1of3      :: (Typeable a, Typeable b, Typeable c) => Exp (a,b,c) -> Exp a
  E2of3      :: (Typeable a, Typeable b, Typeable c) => Exp (a,b,c) -> Exp b
  E3of3      :: (Typeable a, Typeable b, Typeable c) => Exp (a,b,c) -> Exp c

  -- Note how EMapP is the only one in curried style to reflect lack of closures.
  EIf        :: Exp Bool -> Exp a -> Exp a -> Exp a
  EMapP      :: (Typeable a, Typeable b) => Name a -> (Exp a -> Exp b) -> Exp (Arr a) -> Exp (Arr b)
  ECombine   :: Typeable a => Exp (Arr Bool, Arr a, Arr a) -> Exp (Arr a)
  EConcat    :: Typeable a => Exp (Arr (Arr a)) -> Exp (Arr a)
  EReplicate :: Typeable a => Exp (Int, a) -> Exp (Arr a)
  EUnconcat  :: (Typeable a, Typeable b) => Exp (Arr (Arr a), Arr b) -> Exp (Arr (Arr b))
  EZip2      :: (Typeable a, Typeable b) => Exp (Arr a, Arr b) -> Exp (Arr (a,b))
  EZip3      :: (Typeable a, Typeable b, Typeable c) => Exp (Arr a, Arr b, Arr c) -> Exp (Arr (a,b,c))
  EUnzip2    :: (Typeable a, Typeable b) => Exp (Arr (a,b)) -> Exp (Arr a, Arr b)
  EIndex     :: Typeable a => Exp (Arr Int, Arr a) -> Exp (Arr a)

  -- Bigger, temporaray building blocks
  EReduceP   :: (Typeable a) => (Fun (a, a) a, Exp (Arr a)) -> Exp a
  EScanP     :: (Typeable a) => (Fun (a, a) a, Exp (Arr a)) -> Exp (Arr a)
  deriving (Typeable)

data Name a where Name :: Int -> String -> Name a
  deriving Typeable

-- Only allow top-level functions by having user defined functions as a
-- distinct type.
data Fun a b where
  Fun      :: String -> Name a -> (Exp a -> Exp b) -> Fun a b
  FBuiltin :: String -> (a -> b) -> Fun a b
  deriving Typeable

data Arr a   where Arr  :: [a] -> Arr a
  deriving Typeable

-- (non-fresh) construction of a variable name
var :: String -> Name a
var nm = Name 0 nm

-- }}}
-- {{{ "StdLib

plus :: (Num a) => Fun (a,a) a
plus = FBuiltin "+" (uncurry (+))
(+.) x y = EApp plus (ET2(x,y))

minus :: (Num a) => Fun (a,a) a
minus = FBuiltin "-" (uncurry (-))
(-.) x y = EApp plus (ET2(x,y))

mult :: (Num a) => Fun (a,a) a
mult = FBuiltin "*" (uncurry (*))
x *. y = EApp mult (ET2(x,y))

pow :: Fun (Double,Int) Double
pow = FBuiltin "^" (uncurry (^))
x ^. y = EApp pow (ET2(x,y))

div' :: Fun (Double, Double) Double
div' = FBuiltin "/" (uncurry (/))
x /. y = EApp div' (ET2(x,y))

exp' :: Fun Double Double
exp' = FBuiltin "exp" exp

sqrt' :: Fun Double Double
sqrt' = FBuiltin "sqrt" sqrt

toDouble :: Fun Int Double
toDouble = FBuiltin "toDouble" fromIntegral

($.) :: (Typeable a, Typeable b) => Fun a b -> Exp a -> Exp b
($.) = EApp

-- }}}
-- {{{ Vectorisation :

-- | Vectorise a function (Fun). No support for recursion yet.
vectoriseF :: Typeable a =>
              Typeable b => Int -> Fun a b -> Fresh (Fun (Arr a) (Arr b))
vectoriseF n (Fun nm argNm fn) = do
  Fun nm (varL argNm) <$> vectoriseArrow n fn
vectoriseF n op@(FBuiltin nm fn) = do
  ix <- getVar "#ix"
  fnArg <- getVar "#fnArg"
  return $ Fun (nm++"_L") (varL fnArg) (\v -> EMapP ix (\ix -> EApp op ix) v)

vectoriseArrow :: Typeable a =>
                  -- Typeable b => Int -> Name a -> (Exp a -> Exp b) -> Fresh (Exp (Arr a) -> Exp (Arr b))
                  Typeable b => Int -> (Exp a -> Exp b) -> Fresh (Exp (Arr a) -> Exp (Arr b))
--vectoriseArrow n argNm fn = abstract (varL argNm) (vectoriseE n $ fn $ EVar argNm)
vectoriseArrow n fn = do
  freshNm <- getVar "fresh"
  vecBody <- vectoriseE n $ fn $ EVar freshNm
  return $ abstract (varL freshNm) vecBody

-- | Vectorise an expression (Exp). No support for function recursion yet.
vectoriseE :: (Typeable a) => Int -> Exp a -> Fresh (Exp (Arr a))
vectoriseE n exp =
  case exp of
    EBool x -> return $ EReplicate (ET2 (EInt n, EBool x))
    EInt x  -> return $ EReplicate (ET2 (EInt n, EInt x))
    EDouble x -> return $ EReplicate (ET2 (EInt n, EDouble x))

    ET2 (f, s) -> ((EZip2 . ET2) .) . (,) <$> vectoriseE n f <*> vectoriseE n s
    ET3 (f, s, t) -> (((EZip3 . ET3) .) .) . (,,) <$> vectoriseE n f <*> vectoriseE n s <*> vectoriseE n t
    EArrFun n' ixVar f -> return $ EReplicate (ET2 (EInt n, EArrFun n' ixVar f))

{-
    EPlusI  a -> vecOp EPlusI  a
    EMultI  a -> vecOp EMultI  a
    EEqI    a -> vecOp EEqI    a
    ELessTI a -> vecOp ELessTI a

    EPlusD  a -> vecOp EPlusD  a
    EMultD  a -> vecOp EMultD  a
    EEqD    a -> vecOp EEqD    a
    ELessTD a -> vecOp ELessTD a
    -}

    EVar x            -> return $ EVar $ varL x
    EApp f e          -> EApp <$> vectoriseF n f <*> vectoriseE n e
    ELetIn nm val exp -> do
      vecBody <- vectoriseE n $ exp (EVar nm)
      vecVal <- vectoriseE n val
      return $ ELetIn (varL nm) vecVal (abstract (varL nm) vecBody)

    -- For compositionality, these should be Fun-wrapped in actual use.
    EIf c t e  -> (((ECombine . ET3) .) .) . (,,) <$> vectoriseE n c
                                                  <*> vectoriseE n t
                                                  <*> vectoriseE n e
    E1of2 p    -> E1of2 . EUnzip2 <$> vectoriseE n p -- vecOp E1of2 p
    E2of2 p    -> E2of2 . EUnzip2 <$> vectoriseE n p -- vecOp E2of2 p
    EMapP ixVar f a  -> do
      freshArg <- getVar "#sharedArg"
      freshConcat <- getVar "#sharedConcat"
      bodyExp <- vectoriseArrow n f
      ELetIn freshArg <$> vectoriseE n a <*> pure (\v -> EUnconcat $ ET2 (v, ELetIn freshConcat (EConcat v) bodyExp))
    ECombine cte -> vecOp ECombine cte
    EConcat a    -> vecOp EConcat a
    EReplicate a -> vecOp EReplicate a
    EUnconcat a  -> vecOp EUnconcat a
    EZip2 a      -> vecOp EZip2 a
    EZip3 a      -> vecOp EZip3 a
    EUnzip2 a    -> vecOp EUnzip2 a
    EIndex a     -> vecOp EIndex a

    -- Bigger, temporaray building blocks
    EReduceP (f, a) -> EMapP <$> getVar "#ix" <*> pure (\a -> EReduceP (f,a)) <*> vectoriseE n a
    EScanP (f,a)    -> EMapP <$> getVar "#ix" <*> pure (\a -> EScanP   (f,a)) <*> vectoriseE n a
  where
    -- Vectorise a builtin operator
    vecOp :: (Typeable a,Typeable b) => (Exp a -> Exp b) -> Exp a -> Fresh (Exp (Arr b))
    vecOp op arg = EMapP <$> getVar "#ix" <*> pure op <*> vectoriseE n arg

-- }}}
-- {{{ Free variable handling

-- A monad for generating fresh names
type Fresh a = State Int a

runFresh :: Fresh a -> a
runFresh act = evalState act 0

getVar :: String -> Fresh (Name a)
getVar nm = do
  cnt <- get
  put $ succ cnt
  return $ Name cnt nm

-- | Lift a name to vectorised type.
varL :: Name a -> Name (Arr a)
varL (Name i n) = Name i n

-- | Abstract a free variable expression to a lambda.
abstract :: Typeable a =>
            Typeable b => Name a -> Exp b -> (Exp a -> Exp b)
abstract nm exp = \v -> subst v nm exp

-- | Perform variable substitution in expressions. Var is the only interesting case...
subst :: Typeable a =>
         Typeable b => Exp a -> Name a -> Exp b -> Exp b
subst e1 nm@(Name cnt nm') e2 =
  case e2 of
{-
    EPlusI  a -> EPlusI  $ rec a

    EMultI  a -> EMultI  $ rec a
    EEqI    a -> EEqI    $ rec a
    ELessTI a -> ELessTI $ rec a

    EPlusD  a -> EPlusD  $ rec a
    EMultD  a -> EMultD  $ rec a
    EEqD    a -> EEqD    $ rec a
    ELessTD a -> ELessTD $ rec a
-}
    EArrFun n ixV fn -> EArrFun (rec n) ixV (abstract ixV (rec $ fn $ EVar ixV))
    ET2(a,b) -> ET2(rec a, rec b)
    ET3(a,b,c) -> ET3(rec a, rec b, rec c)

    -- Won't work without extensive copying or De Bruijn indexing of free variables
    -- ... So we just unsafeCoerce for now.
    v@(EVar (Name cnt' nm'')) -> if nm' == nm'' && cnt == cnt'
                                 then case cast e1 of {Just e1 -> e1; Nothing -> v}
                                 else v
    EApp f a -> EApp f (rec a)
    ELetIn v val exp -> ELetIn v (rec val) (abstract v (rec $ exp $ EVar v))

    -- For compositionality, all these might be Fun-wrapped in actual use.

    E1of2 x -> E1of2 (rec x)
    E2of2 x -> E2of2 (rec x)
    E1of3 x -> E1of3 (rec x)
    E2of3 x -> E2of3 (rec x)
    E3of3 x -> E3of3 (rec x)

    -- Note how EMapP is the only one in curried style to reflect lack of closures.
    EIf c t e -> EIf (rec c) (rec t) (rec e)
    EMapP ixV f a -> EMapP ixV (abstract ixV (rec $ f $ EVar ixV)) (rec a)
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
    rec :: Typeable c => Exp c -> Exp c
    rec x = subst e1 nm x

-- }}}
-- {{{ Pretty printing:

-- Determine if a name should be prettyprinted infix
infixLex [] = False
infixLex s = (head s) `elem` "+-*/'`"


ppFun :: Fun a b -> Doc
ppFun (Fun nm argNm fn) = "fun" <+> text nm <+> ppName argNm <+> "=" <+> (ppExp $ fn $ EVar argNm)
ppFun (FBuiltin nm fn) = "fun" <+> text nm <+> "x" <+> "=" <+> "<builtin>"

funName (Fun nm _ _) = nm
funName (FBuiltin nm _) = nm

ppFunName :: Fun a b -> Doc
ppFunName f = text $ funName f

ppName :: Name a -> Doc
ppName (Name cnt nm) = text $ nm  ++ if cnt > 0 then "'" ++ show cnt else ""

ppExp :: Exp a -> Doc
ppExp x = case x of
  EBool b   -> text $ show b
  EInt i    -> text $ show i
  EDouble d -> text $ show d

{-
  EPlusI (ET2(a,b))  -> ppExp a <+> "+" <+> ppExp b
  EMultI (ET2(a,b))  -> ppExp a <+> "*" <+> ppExp b
  EEqI   (ET2(a,b))  -> ppExp a <+> "=" <+> ppExp b
  ELessTI (ET2(a,b)) -> ppExp a <+> "<=" <+> ppExp b

  EPlusD (ET2(a,b))  -> ppExp a <+> "+"  <+> ppExp b
  EMultD (ET2(a,b))  -> ppExp a <+> "*"  <+> ppExp b
  EEqD   (ET2(a,b))  -> ppExp a <+> "="  <+> ppExp b
  ELessTD (ET2(a,b)) -> ppExp a <+> "<=" <+> ppExp b

  EPlusI  a -> "(+)"  <+> ppExp a
  EMultI  a -> "(*)"  <+> ppExp a
  EEqI    a -> "(=)"  <+> ppExp a
  ELessTI a -> "(<=)" <+> ppExp a

  EPlusD a  -> "(+)"  <+> ppExp a
  EMultD a  -> "(*)"  <+> ppExp a
  EEqD   a  -> "(=)"  <+> ppExp a
  ELessTD a -> "(<=)" <+> ppExp a
  -}

  EArrFun n ixV f -> parens $ ppExp n <+> "§" <+> ppName ixV <+> "->" <+> (ppExp $ f $ EVar ixV)
  ET2 (a,b)   -> parens $ ppExp a <+> "," <+> ppExp b
  ET3 (a,b,c) -> parens $ ppExp a <+> "," <+> ppExp b <+> "," <+> ppExp c

  EVar a -> ppName a
  EApp f (ET2(a,b)) | infixLex (funName f) -> ppExp a <+> ppFunName f <+> ppExp b
  EApp f a -> ppFunName f <+> "$" <+> ppExp a
  -- ELetIn nm val exp -> "let" <+> ppName nm <+> "=" <+> ppExp val $$ "in" <+> nest 2 (ppExp $ exp $ EVar nm)
  letIn@(ELetIn _ _ _) -> let (docs, body) = ppLetBind ([], letIn)
                           in "let" <+> vcat docs $$ "in" <+> (ppExp body)

  E1of2 x -> "#1" <+> (parens $ ppExp x)
  E2of2 x -> "#2" <+> (parens $ ppExp x)
  E1of3 x -> "#1" <+> (parens $ ppExp x)
  E2of3 x -> "#2" <+> (parens $ ppExp x)
  E3of3 x -> "#3" <+> (parens $ ppExp x)


  -- Note how EMapP is the only one in curried style to reflect lack of closures.
  EIf c t e -> "if " <+> ppExp c <+> "then" $$ (nest 2 $ ppExp t) $$ "else" $$ (nest 2 $ ppExp e)
  EMapP x f a -> "mapP" <+> nest 2 (parens ("\\" <> ppName x <+> "->" <+> ppExp (f $ EVar x)) $$ parens (ppExp a) )
  ECombine a -> "combine" <+> parens (ppExp a)
  EConcat a -> "concat" <+> parens (ppExp a)
  EReplicate a -> "replicate" <+> parens (ppExp a)
  EUnconcat  a -> "unconcat" <+> parens (ppExp a)
  EZip2      a -> "zip2" <+> parens (ppExp a)
  EZip3      a -> "zip3" <+> parens (ppExp a)
  EUnzip2    a -> "unzip2" <+> parens (ppExp a)
  EIndex     a -> "index" <+> parens (ppExp a)

  -- Bigger, temporaray building blocks
  EReduceP(f,a)-> "reduce" <+> parens (ppFunName f <+> "," <+> ppExp a)
  EScanP  (f,a)-> "scan" <+> parens (ppFunName f <+> "," <+> ppExp a)
  where
    ppLetBind (doc,ELetIn x val exp) = ppLetBind(doc ++ [ppName x <+> "=" <+> ppExp val], exp $ EVar x)
    ppLetBind (doc,body) = (doc,body)


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
  Fun "sparseMatMult" (var "matVec") (\matVec ->
    ELetIn (var "mat") (E1of2 matVec)
    (\mat ->
      ELetIn (var "vec") (E2of2 matVec)
      (\vec -> EMapP (var "red") (reducePPlus)
                     (EMapP (var "row") (colProd vec) mat)
      ))
  )
  where
    reducePPlus ::Exp (Arr Double) -> Exp Double
    reducePPlus x = EReduceP (plus, x)
    colProd :: Exp (Arr Double) -> Exp (Arr (Int,Double)) -> Exp (Arr Double)
    colProd vec mat = ELetIn (var "colsVals") (EUnzip2 mat)
      (\colsVals -> ELetIn (var "cols") (E1of2 colsVals)
      (\cols -> ELetIn (var "cells") (E2of2 colsVals)
      (\cells -> EMapP (var "cellVec") (\cellVec ->
                         ELetIn (var "cell") (E1of2 cellVec)
                         (\cell ->
                         ELetIn (var "vec") (E2of2 cellVec)
                         (\vec  -> cell *. vec
                         )))
                 (EZip2 $ ET2(cells, (EIndex $ ET2(cols, vec))))
      )))

sparseMatMultVec = vectoriseF 10 sparseMatMult

-- {{{ Notes
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
-- }}}


-- {{{ Vectorised binomial

data FinModel = FinModel{
 modStrike :: Double,
 modBankDays :: Int,
 modS0 :: Double,
 modR :: Double,
 modAlpha :: Double,
 modSigma :: Double
}

-- A transl(iter)ation of binom from the Vector version.
-- (Some of the model parameters are given in the meta language)
binomTranslit :: FinModel -> Fun Int Double
binomTranslit mod = Fun "binom" (var "expiry") (\expiry ->
  ELetIn (var "s0") (EDouble $ modS0 mod) (\s0 ->
  ELetIn (var "r") (EDouble $ modR mod) (\r ->
  ELetIn (var "alpha") (EDouble $ modAlpha mod) (\alpha ->
  ELetIn (var "sigma") (EDouble $ modSigma mod) (\sigma ->
  ELetIn (var "n") ((EInt $ modBankDays mod) *. expiry) (\n ->
  ELetIn (var "dt") ((toDouble $. expiry) /. (toDouble $. n)) (\dt ->
  ELetIn (var "u") (exp' $. (alpha *. dt +. sigma *. (sqrt' $. dt))) (\u ->
  ELetIn (var "d") (exp' $. (alpha *. dt -. sigma *. (sqrt' $. dt))) (\d ->
  ELetIn (var "stepR") (exp' $. r *. dt) (\stepR ->
  ELetIn (var "q") ((stepR -. d) /. (u -. d)) (\q ->
  ELetIn (var "qUR") (q /. stepR) (\qUr ->
  ELetIn (var "qDR") ((EDouble 1 -. q) /. stepR) (\qDr ->
  ELetIn (var "uPow") (EArrFun (n +. EInt 1) (var "i") (\i -> u ^. i)) (\uPow ->
  ELetIn (var "dPow") (EArrFun (n +. EInt 1) (var "i") (\i -> d ^. (n -.i {- watch out for off-by-one here! -})))
  (\dPow ->
  ELetIn (var "st") (s0 *.^ (zipWith2 (mult $.) uPow dPow)) (\st ->
  ELetIn (var "finalPut") (pmax $. ET2((EDouble $ modStrike mod) -.^ st, EDouble 0)) (\finalPut ->
    undefined
  )))))))))))))))))

  where
    pmax :: Fun (Arr Double, Double) (Arr Double)
    pmax = FBuiltin "pmax" undefined
    ppmax :: Fun (Arr Double, Arr Double) (Arr Double)
    ppmax = FBuiltin "ppmax" undefined
    (*.^) x as = EMapP (var "a") (\a -> x *. a) as
    (-.^) x as = EMapP (var "a") (\a -> x -. a) as
    zipWith2 :: (Typeable a, Typeable b, Typeable c) => (Exp (a,b) -> Exp c) -> Exp (Arr a) -> Exp (Arr b) -> Exp (Arr c)
    zipWith2 f a b = EMapP (var "ab") (\ab -> f ab) (EZip2 (ET2(a,b)))

--binom :: Int -> Double
--binom expiry = V.head first
--  where
--    uPow = V.generate (n+1) (u^)
--    dPow = V.reverse $ V.generate (n+1) (d^)
--
--    st = s0 *^ (uPow ^*^ dPow)
--    finalPut = pmax (strike -^ st) 0
--
---- for (i in n:1) {
----   St<-S0*u.pow[1:i]*d.pow[i:1]
----   put[1:i]<-pmax(strike-St,(qUR*put[2:(i+1)]+qDR*put[1:i]))
---- }
--    first = foldl' prevPut finalPut [n, n-1 .. 1]
--    prevPut put i = ppmax (strike -^ st) ((qUR *^ V.tail put) ^+^ (qDR *^ V.init put))
--      where st = s0 *^ ((V.take i uPow) ^*^ (V.drop (n+1-i) dPow))
--
--    -- standard econ parameters
--    strike = 100 :: Double
--    bankDays = 256 :: Int
--    s0 = 100 :: Double
--    r = 0.03; alpha = 0.07; sigma = 0.20
--    n = expiry*bankDays
--    dt = fromIntegral expiry/fromIntegral n
--    u = exp(alpha*dt+sigma*sqrt dt)
--    d = exp(alpha*dt-sigma*sqrt dt)
--    stepR = exp(r*dt)
--    q = (stepR-d)/(u-d)
--    qUR = q/stepR; qDR = (1-q)/stepR



-- }}}
