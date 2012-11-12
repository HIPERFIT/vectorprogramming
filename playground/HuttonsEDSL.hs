{-# LANGUAGE GADTs #-}
module HuttonsEDSL where

-- | A minimalistic EDSL.

-- | Expressions. More simple at the top, more specific structures below.
data Exp a where
  EBool     :: Bool -> Exp Bool
  EInt      :: Int -> Exp Int
  EDouble   :: Double -> Exp Double

  EPlusI     :: Exp Int -> Exp Int -> Exp Int
  EEqI       :: Exp Int -> Exp Int -> Exp Bool
  ELessTI    :: Exp Int -> Exp Int -> Exp Bool

  EPlusD     :: Exp Double -> Exp Double -> Exp Double
  EMultD     :: Exp Double -> Exp Double -> Exp Double
  EEqD       :: Exp Double -> Exp Double -> Exp Bool
  ELessTD    :: Exp Double -> Exp Double -> Exp Bool

  -- ELam      :: (Exp a -> Exp b) -> Exp (a -> b)
  -- EVar      :: String -> Exp a
  EApp      :: Fun a b -> Exp a -> Exp b
  ELetIn    :: Exp a -> (Exp a -> Exp b) -> Exp b

  EIf       :: Exp Bool -> Exp a -> Exp a -> Exp a
  EPair     :: Exp a -> Exp b -> Exp (a,b)
  EFst      :: Exp (a,b) -> Exp a
  ESnd      :: Exp (a,b) -> Exp b

  EArrFun   :: Exp Int -> (Exp Int -> Exp a) -> Exp (Arr a)
  EMapP     :: (Exp a -> Exp b) -> Exp (Arr a) -> Exp (Arr b)
  EConcat   :: Exp (Arr (Arr a)) -> Exp (Arr a)
  EUnconCat :: Exp (Arr a) -> Exp (Arr b) -> Exp (Arr b)
  EZip      :: Exp (Arr a) -> Exp (Arr b) -> Exp (Arr (a,b))
  EUnzip    :: Exp (Arr (a,b)) -> Exp (Arr a, Arr b)
  EIndex    :: Exp (Arr Int) -> Exp (Arr a) -> Exp (Arr a)

-- Only allow top-level functions by having user defined functions as a
-- distinct type.
data Fun a b where Fun :: (Exp a -> Exp b) -> Fun a b
data Arr  a = Arr [a]

type SparseMat = Arr (Arr (Int,Double))

sparseMatMult :: Fun (SparseMat, Arr Double) (Arr Double)
sparseMatMult =
  Fun (\matVec ->
    ELetIn (EFst matVec)
    (\mat ->
      ELetIn (ESnd matVec)
      (\vec -> EMapP (reducePPlus)
                     (EMapP (colProd vec) mat)
      ))
  )
  where
    reducePPlus = undefined :: Exp (Arr Double) -> Exp Double
    colProd :: Exp (Arr Double) -> Exp (Arr (Int,Double)) -> Exp (Arr Double)
    colProd vec mat = ELetIn (EUnzip mat)
      (\colsVals -> ELetIn (EFst colsVals)
      (\cols -> ELetIn   (ESnd colsVals)
      (\cells -> EMapP (\cellVec ->
                         ELetIn (EFst cellVec)
                         (\cell ->
                         ELetIn (ESnd cellVec)
                         (\vec  -> EMultD cell vec
                         )))
                 (EZip cells (EIndex cols vec))
      )))


{-
Segmented array style: (using `[: [: (F,Int) :] :]` for matrix)

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
