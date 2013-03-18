-- ^ Defines the standard constructions necessary for the technique
-- described in "Deconstraining DSLs" by Will Jones, Tony Field and
-- Tristan Allwood (ICFP 2012)

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Deconstrain where

data Evidence a as where
  Head :: Evidence a (a ': as)
  Tail :: (Elem a as) => Evidence a (b ': as)
  
class Elem a as where  
  evidence :: Evidence a as

instance Elem a (a ': as) where
  evidence = Head
  
instance Elem a as => Elem a (b ': as) where
  evidence = Tail

data Proxy :: [*] -> * where 
  Proxy :: Proxy as

data Trap c b where
  Trap :: c b => Trap c b

class All c as where
  withElem :: Elem b as => Proxy as -> (Trap c b -> d) -> d

instance (c a, All c as) => All c (a ': as) where
  withElem _ (f :: Trap c b -> d) = 
    case (evidence :: Evidence b (a ': as)) of
      Head -> f Trap
      Tail -> withElem (Proxy :: Proxy as) f

instance All c '[] where
  withElem _ (f :: Trap c b -> d) = seq (evidence :: Evidence b '[]) undefined
