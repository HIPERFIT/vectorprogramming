module ContPlay where

import Control.Monad.Cont

reset :: Cont a a -> Cont r a
reset m = cont $ \ k -> k $ runCont m id

shift :: ( (a -> Cont r r) -> Cont r r) -> Cont r a
shift f = cont $ \ k -> runCont (f $ \ x -> cont $ \ k' -> k' $ k x) id
-- x  :: a
-- k  :: (a -> r)
-- k' :: (r -> r)

-- | Interesting....
act = do
  wtf' <- shift $ 
    \k -> do
            x <- k 1
            y <- k 2
            z <- k 3
            return $ x ++ x ++ y ++ y ++ z ++ z
  wtf <- shift $ 
    \k -> do
            x <- k 4
            y <- k 5
            z <- k 6
            return $ x ++ y ++ z
  return $ [(wtf,wtf')]
