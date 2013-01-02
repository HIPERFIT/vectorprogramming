module BrownianBridge (brownianBridge) where

import Data.Array.Accelerate
import PricingTypesS

brownianBridge :: Pricing_Data -> Acc (Array DIM2 SpecReal) -> Acc (Array DIM3 SpecReal)
brownianBridge = undefined

brownian_bridge_gen :: Pricing_Data -> [SpecReal] -> [[SpecReal]]
brownian_bridge_gen conf@Pricing_Data{..} =
    arrangePerDate . map (mkBBridge conf) . divideInto md_dim

-- arrangePerDate :: [VB.Vector SpecReal] -> [[SpecReal]]
-- arrangePerDate vs = let l = VB.length (head vs)
--                     in [ map (VB.! k) vs | k <- [0..l-1] ]

divideInto :: Int -> Acc (Array DIM2 SpecReal) -> Acc (Array DIM3 SpecReal)
divideInto n xs = backpermute (\(Z :. i :. j) -> Z :. i :. j :. (j `mod` n)) xs


mkBBridge :: Pricing_Data -> [SpecReal] -> VB.Vector SpecReal
mkBBridge Pricing_Data{..} xs = runST mkBBridgeST
    where mkBBridgeST :: ST s (VB.Vector SpecReal)
          mkBBridgeST = do v <- VM.new md_nb_path_dates
                           res <- fillRec 0 v
                           VB.unsafeFreeze res
          fillRec n v | n == md_nb_path_dates = return v
                      | n == 0 = do VM.write v (bb_bi VB.! 0-1) (bb_sd VB.! 0 * head xs)
                                    fillRec (n+1) v
                      | otherwise = do
                          let lb  = bb_li VB.! n - 1
                              ub  = bb_ri VB.! n - 1
                              mid = bb_bi VB.! n - 1
                              zi  = xs !! n
                          wk <- VM.read  v ub
                          let tmp = bb_rw VB.! n * wk + bb_sd VB.! n * zi
                          if lb == -1 then VM.write v mid tmp
                                      else do z <- VM.read v lb
                                              VM.write v mid (tmp+z*bb_lw VB.! n)
                          fillRec (n+1) v
