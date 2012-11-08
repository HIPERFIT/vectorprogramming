module Main where

gaussianElem ::  Double -> Double
gaussianElem q =
        let dq = q - 0.5
        in if( abs dq <= 0.425 ) then
               42.0
           else
               let pp = if dq < 0.0 then q else (1.0 - q)
                   s  = sqrt (0.0 - (log pp))
                   x  = if (s <= 5.0) then 3.14 * s else 2.5+s
               in if dq < 0.0 then (-x) else x

gaussian = map gaussianElem

main = defaultMain gaussian
