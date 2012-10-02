module American.Model where

type F = Double

data Model = Model 
             {
               bankDays :: Int
             , alpha :: F
             , sigma :: F
             , r :: F
             , s0 :: F
             , strike :: F
             }
 deriving Show

defaultModel = Model
             {
               bankDays = 252
             , alpha = 0.07
             , sigma = 0.20
             , r = 0.03
             , s0 = 100
             , strike = 100
             }
