import qualified Data.Vector as DV
import Data.Vector (Vector)
import Data.List (intersperse)
import System.Random
import Data.Random.Normal
import Control.Monad
import Control.DeepSeq
import LinAlg

-- Simulation parameters
n_paths :: Int
n_paths = 20000          -- time steps
n_points :: Int
n_points = 252
reg = 2

-- American put option parameters
r,vol,s0,t,dt,df :: Double
r   = 0.03               -- short rate
vol = 0.2                -- volatility
s0  = 100.0               -- initial stock level
t   = 1.0                -- time - to - maturity
dt  = t/(fromIntegral n_points) -- length of time interval
df  = exp(-r*dt)         -- discount factor per time interval
k   = 100.0              -- strike price

-- ^ Generate @n@ samples from a mean=0, stddev=1 normal distribution
rng :: Int -> IO (Vector Double)
rng n = do
  gen <- newStdGen
  let list = DV.fromList . take (n `div` 2) $ normals gen
  return $ list DV.++ (DV.reverse list)

-- ^ Monadic unfoldr. Stops unfolding after N steps and includes the
-- seed in the result.
unfoldrNM :: (Monad m) => (a -> m a) -> Int -> a -> m [a]
unfoldrNM _ 0 seed = return [seed]
unfoldrNM f n seed = do
  x <- f seed
  xs <- unfoldrNM f (n-1) x
  return (seed : xs)

-- testUnfold :: IO [Int]
-- testUnfold = unfoldrNM (\x -> return $ x + 1) 5 0

-- ^ Generate paths
genPaths :: Int -> Int -> IO (Vector(Vector Double))
genPaths m n = DV.fromList `fmap` unfoldrNM iter m initvec
  where
    coef1 = dt*(r-0.5*vol*vol)
    coef2 = vol*sqrt(dt)
    initvec :: Vector Double
    initvec = DV.generate n (const s0)

    iter :: Vector Double -> IO (Vector Double)
    iter prev = do
      ran <- rng n
      return $ DV.zipWith (\s x -> s * exp(coef1 + x*coef2)) prev ran

iv :: Vector Double -> Vector Double
iv = DV.map (\x -> max (k-x) 0)

average :: Vector Double -> Double
average xs = DV.sum xs / (fromIntegral $ DV.length xs)

pick :: Vector (Bool, a) -> Vector a
pick = DV.map snd . DV.filter fst

lsm :: Int -> Int -> IO Double
lsm n_points n_paths = do
  s <- genPaths n_points n_paths
  let init_disccashflow = iv (DV.last s) :: Vector Double
  res <- DV.foldM lsm' init_disccashflow (DV.reverse $ DV.init s)
  return . average $ res
 where 
  exercise_decision ev iv v | iv > 0 && iv > ev = iv
                            | otherwise = v

  lsm' :: Vector Double -> Vector Double -> IO (Vector Double)
  lsm' disccashflow s = do
    let intrinsic_value = iv s
        p = DV.map (>0) intrinsic_value
        default_out = DV.map (*df) disccashflow
        y = pick $ DV.zip p default_out
        spick = pick $ DV.zip p s
        rg = polyfit spick y reg
        estimatedtimevalue = polyvals rg s
    return $ DV.zipWith3 exercise_decision 
                estimatedtimevalue
                intrinsic_value
                default_out

main :: IO ()
main = print =<< lsm n_points n_paths
 where
   n_points = 252 -- time steps
   n_paths = 2000


-- main :: IO ()
-- main = do
--   s <- readPaths
--   let v = iv (DV.last s) :: Vector Double
--   let v' = DV.map (*df) $ v
--   let s' = DV.last $ DV.init s
--   let rg = polyfit s' v' reg
--   print rg

-- writePaths :: Vector (Vector Double) -> IO ()
-- writePaths xs = zipWithM_ printPath [1..] (DV.toList $ transpose xs)
--   where
--     printPath :: Int -> Vector Double -> IO ()
--     printPath i xs = zipWithM_ (\x -> putStrLn . ((show i ++ "," ++ show x ++ ",") ++) . show) [0..] $ DV.toList xs

-- readPaths :: IO (Vector (Vector Double))
-- readPaths = do
--   content <- readFile "paths.csv"
--   let processline :: String -> Vector Double
--       processline line = DV.fromList . map read $ words line
--   return . DV.fromList . map processline $ lines content
                      
-- main = writePaths =<< genPaths 50 (4*4096)

--instance VU.Unbox a => NFData (VU.Vector a)

-- instance NFData a => NFData (DV.Vector a) where
--   rnf v = DV.foldl' (\x y -> y `deepseq` x) () v