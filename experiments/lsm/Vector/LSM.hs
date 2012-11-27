import qualified Data.Vector as DV
import Data.Vector (Vector)
import Data.List (intersperse)
import System.Random
import Data.Random.Normal
import Control.Monad
import Control.DeepSeq
import LinAlg

m :: Int
m = 50            -- time steps
reg = 9

r,vol,s0,t,v0_right,dt,df :: Double
r   = 0.06               -- short rate
vol = 0.2                -- volatility
s0  = 36.0               -- initial stock level
t   = 1.0                -- time - to - maturity
v0_right = 4.48637       -- American Put Option ( 500 steps bin . model)
dt  = t/(fromIntegral m) -- length of time interval
df  = exp(-r*dt)         -- discount factor per time interval

-- ^ Generate @n@ samples from a mean=0, stddev=1 normal distribution
rng :: Int -> IO (Vector Double)
rng n = do
  gen <- newStdGen
  return . DV.fromList . take n $ normals gen

-- ^ Monadic unfoldr. Stops unfolding after N steps and includes the
-- seed in the result.
unfoldrNM :: (Monad m) => (a -> m a) -> Int -> a -> m [a]
unfoldrNM _ 0 seed = return [seed]
unfoldrNM f n seed = do
  x <- f seed
  xs <- unfoldrNM f (n-1) x
  return (seed : xs)


testUnfold :: IO [Int]
testUnfold = unfoldrNM (\x -> return $ x + 1) 5 0

-- ^ Generate paths
genPaths :: Int -> Int -> IO (Vector(Vector Double))
genPaths m n = DV.fromList `fmap` unfoldrNM iter m initvec
  where
    initvec :: Vector Double
    initvec = DV.generate n (const s0)

    iter :: Vector Double -> IO (Vector Double)
    iter prev = do
      ran <- rng n
      return $ DV.zipWith (\s x -> s * exp((r-(vol**2)/2)*dt + vol*x*sqrt(dt))) prev ran

iv :: Vector Double -> Vector Double
iv = DV.map (\x -> max (40-x) 0)

average :: Vector Double -> Double
average xs = DV.sum xs / (fromIntegral $ DV.length xs)

lsm :: Int -> Int -> IO Double
lsm m n_paths = do
  s <- genPaths m n_paths
  s <- readPaths
  let v = iv (DV.last s) :: Vector Double
  res <- DV.foldM lsm' v (DV.reverse $ DV.init s)
  return . average $ res
 where 
  exercise_decision c h v | h > c = h
                          | otherwise = v

  lsm' :: Vector Double -> Vector Double -> IO (Vector Double)
  lsm' v s = do
    let h = iv s
        v' = DV.map (*df) $ v
        rg = polyfit s v' reg
        c = polyval rg s
    return $ DV.zipWith (exercise_decision c) h v'

main :: IO ()
main = print =<< lsm m n_paths
 where
   m = 50 -- time steps
   n_paths = 100


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

readPaths :: IO (Vector (Vector Double))
readPaths = do
  content <- readFile "paths.csv"
  let processline :: String -> Vector Double
      processline line = DV.fromList . map read $ words line
  return . DV.fromList . map processline $ lines content
                      
-- main = writePaths =<< genPaths 50 (4*4096)

--instance VU.Unbox a => NFData (VU.Vector a)

instance NFData a => NFData (DV.Vector a) where
  rnf v = DV.foldl' (\x y -> y `deepseq` x) () v