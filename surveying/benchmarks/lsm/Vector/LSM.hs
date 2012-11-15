
import qualified Data.Vector as DV
import Data.Vector (Vector)
import System.Random
import Data.Random.Normal

m, n_paths :: Int
m = 2            -- time steps
n_paths = 4 -- paths for valuation

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

-- ^ Generate paths
genPaths :: Int -> IO (Vector(Vector Double))
genPaths n = DV.fromList `fmap` unfoldrNM iter (m+1) initvec
  where
    initvec :: Vector Double
    initvec = DV.generate n (const s0)

    iter :: Vector Double -> IO (Vector Double)
    iter prev = do
      ran <- rng n
      return $ DV.zipWith (\s x -> s * exp((r-vol**2/2)*dt + vol*x*sqrt(dt))) prev ran

polyval :: Vector Double -> Double -> Double
polyval p x = DV.sum $ DV.zipWith (\p' n' -> p'*x**(fromIntegral n')) p pows
  where
   n = DV.length p
   pows = DV.fromList [n-1..0]

-- http://facstaff.unca.edu/mcmcclur/class/LinearII/presentations/html/leastsquares.html
vander :: Vector Double -> Vector (Vector Double)
vander xs = DV.generate (DV.length xs) (\x -> DV.map (** (fromIntegral x)) xs)

-- https://github.com/numpy/numpy/blob/master/numpy/lib/polynomial.py#L394
--polyfit :: 

main :: IO ()
main = do
  a <- genPaths 4
  print a




