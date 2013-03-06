{-# LANGUAGE CPP, MagicHash, BangPatterns #-}

module Unfold where

import GHC.Prim
import GHC.Types
import GHC.Int

import System.IO.Unsafe

import Data.Array.Repa
import Data.Array.Repa.Eval
import Data.Array.Repa.Eval.Gang

-------------------------------------------------------------------------------
-- | Fill something in parallel.
-- 
--   * The array is split into linear chunks,
--     and each thread linearly fills one chunk.
-- 
unfoldFillChunkedP
        :: Int                  -- ^ Number of elements.
        -> (Int -> b -> IO ())  -- ^ Update function to write into result buffer.
        -> (Int -> a -> a)      -- ^ Recursive fn to get the value at a given index.
        -> (Int -> a)           -- ^ Inductive fn to get the value at a given index.
        -> (a -> b)             -- ^ Generate output (useful for fusion)
        -> IO ()
unfoldFillChunkedP !(I# len) write getElemRec getElemInd output
 =      gangIO theGang
         $  \(I# thread) -> 
              let !start   = splitIx thread
                  !end     = splitIx (thread +# 1#)
              in  fillStart start end

 where
        -- Decide now to split the work across the threads.
        -- If the length of the vector doesn't divide evenly among the threads,
        -- then the first few get an extra element.
        !(I# threads)   = gangSize theGang
        !chunkLen       = len `quotInt#` threads
        !chunkLeftover  = len `remInt#`  threads

        {-# INLINE splitIx #-}
        splitIx thread
         | thread <# chunkLeftover = thread *# (chunkLen +# 1#)
         | otherwise               = thread *# chunkLen  +# chunkLeftover

        -- Evaluate the elements of a single chunk.
        {-# INLINE fillStart #-}
        fillStart !ix !end
         | ix >=# end           = return ()
         | otherwise = do 
            let x = getElemInd (I# ix)
            write (I# ix) (output x)
            fillRec (ix +# 1#) end x

        -- Evaluate the elements of a single chunk.
        {-# INLINE fillRec #-}
        fillRec !ix !end x
         | ix >=# end           = return ()
         | otherwise = do 
            let x' = getElemRec (I# ix) x
            write (I# ix) (output x')
            fillRec (ix +# 1#) end x'
{-# INLINE [0] unfoldFillChunkedP #-}

unfoldChunkedP :: (Target r b, Source r b, Monad m) => 
                  Int -> (Int -> a -> a) -> (Int -> a) -> (a -> b) -> m (Array r DIM1 b)
unfoldChunkedP n next skipahead output = now $ unsafePerformIO $ do
  mvec <- newMVec n
  unfoldFillChunkedP n (unsafeWriteMVec mvec) next skipahead output
  unsafeFreezeMVec (Z :. n) mvec
