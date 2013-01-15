/* 1.66:cc: deleting persistent cache */
/* 1.66:cc: (3.0,"\v\175\ETB\135\236v\151\195E\ACK\141\192N F\182") */
#include <accelerate_cuda_extras.h>
static __constant__ DIM2 sh0;
static TexWord32 avar0_a0;
typedef DIM2 DimOut;
extern "C" __global__ void generate(Word32* arrOut_a1, Word8* arrOut_a0, const DimOut shOut)
{
  const int n = size(shOut);
  const int gridSize = __umul24(blockDim.x, gridDim.x);
  int ix;

  for (ix = __umul24(blockDim.x, blockIdx.x) + threadIdx.x; ix < n; ix += gridSize) {
    const DimOut x0 = fromIndex(shOut, ix);
    const int x0_a0 = x0.a0;
    const int x0_a1 = x0.a1;
    const int v0 = toIndex(sh0, shape(x0_a1, x0_a0));

    arrOut_a1[ix] = indexArray(avar0_a0, v0);
    arrOut_a0[ix] = (Word32) 0 != ((Word32) 0 & (Word32) 1 << x0_a0);
  }
}


/* 1.66:cc: execute: /usr/local/cuda/bin/nvcc -I /home/dybber/hsenvs/accelerate-github-GHC7.6.1/.hsenv_accelerate-github-GHC7.6.1/cabal/share/accelerate-cuda-0.13.0.0/cubits -arch=sm_30 -cubin -o /tmp/accelerate-cuda-16641/dragon16641.cubin -O3 -m64 /tmp/accelerate-cuda-16641/dragon16641.cu */
/* 1.66:cc: (3.0,"D\\w\183s\254\&1\218g\SO=\239\243I\157Z") */


#include <accelerate_cuda_extras.h>
extern "C" __global__ void fold(Word32* arrOut_a1, Word8* arrOut_a0, const Word32* arrIn0_a1, const Word8* arrIn0_a0, const Ix interval_size, const Ix num_intervals, const Ix num_elements)
{
  extern volatile __shared__ Word32 s0_a1[];
  volatile Word8* s0_a0 = (Word8*) &s0_a1[blockDim.x];
  Word32 x1_a1;
  Word8 x1_a0;
  Word32 x0_a1;
  Word8 x0_a0;

  if (interval_size == 0) {
    const int gridSize = __umul24(blockDim.x, gridDim.x);
    int seg;

    for (seg = __umul24(blockDim.x, blockIdx.x) + threadIdx.x; seg < num_intervals; seg += gridSize) {
      arrOut_a1[seg] = (Word32) 0;
      arrOut_a0[seg] = 1;
    }
    return;
  }
  for (int seg = blockIdx.x; seg < num_intervals; seg += gridDim.x) {
    const int start = seg * interval_size;
    const int end = min(start + interval_size, num_elements);
    const int n = min(end - start, blockDim.x);

    if (threadIdx.x >= n)
      return;


    int i = start - (start & warpSize - 1);

    if (i == start || interval_size > blockDim.x) {
      i += threadIdx.x;
      if (i >= start) {
        x1_a1 = arrIn0_a1[i];
        x1_a0 = arrIn0_a0[i];
      }
      if (i + blockDim.x < end) {
        const Word32 x0_a1 = arrIn0_a1[i + blockDim.x];
        const Word8 x0_a0 = arrIn0_a0[i + blockDim.x];

        if (i >= start) {
          const bool v0 = x0_a0;
          const bool v1 = x1_a0;

          x1_a1 = v1 ? v0 ? x0_a1 ^ x1_a1 : x1_a1 : x0_a1;
          x1_a0 = v1 ? v0 ? x1_a0 : x1_a0 : x0_a0;
        } else {
          x1_a1 = x0_a1;
          x1_a0 = x0_a0;
        }
      }
      for (i += 2 * blockDim.x; i < end; i += blockDim.x) {
        x0_a1 = arrIn0_a1[i];
        x0_a0 = arrIn0_a0[i];

        const bool v0 = x0_a0;
        const bool v1 = x1_a0;

        x1_a1 = v1 ? v0 ? x0_a1 ^ x1_a1 : x1_a1 : x0_a1;
        x1_a0 = v1 ? v0 ? x1_a0 : x1_a0 : x0_a0;
      }
    } else {
      x1_a1 = arrIn0_a1[start + threadIdx.x];
      x1_a0 = arrIn0_a0[start + threadIdx.x];
    }
    s0_a1[threadIdx.x] = x1_a1;
    s0_a0[threadIdx.x] = x1_a0;
    __syncthreads();
    if (threadIdx.x + 512 < n) {
      x0_a1 = s0_a1[threadIdx.x + 512];
      x0_a0 = s0_a0[threadIdx.x + 512];

      const bool v0 = x0_a0;
      const bool v1 = x1_a0;

      x1_a1 = v1 ? v0 ? x0_a1 ^ x1_a1 : x1_a1 : x0_a1;
      x1_a0 = v1 ? v0 ? x1_a0 : x1_a0 : x0_a0;
      s0_a1[threadIdx.x] = x1_a1;
      s0_a0[threadIdx.x] = x1_a0;
    }
    __syncthreads();
    if (threadIdx.x + 256 < n) {
      x0_a1 = s0_a1[threadIdx.x + 256];
      x0_a0 = s0_a0[threadIdx.x + 256];

      const bool v0 = x0_a0;
      const bool v1 = x1_a0;

      x1_a1 = v1 ? v0 ? x0_a1 ^ x1_a1 : x1_a1 : x0_a1;
      x1_a0 = v1 ? v0 ? x1_a0 : x1_a0 : x0_a0;
      s0_a1[threadIdx.x] = x1_a1;
      s0_a0[threadIdx.x] = x1_a0;
    }
    __syncthreads();
    if (threadIdx.x + 128 < n) {
      x0_a1 = s0_a1[threadIdx.x + 128];
      x0_a0 = s0_a0[threadIdx.x + 128];

      const bool v0 = x0_a0;
      const bool v1 = x1_a0;

      x1_a1 = v1 ? v0 ? x0_a1 ^ x1_a1 : x1_a1 : x0_a1;
      x1_a0 = v1 ? v0 ? x1_a0 : x1_a0 : x0_a0;
      s0_a1[threadIdx.x] = x1_a1;
      s0_a0[threadIdx.x] = x1_a0;
    }
    __syncthreads();
    if (threadIdx.x + 64 < n) {
      x0_a1 = s0_a1[threadIdx.x + 64];
      x0_a0 = s0_a0[threadIdx.x + 64];

      const bool v0 = x0_a0;
      const bool v1 = x1_a0;

      x1_a1 = v1 ? v0 ? x0_a1 ^ x1_a1 : x1_a1 : x0_a1;
      x1_a0 = v1 ? v0 ? x1_a0 : x1_a0 : x0_a0;
      s0_a1[threadIdx.x] = x1_a1;
      s0_a0[threadIdx.x] = x1_a0;
    }
    __syncthreads();
    if (threadIdx.x < 32) {
      if (threadIdx.x + 32 < n) {
        x0_a1 = s0_a1[threadIdx.x + 32];
        x0_a0 = s0_a0[threadIdx.x + 32];

        const bool v0 = x0_a0;
        const bool v1 = x1_a0;

        x1_a1 = v1 ? v0 ? x0_a1 ^ x1_a1 : x1_a1 : x0_a1;
        x1_a0 = v1 ? v0 ? x1_a0 : x1_a0 : x0_a0;
        s0_a1[threadIdx.x] = x1_a1;
        s0_a0[threadIdx.x] = x1_a0;
      }
      if (threadIdx.x + 16 < n) {
        x0_a1 = s0_a1[threadIdx.x + 16];
        x0_a0 = s0_a0[threadIdx.x + 16];

        const bool v0 = x0_a0;
        const bool v1 = x1_a0;

        x1_a1 = v1 ? v0 ? x0_a1 ^ x1_a1 : x1_a1 : x0_a1;
        x1_a0 = v1 ? v0 ? x1_a0 : x1_a0 : x0_a0;
        s0_a1[threadIdx.x] = x1_a1;
        s0_a0[threadIdx.x] = x1_a0;
      }
      if (threadIdx.x + 8 < n) {
        x0_a1 = s0_a1[threadIdx.x + 8];
        x0_a0 = s0_a0[threadIdx.x + 8];

        const bool v0 = x0_a0;
        const bool v1 = x1_a0;

        x1_a1 = v1 ? v0 ? x0_a1 ^ x1_a1 : x1_a1 : x0_a1;
        x1_a0 = v1 ? v0 ? x1_a0 : x1_a0 : x0_a0;
        s0_a1[threadIdx.x] = x1_a1;
        s0_a0[threadIdx.x] = x1_a0;
      }
      if (threadIdx.x + 4 < n) {
        x0_a1 = s0_a1[threadIdx.x + 4];
        x0_a0 = s0_a0[threadIdx.x + 4];

        const bool v0 = x0_a0;
        const bool v1 = x1_a0;

        x1_a1 = v1 ? v0 ? x0_a1 ^ x1_a1 : x1_a1 : x0_a1;
        x1_a0 = v1 ? v0 ? x1_a0 : x1_a0 : x0_a0;
        s0_a1[threadIdx.x] = x1_a1;
        s0_a0[threadIdx.x] = x1_a0;
      }
      if (threadIdx.x + 2 < n) {
        x0_a1 = s0_a1[threadIdx.x + 2];
        x0_a0 = s0_a0[threadIdx.x + 2];

        const bool v0 = x0_a0;
        const bool v1 = x1_a0;

        x1_a1 = v1 ? v0 ? x0_a1 ^ x1_a1 : x1_a1 : x0_a1;
        x1_a0 = v1 ? v0 ? x1_a0 : x1_a0 : x0_a0;
        s0_a1[threadIdx.x] = x1_a1;
        s0_a0[threadIdx.x] = x1_a0;
      }
      if (threadIdx.x + 1 < n) {
        x0_a1 = s0_a1[threadIdx.x + 1];
        x0_a0 = s0_a0[threadIdx.x + 1];

        const bool v0 = x0_a0;
        const bool v1 = x1_a0;

        x1_a1 = v1 ? v0 ? x0_a1 ^ x1_a1 : x1_a1 : x0_a1;
        x1_a0 = v1 ? v0 ? x1_a0 : x1_a0 : x0_a0;
      }
    }
    if (threadIdx.x == 0) {
      x0_a1 = (Word32) 0;
      x0_a0 = 1;

      const bool v0 = x0_a0;
      const bool v1 = x1_a0;

      x1_a1 = v1 ? v0 ? x0_a1 ^ x1_a1 : x1_a1 : x0_a1;
      x1_a0 = v1 ? v0 ? x1_a0 : x1_a0 : x0_a0;
      arrOut_a1[seg] = x1_a1;
      arrOut_a0[seg] = x1_a0;
    }
  }
}


/* 1.66:cc: execute: /usr/local/cuda/bin/nvcc -I /home/dybber/hsenvs/accelerate-github-GHC7.6.1/.hsenv_accelerate-github-GHC7.6.1/cabal/share/accelerate-cuda-0.13.0.0/cubits -arch=sm_30 -cubin -o /tmp/accelerate-cuda-16641/dragon16642.cubin -O3 -m64 /tmp/accelerate-cuda-16641/dragon16642.cu */
/* 1.66:cc: (3.0,"\222\SUBh\EOT\193\218T\205\130\200\138\162)\SUB\DC4\191") */
#include <accelerate_cuda_extras.h>
extern "C" __global__ void map(double* arrOut_a0, const Word32* arrIn0_a1, const Word8* arrIn0_a0, const Ix num_elements)
{
  const int gridSize = __umul24(blockDim.x, gridDim.x);
  int ix;

  for (ix = __umul24(blockDim.x, blockIdx.x) + threadIdx.x; ix < num_elements; ix += gridSize) {
    const Word32 x0_a1 = arrIn0_a1[ix];

    arrOut_a0[ix] = (double) x0_a1 / 1.073741824e9;
  }
}


/* 1.66:cc: execute: /usr/local/cuda/bin/nvcc -I /home/dybber/hsenvs/accelerate-github-GHC7.6.1/.hsenv_accelerate-github-GHC7.6.1/cabal/share/accelerate-cuda-0.13.0.0/cubits -arch=sm_30 -cubin -o /tmp/accelerate-cuda-16641/dragon16643.cubin -O3 -m64 /tmp/accelerate-cuda-16641/dragon16643.cu */
/* 1.66:cc: waiting for nvcc... */
/* 1.67:cc: persist/save: /home/dybber/.accelerate/accelerate-cuda-0.13.0.0/cache/3.0/zr182Fz20UNzr192zr141zrACKEzr195zr151vzr236zr135zrETBzr175zrv */
/* 1.67:cc: entry function 'generate' used 19 registers, 0 bytes smem, 0 bytes lmem, 8 bytes cmem */
/*      ... multiprocessor occupancy 100.0% : 2048 threads over 64 warps in 16 blocks */
/* 1.67:cc: waiting for nvcc... */
/* 1.67:cc: persist/save: /home/dybber/.accelerate/accelerate-cuda-0.13.0.0/cache/3.0/ZZzr157Izr243zr239zezrSOgzr218zrza1zr254szr183wzrzrD */
/* 1.68:cc: entry function 'fold' used 20 registers, 0 bytes smem, 0 bytes lmem, 0 bytes cmem */
/*      ... multiprocessor occupancy 100.0% : 2048 threads over 64 warps in 16 blocks */
/* 1.68:cc: waiting for nvcc... */
/* 1.68:cc: persist/save: /home/dybber/.accelerate/accelerate-cuda-0.13.0.0/cache/3.0/zr191zrDC4zrSUBZRzr162zr138zr200zr130zr205Tzr218zr193zrEOThzrSUBzr222 */
/* 1.68:cc: entry function 'map' used 11 registers, 0 bytes smem, 0 bytes lmem, 0 bytes cmem */
/*      ... multiprocessor occupancy 100.0% : 2048 threads over 64 warps in 16 blocks */
