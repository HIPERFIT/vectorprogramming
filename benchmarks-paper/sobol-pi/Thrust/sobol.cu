#include "sobol.h"
#include <thrust/device_vector.h>

#define TESTBIT(var,pos) (((var) & (1<<(pos))) != 0)
#define k_2powneg32 2.3283064E-10F

// Inductively generates a Sobol number from a direction vector and
// the index in the sequence
__host__ __device__ 
float sobolInductive(unsigned int *dir, unsigned int i) {
  unsigned int acc = 0;
  for(int j = 0; j < n_directions; j++) {
    acc ^= TESTBIT(i,j) * dir[j];
  }
  return ((float)acc) * k_2powneg32;
}

// Generates a multidimensional Sobol-sequence where all numbers of
// the first dimension is placed first in the output array, followed
// by the second dimension and so on.
__global__ 
void cuda_sobolInductive(unsigned int *d_directions, float *d_output,
                         unsigned int n, unsigned int dimensions) {
  const int gridSize = __umul24(blockDim.x, gridDim.x);

  for (int i = __umul24(blockDim.x, blockIdx.x) + threadIdx.x; 
       i < n * dimensions;
       i += gridSize) {
    int dirvec_id = i / n;
    int sobol_id = i % n;
    d_output[i] = sobolInductive(&d_directions[dirvec_id*n_directions], sobol_id);
  }
}

// Thrust-interface to cuda_sobolInductive
void generate_sobol(thrust::device_vector<unsigned int> *d_directions,
                    thrust::device_vector<float> *d_sobol,
                    unsigned int n,
                    unsigned int dimensions) {
  // Convert to raw CUDA pointers
  unsigned int *d_directions_raw = thrust::raw_pointer_cast(&(*d_directions)[0]);
  float *d_sobol_raw = thrust::raw_pointer_cast(&(*d_sobol)[0]);

  // TODO: Make an informed choice of launch parameters
  int blockSize = 32;
  int gridSize = 480;
  cuda_sobolInductive<<<gridSize, blockSize>>>(d_directions_raw, d_sobol_raw, n, dimensions);
}
