#include <iostream>
#include <stdexcept>
#include <math.h>
#include <stdio.h>

#include "sobol.h"
#include <thrust/device_vector.h>
#include <thrust/generate.h>

#define TESTBIT(var,pos) (((var) & (1<<(pos))) != 0)
#define k_2powneg32 2.3283064E-10F

// Inductively generates a Sobol number from a direction vector and
// the index in the sequence
__host__ __device__
float sobolInductive(const unsigned int *dir, unsigned int i) {
  unsigned int acc = 0;
  for(int j = 0; j < n_directions; j++) {
    acc ^= TESTBIT(i,j) * dir[j];
  }
  return ((float)acc) * k_2powneg32;
}

struct sobolInductiveFunctor {
private:
  const int n;
  const unsigned int *d_directions_raw;

public:
  sobolInductiveFunctor(unsigned int _n, unsigned int* _d_directions_raw)
    : n(_n), d_directions_raw(_d_directions_raw)  {}

  __host__ __device__
  float operator()(const int& i) const {
    int dirvec_id = i / n;
    int sobol_id = i % n;
    return sobolInductive(&d_directions_raw[dirvec_id*n_directions], sobol_id);
  }
};

void generate_sobol(thrust::device_vector<unsigned int> *d_directions,
                    thrust::device_vector<float> *d_sobol,
                    unsigned int n,
                    unsigned int dimensions) {

  // Convert to raw CUDA pointer
  unsigned int *d_directions_raw = thrust::raw_pointer_cast(&(*d_directions)[0]);
  thrust::counting_iterator<int> iter(0);

  // Should be a transform iterator
  thrust::transform(iter,
                    iter + n*dimensions,
                    d_sobol->begin(),
                    sobolInductiveFunctor(n, d_directions_raw));
}
