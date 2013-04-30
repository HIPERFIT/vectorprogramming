#ifndef SOBOL_H
#define SOBOL_H

#include <thrust/device_vector.h>

#define n_directions 32

extern "C" __host__ __device__ float sobolInductive(unsigned int *dir, unsigned int i);

extern "C" void generate_sobol(thrust::device_vector<unsigned int> *d_directions,
                               thrust::device_vector<float> *d_sobol,
                               unsigned int n,
                               unsigned int dimensions);

#endif
