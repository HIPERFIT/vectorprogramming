#ifndef COMPUTE_PI_H
#define COMPUTE_PI_H

#include <thrust/device_vector.h>

extern "C" float computepi(thrust::device_vector<float> *d_sobol, unsigned int n_vectors);

#endif
