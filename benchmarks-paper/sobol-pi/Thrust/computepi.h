#ifndef COMPUTE_PI_H
#define COMPUTE_PI_H

#include <thrust/device_vector.h>

extern "C"
float computepi(thrust::device_vector<unsigned int> *d_directions,
                unsigned int n,
                unsigned int dimensions);

#endif
