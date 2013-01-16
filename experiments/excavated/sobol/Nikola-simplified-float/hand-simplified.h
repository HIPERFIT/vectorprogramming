#ifndef SOBOL_GPU_NIKOLA_UNSIMPLIFIED_H
#define SOBOL_GPU_NIKOLA_UNSIMPLIFIED_H

#include <inttypes.h>

extern "C" cudaError_t sobol_nikola_unsimplified(int32_t n_vectors, int32_t* indices, int32_t n_indices, float** sobol_sequence, int32_t* n_vec);

#endif
