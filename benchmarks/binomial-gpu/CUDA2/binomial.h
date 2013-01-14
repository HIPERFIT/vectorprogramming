#ifndef BINOMIAL_H
#define BINOMIAL_H

#include <inttypes.h>

// extern "C" cudaError_t binomial(double* putValue, int32_t expiry);

extern "C" void finalPut(double* output, double* uPow, double* dPow,
                         int32_t n, double u, double d, double strike, double S0);

extern "C" void prevPut(double* last, double* uPow, double* dPow,
                        int32_t i, double* output,
                        double qUR, double qDR, double S0, double strike, double u, double d);

#endif
