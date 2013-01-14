#include "cuda.h"
#include "cuda_runtime_api.h"
#include <inttypes.h>

#include "binomial.h"

extern "C" __global__ void finalPut_kernel(double* output, double* uPow, double* dPow,
                                           int32_t n, double u, double d, double strike, double S0) {
 for (int32_t i = blockIdx.x * blockDim.x + threadIdx.x; i <= n;
      i += blockDim.x * gridDim.x) {
      
      double uPow_i = pow(u, (double)i);
      double dPow_i = pow(d, (double)(n-i));
      uPow[i] = uPow_i;
      dPow[i] = pow(d, (double)(i));
      output[i] = max(strike - S0 * uPow_i * dPow_i, 0.0);
    }
}

extern "C" __global__ void prevPut_kernel(double* last, double* uPow, double* dPow,
                                          int32_t i, double* output,
                                          double qUR, double qDR, double S0, double strike, double u, double d) {

    for (int32_t j = blockIdx.x * blockDim.x + threadIdx.x; j <= i;
         j += blockDim.x * gridDim.x) {

        double e = qUR * last[j+1] + qDR * last[j];

        output[j] = max(strike - S0 * uPow[j] * dPow[i-j], e);
    }
}

void finalPut(double* output, double* uPow, double* dPow,
              int32_t n, double u, double d, double strike, double S0) {
    dim3 gdims;
    dim3 tdims;
       
    gdims.x = 128;
    gdims.y = 1;
    gdims.z = 1;
    tdims.x = 480;
    tdims.y = 1;
    tdims.z = 1;
    
    finalPut_kernel<<<gdims, tdims>>>(output, uPow, dPow, n, u, d, strike, S0);
}

void prevPut(double* last, double* uPow, double* dPow,
             int32_t i, double* output,
             double qUR, double qDR, double S0, double strike, double u, double d) {
    dim3 gdims;
    dim3 tdims;
       
    gdims.x = 128;
    gdims.y = 1;
    gdims.z = 1;
    tdims.x = 480;
    tdims.y = 1;
    tdims.z = 1;
    
    prevPut_kernel<<<gdims, tdims>>>(last, uPow, dPow, i, output, qUR, qDR, S0, strike, u, d);
}
