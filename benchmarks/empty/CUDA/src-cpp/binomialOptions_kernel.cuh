/*
 * Copyright 1993-2012 NVIDIA Corporation.  All rights reserved.
 *
 * Please refer to the NVIDIA end user license agreement (EULA) associated
 * with this source code for terms and conditions that govern your use of
 * this software. Any use, reproduction, disclosure, or distribution of
 * this software and related documentation outside the terms of the EULA
 * is strictly prohibited.
 *
 */

////////////////////////////////////////////////////////////////////////////////
// Global types and parameters
////////////////////////////////////////////////////////////////////////////////
#include <stdio.h>
#include <stdlib.h>
#include <helper_cuda.h>
#include "realtype.h"
#include "binomialOptions_common.h"

#if defined(__CUDA_ARCH__) && (__CUDA_ARCH__ < 200)
 #define printf(f, ...) ((void)(f, __VA_ARGS__),0)
#endif


////////////////////////////////////////////////////////////////////////////////
// Internal GPU-side constants and data structures
////////////////////////////////////////////////////////////////////////////////
#define  TIME_STEPS 16
#define  CACHE_SIZE (1024)
#define CACHE_DELTA (2 * TIME_STEPS)
#define  CACHE_STEP (CACHE_SIZE - CACHE_DELTA)

//Preprocessed input option data
typedef struct
{
    double S_0;
    double strike;
    double u;
    double d;
    double R;
    double q;
    int  n;
} __TOptionData;
static __constant__ __TOptionData d_OptionData;
static __device__           double d_CallValue;
static __device__            double* d_CallBuffer;


////////////////////////////////////////////////////////////////////////////////
// Overloaded shortcut functions for different precision modes
////////////////////////////////////////////////////////////////////////////////
__device__ inline double expiryCallValue(double S0, double strike, double u, double d, int n, int i)
{
    return fmax(strike-S0 * pow(u,i) * pow(d,n-i),0);
}


////////////////////////////////////////////////////////////////////////////////
// GPU kernel
////////////////////////////////////////////////////////////////////////////////
static __global__ void binomialOptionsKernel()
{
    __shared__ double callA[CACHE_SIZE+1];
    __shared__ double callB[CACHE_SIZE+1];

    const int     tid = threadIdx.x;
    const double    S_0 = d_OptionData.S_0;
    const double strike = d_OptionData.strike;
    const double      u = d_OptionData.u;
    const double      d = d_OptionData.d;
    const double      q = d_OptionData.q;
    const double      R = d_OptionData.R;
    const int       n = d_OptionData.n;
    double *const d_Call = d_CallBuffer;

    //Compute values at expiry date
    for (int i = tid; i < n; i += CACHE_SIZE)
    {
        d_Call[i] = expiryCallValue(S_0, strike, u, d, n, i);
    }

    double S;
    int t;
    //Walk down binomial tree
    //So double-buffer and synchronize to avoid read-after-write hazards.
    // This loop is the start of the reduce primitive,
    // (each iteraniot is a vertical step of the reduction)
    for (int i = n; i > 0; i -= CACHE_DELTA)
    {
        // 'i' is the last index to be treated in the reduction
        // and also the 'time' at the start of the iteration.

        // Each iteration of this loop is a horizontal application
        // of the reduction.
        for (int c_base = 0; c_base < i; c_base += CACHE_STEP)
        {
            //Start and end positions within shared memory cache
            int c_start = min(CACHE_SIZE - 1, i - c_base);
            int c_end   = c_start - CACHE_DELTA;
            t = i;

            //Read data(with apron) to shared memory
            __syncthreads();

            if (tid <= c_start)
            {
                callA[tid] = d_Call[c_base + tid];
            }

            //Calculations within shared memory
            // Each step is an advancement inside the reduction primitive.
            for (int k = c_start - 1; k >= c_end;)
            {
                //Compute discounted expected value
                __syncthreads();
                S = S_0 * pow(u, c_base+tid) * pow(d, t-(c_base+tid)-1);
                callB[tid] = fmax(strike-S, (q * callA[tid + 1] + (1-q) * callA[tid])/R);
                t--;
                k--;

                //Compute discounted expected value
                //S = S_0 * powf(u, tid-1.0f) * powf(d, t-tid);
                __syncthreads();
                S = S_0 * pow(u, c_base+tid) * pow(d, t-(c_base+tid)-1);
                callA[tid] = fmax(strike-S, (q * callB[tid + 1] + (1-q) * callB[tid])/R);
                k--;
                t--;
            }
            
            //Flush shared memory cache
            __syncthreads();

            if (tid <= c_end)
            {
                d_Call[c_base + tid] = callA[tid];
            }
        }
    }

    //Write the value at the top of the tree to destination buffer
    if (tid == 0)
    {
      d_CallValue = callA[0];
    }
}



////////////////////////////////////////////////////////////////////////////////
// Host-side interface to GPU binomialOptions
////////////////////////////////////////////////////////////////////////////////
static void binomialOptionsGPU(
    double *callValue,
    TOptionData optionData
)
{
    __TOptionData h_OptionData;

    const int n = optionData.expiry * optionData.bankDays;

    const double dt = 1.0f / optionData.bankDays;
    const double alpha = optionData.alpha;
    const double sigma = optionData.sigma;
    const double u     = exp(alpha*dt+sigma*sqrt(dt));
    const double d     = exp(alpha*dt-sigma*sqrt(dt));
    const double R     = exp(dt*optionData.r);

    h_OptionData.S_0    = optionData.S_0;
    h_OptionData.strike = optionData.strike;
    h_OptionData.u      = u;
    h_OptionData.d      = d;
    h_OptionData.R      = R;
    h_OptionData.q      = (R-d)/(u-d);
    h_OptionData.n      = n;


    // Allocate result array
    void *d_CallBufferPtr;
    checkCudaErrors(cudaMalloc(&d_CallBufferPtr, n * sizeof(double)));
    checkCudaErrors(cudaMemcpyToSymbol(d_CallBuffer, &d_CallBufferPtr, sizeof(double*)));

    // Transmit work descriptions
    checkCudaErrors(cudaMemcpyToSymbol(d_OptionData, &h_OptionData, sizeof(__TOptionData)));

    // Run on one block
    binomialOptionsKernel<<<1, CACHE_SIZE>>>();
    getLastCudaError("binomialOptionsKernel() execution failed.\n");

    checkCudaErrors(cudaMemcpyFromSymbol(callValue, d_CallValue, sizeof(double)));
    checkCudaErrors(cudaFree(d_CallBufferPtr));

}
