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
// #define  TIME_STEPS 16
#define  CACHE_SIZE (256)
// #define CACHE_DELTA (2 * TIME_STEPS)
// #define  CACHE_STEP (CACHE_SIZE - CACHE_DELTA)

// #if NUM_STEPS % CACHE_DELTA
// #error Bad constants
// #endif

//Preprocessed input option data
typedef struct
{
    real S_0;
    real strike;
    //real dt;
    real u;
    real d;
    real R;
    real q;
    int  n;
} __TOptionData;
static __constant__ __TOptionData d_OptionData[MAX_OPTIONS];
static __device__           float d_CallValue[MAX_OPTIONS];
//static __device__            real d_CallBuffer[MAX_OPTIONS * (NUM_STEPS + 16)];
static __device__            real* d_CallBuffer;
//static __device__            int NUM_STEPS;


////////////////////////////////////////////////////////////////////////////////
// Overloaded shortcut functions for different precision modes
////////////////////////////////////////////////////////////////////////////////
#ifndef DOUBLE_PRECISION
__device__ inline float expiryCallValue(real S0, real strike, real u, real d, int n, int i)
{
    return fmaxf(strike-S0 * powf(u,i) * powf(d,n-i),0);
    /*
    real d = S * expf(vDt * (2.0f * i - NUM_STEPS)) - X;
    return (d > 0) ? d : 0;
    */
}
#else
__device__ inline double expiryCallValue(double S0, double strike, double u, double d, int n, int i)
{
    return fmaxf(strike-S0 * powf(u,i) * powf(d,n-i),0);
    /*
    double d = S * exp(vDt * (2.0 * i - NUM_STEPS)) - X;
    return (d > 0) ? d : 0;
    */
}
#endif


// Just slice the input array into n bits, sync after each iteration.
/*
static __global__ void binomiaOptionsKernelNaive()
{

    const int     tid = threadIdx.x;
    const real    S_0 = d_OptionData[tid].S_0;
    const real strike = d_OptionData[tid].strike;
    const real      u = d_OptionData[tid].u;
    const real      d = d_OptionData[tid].d;
    const real      q = d_OptionData[tid].q;
    const real      R = d_OptionData[tid].R;
    const int       n = d_OptionData[tid].n;
    real *const d_Call = &d_CallBuffer[tid * (n + 16)];

    // TODO...
}
*/

////////////////////////////////////////////////////////////////////////////////
// GPU kernel
////////////////////////////////////////////////////////////////////////////////
static __global__ void binomialOptionsKernel()
{
    __shared__ real callA[CACHE_SIZE+1];
    __shared__ real callB[CACHE_SIZE+1];
    //Global memory frame for current option (thread block)
    //const int NUM_STEPS   = d_OptionData[blockIdx.x].n;
    const int TIME_STEPS  = 16; // what exactly is TIME_STEPS?
    const int CACHE_DELTA = (2 * TIME_STEPS);
    const int CACHE_STEP  = (CACHE_SIZE - CACHE_DELTA);


    // #if NUM_STEPS % CACHE_DELTA ==> bad constants

    const int     tid = threadIdx.x;
    const int     bid = blockIdx.x;
    const real    S_0 = d_OptionData[bid].S_0;
    const real strike = d_OptionData[bid].strike;
    //const real     dt = d_OptionData[bid].dt;
    const real      u = d_OptionData[bid].u;
    const real      d = d_OptionData[bid].d;
    const real      q = d_OptionData[bid].q;
    const real      R = d_OptionData[bid].R;
    const int       n = d_OptionData[bid].n;
    real *const d_Call = &d_CallBuffer[tid * (n + 16)];

    //Compute values at expiry date
    for (int i = tid; i <= n; i += CACHE_SIZE)
    {
        d_Call[i] = expiryCallValue(S_0, strike, u, d, n, i);
    }

    real S;
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
                S = S_0 * powf(u, c_base+tid) * powf(d, t-(c_base+tid)-1);
                callB[tid] = fmaxf(strike-S, (q * callA[tid + 1] + (1-q) * callA[tid])/R);
                t--;
                k--;

                //Compute discounted expected value
                //S = S_0 * powf(u, tid-1.0f) * powf(d, t-tid);
                __syncthreads();
                S = S_0 * powf(u, c_base+tid) * powf(d, t-(c_base+tid)-1);
                callA[tid] = fmaxf(strike-S, (q * callB[tid + 1] + (1-q) * callB[tid])/R);
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
    if (threadIdx.x == 0)
    {
        d_CallValue[blockIdx.x] = (float)callA[0];
    }
}



////////////////////////////////////////////////////////////////////////////////
// Host-side interface to GPU binomialOptions
////////////////////////////////////////////////////////////////////////////////
static void binomialOptionsGPU(
    float *callValue,
    TOptionData *optionData,
    int optN
)
{
    __TOptionData h_OptionData[MAX_OPTIONS];

    const int n = optionData[0].expiry * optionData[0].bankDays;
    for (int i = 0; i < optN; i++)
    {
       const double dt = 1.0f / optionData[i].bankDays;
       const double alpha = optionData[i].alpha;
       const double sigma = optionData[i].sigma;
       const double u     = exp(alpha*dt+sigma*sqrt(dt));
       const double d     = exp(alpha*dt-sigma*sqrt(dt));
       const double R     = exp(dt*optionData[i].r);

       h_OptionData[i].S_0    = optionData[i].S_0;
       h_OptionData[i].strike = optionData[i].strike;
       h_OptionData[i].u      = u;
       h_OptionData[i].d      = d;
       h_OptionData[i].R      = R;
       h_OptionData[i].q      = (R-d)/(u-d);
       h_OptionData[i].n      = n;
    }

    /*
printf("h_OptionData[i].S_0 = %f\n"
"h_OptionData[0].strike = %f\n"
"h_OptionData[0].u      = %f\n"
"h_OptionData[0].d      = %f\n"
"h_OptionData[0].R      = %f\n"
"h_OptionData[0].q      = %f\n"
"h_OptionData[0].n      = %d\n",
h_OptionData[0].S_0    ,
h_OptionData[0].strike ,
h_OptionData[0].u      ,
h_OptionData[0].d      ,
h_OptionData[0].R      ,
h_OptionData[0].q      ,
h_OptionData[0].n      );
*/

    // Allocate result array
    void *d_CallBufferPtr;
    checkCudaErrors(cudaMalloc(&d_CallBufferPtr, MAX_OPTIONS * (n + 16) * sizeof(real)));
    checkCudaErrors(cudaMemcpyToSymbol(d_CallBuffer, &d_CallBufferPtr, sizeof(real*)));

    // Transmit work descriptions
    checkCudaErrors(cudaMemcpyToSymbol(d_OptionData, h_OptionData, optN * sizeof(__TOptionData)));

    binomialOptionsKernel<<<optN, CACHE_SIZE>>>();
    getLastCudaError("binomialOptionsKernel() execution failed.\n");
    checkCudaErrors(cudaMemcpyFromSymbol(callValue, d_CallValue, optN *sizeof(float)));
    checkCudaErrors(cudaFree(d_CallBufferPtr));

}
