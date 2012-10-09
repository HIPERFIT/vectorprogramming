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

/*
 * This sample evaluates fair call price for a
 * given set of European options under binomial model.
 * See supplied whitepaper for more explanations.
 */



#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <cuda_runtime.h>

#include <helper_functions.h>
#include <helper_cuda.h>

// #include <shrQATest.h>
#include "binomialOptions_common.h"


////////////////////////////////////////////////////////////////////////////////
// Black-Scholes formula for binomial tree results validation
////////////////////////////////////////////////////////////////////////////////
/*
extern "C" void BlackScholesCall(
    float &callResult,
    TOptionData optionData
);
*/

////////////////////////////////////////////////////////////////////////////////
// Process single option on CPU
// Note that CPU code is for correctness testing only and not for benchmarking.
////////////////////////////////////////////////////////////////////////////////
/*
extern "C" void binomialOptionsCPU(
    float &callResult,
    TOptionData optionData
);

*/

////////////////////////////////////////////////////////////////////////////////
// Process an array of OptN options on GPU
////////////////////////////////////////////////////////////////////////////////
extern "C" void binomialOptions_SM10(
    float *callValue,
    TOptionData  *optionData,
    int optN, int NUM_STEPS
);

extern "C" void binomialOptions_SM13(
    float *callValue,
    TOptionData  *optionData,
    int optN, int NUM_STEPS
);


////////////////////////////////////////////////////////////////////////////////
// Helper function, returning uniformly distributed
// random float in [low, high] range
////////////////////////////////////////////////////////////////////////////////
float randData(float low, float high)
{
    float t = (float)rand() / (float)RAND_MAX;
    return (1.0f - t) * low + t * high;
}



////////////////////////////////////////////////////////////////////////////////
// Main program
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char **argv)
{
    const unsigned int OPT_N_MAX = 512;
    unsigned int useDoublePrecision;

    //shrQAStart(argc, argv);
    setlinebuf(stdout);

    int devID = findCudaDevice(argc, (const char **)argv);

    if (devID < 0)
    {
        printf("No CUDA Capable devices found, exiting...\n");
        //shrQAFinishExit(argc, (const char **)argv, QA_WAIVED);
    }

    checkCudaErrors(cudaGetDevice(&devID));
    cudaDeviceProp deviceProp;
    checkCudaErrors(cudaGetDeviceProperties(&deviceProp, devID));

    char *precisionChoice;
    getCmdLineArgumentString(argc, (const char **)argv, "type", &precisionChoice);

    if (precisionChoice == NULL)
    {
        useDoublePrecision = 0;
    }
    else
    {
        if (!STRCASECMP(precisionChoice, "double"))
        {
            useDoublePrecision = 1;
        }
        else
        {
            useDoublePrecision = 0;
        }
    }

    //printf(useDoublePrecision ? "Using double precision...\n" : "Using single precision...\n");
    // const int OPT_N = OPT_N_MAX;
    // Only do a single option
    const int OPT_N = 1;

    TOptionData optionData[OPT_N];
    /*
    TOptionData optionData[OPT_N_MAX];
    float
    callValueBS[OPT_N_MAX],
                callValueGPU[OPT_N_MAX],
                callValueCPU[OPT_N_MAX];

    double
    sumDelta, sumRef, gpuTime, errorVal;

    StopWatchInterface *hTimer = NULL;
    */    
    double errorVal;
    float callValueGPU[OPT_N_MAX];
    int i;

    //sdkCreateTimer(&hTimer);

    int version = deviceProp.major * 10 + deviceProp.minor;

    if (useDoublePrecision && version < 13)
    {
        printf("Double precision is not supported.\n");
        return 0;
    }

    //printf("Generating input data...\n");
    //Generate options set
    srand(123);

    char inBuf[200]; // ridiculously large input buffer.
    int expiry = 0;
    int bankDays = 252;
    printf("OK\n");
    while (true) {

      fgets(inBuf, 200, stdin);
      
      if (sscanf(inBuf, "%u", &expiry) == 0) 
      {
        // if input is not a number, it has to be "EXIT"
        if (strncmp("EXIT",inBuf,4)==0) 
        {
          printf("OK\n");
          break;
        }
        else
        {
          printf("ERROR. Bad input: %s\n", inBuf);
          break;
        }
      }
      
      /* model parameters from the Vector version in Haskell: 
      strike = 100
      bankDays = 252
      s0 = 100
      r = 0.03; alpha = 0.07; sigma = 0.20

      n = expiry*bankDays
      dt = fromIntegral expiry/fromIntegral n
      u = exp(alpha*dt+sigma*sqrt dt)
      d = exp(alpha*dt-sigma*sqrt dt)
      stepR = exp(r*dt)
      q = (stepR-d)/(u-d)
      qUR = q/stepR; qDR = (1-q)/stepR
      */

      int NUM_STEPS = expiry * bankDays;

      for (i = 0; i < OPT_N; i++)
      {
          optionData[i].S = 100.0f; // randData(5.0f, 30.0f);
          optionData[i].X = 100.0f; // randData(1.0f, 100.0f);
          optionData[i].T = (double) expiry;
          optionData[i].R = 0.03f;
          optionData[i].V = 0.10f;
          //optionData[i].n = (int) bankDays * expiry;
          //BlackScholesCall(callValueBS[i], optionData[i]);
      }

      //printf("Running GPU binomial tree...\n");
      checkCudaErrors(cudaDeviceSynchronize());
      //sdkResetTimer(&hTimer);
      //sdkStartTimer(&hTimer);

      if (useDoublePrecision)
      {
          binomialOptions_SM13(callValueGPU, optionData, OPT_N, NUM_STEPS);
      }
      else
      {
          binomialOptions_SM10(callValueGPU, optionData, OPT_N, NUM_STEPS);
      }


      checkCudaErrors(cudaDeviceSynchronize());
      
      printf("RESULT %f\n",fabs(callValueGPU[0]));

      /*
      sdkStopTimer(&hTimer);
      gpuTime = sdkGetTimerValue(&hTimer);
      printf("Options count            : %i     \n", OPT_N);
      printf("Time steps               : %i     \n", NUM_STEPS);
      printf("binomialOptionsGPU() time: %f msec\n", gpuTime);
      printf("Options per second       : %f     \n", OPT_N / (gpuTime * 0.001));

      printf("Running CPU binomial tree...\n");

      for (i = 0; i < OPT_N; i++)
      {
          binomialOptionsCPU(callValueCPU[i], optionData[i]);
      }

      printf("Comparing the results...\n");
      sumDelta = 0;
      sumRef   = 0;
      printf("GPU binomial vs. Black-Scholes\n");

      for (i = 0; i < OPT_N; i++)
      {
          sumDelta += fabs(callValueBS[i] - callValueGPU[i]);
          sumRef += fabs(callValueBS[i]);
      }

      if (sumRef >1E-5)
      {
          printf("L1 norm: %E\n", sumDelta / sumRef);
      }
      else
      {
          printf("Avg. diff: %E\n", sumDelta / (double)OPT_N);
      }

      printf("CPU binomial vs. Black-Scholes\n");
      sumDelta = 0;
      sumRef   = 0;

      for (i = 0; i < OPT_N; i++)
      {
          sumDelta += fabs(callValueBS[i]- callValueCPU[i]);
          sumRef += fabs(callValueBS[i]);
      }

      if (sumRef >1E-5)
      {
          printf("L1 norm: %E\n", sumDelta / sumRef);
      }
      else
      {
          printf("Avg. diff: %E\n", sumDelta / (double)OPT_N);
      }

      printf("CPU binomial vs. GPU binomial\n");
      sumDelta = 0;
      sumRef   = 0;

      for (i = 0; i < OPT_N; i++)
      {
          sumDelta += fabs(callValueGPU[i] - callValueCPU[i]);
          sumRef += callValueCPU[i];
      }

      if (sumRef > 1E-5)
      {
          printf("L1 norm: %E\n", errorVal = sumDelta / sumRef);
      }
      else
      {
          printf("Avg. diff: %E\n", errorVal = sumDelta / (double)OPT_N);
      }

      printf("Shutting down...\n");

      sdkDeleteTimer(&hTimer);
      */

    }

    cudaDeviceReset();
    //shrQAFinishExit(argc, (const char **)argv, ((errorVal < 5e-4) ? QA_PASSED : QA_FAILED));
}
