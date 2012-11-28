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


#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <cuda_runtime.h>

#include <helper_functions.h>
#include <helper_cuda.h>

#include "binomialOptions_common.h"


////////////////////////////////////////////////////////////////////////////////
// Process an array of OptN options on GPU
////////////////////////////////////////////////////////////////////////////////
extern "C" void binomialOptions_SM13(
    double *callValue,
    TOptionData  optionData
);


////////////////////////////////////////////////////////////////////////////////
// Main program
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char **argv)
{

    setlinebuf(stdout);

    int devID = findCudaDevice(argc, (const char **)argv);

    if (devID < 0)
    {
        printf("No CUDA Capable devices found, exiting...\n");
    }

    checkCudaErrors(cudaGetDevice(&devID));

    cudaDeviceProp deviceProp;
    checkCudaErrors(cudaGetDeviceProperties(&deviceProp, devID));

    TOptionData optionData;

    double errorVal;
    double callValueGPU;
    int i;

    char inBuf[200]; // ridiculously large input buffer.
    int expiry = 0;
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

      optionData.S_0 = 100.0f; // randData(5.0f, 30.0f);
      optionData.strike = 100.0f; // randData(1.0f, 100.0f);
      optionData.r = 0.03f;
      optionData.bankDays = 256;
      optionData.expiry = expiry;
      optionData.sigma = 0.20f;
      optionData.alpha = 0.07f;

      checkCudaErrors(cudaDeviceSynchronize());

      binomialOptions_SM13(&callValueGPU, optionData);

      checkCudaErrors(cudaDeviceSynchronize());

      printf("RESULT %f\n",callValueGPU);
    }

    cudaDeviceReset();
}
