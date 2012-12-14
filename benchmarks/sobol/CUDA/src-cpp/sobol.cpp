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
* Portions Copyright (c) 1993-2012 NVIDIA Corporation.  All rights reserved.
* Portions Copyright (c) 2009 Mike Giles, Oxford University.  All rights reserved.
* Portions Copyright (c) 2008 Frances Y. Kuo and Stephen Joe.  All rights reserved.
*
* Sobol Quasi-random Number Generator example
*
* Based on CUDA code submitted by Mike Giles, Oxford University, United Kingdom
* http://people.maths.ox.ac.uk/~gilesm/
*
* and C code developed by Stephen Joe, University of Waikato, New Zealand
* and Frances Kuo, University of New South Wales, Australia
* http://web.maths.unsw.edu.au/~fkuo/sobol/
*
* For theoretical background see:
*
* P. Bratley and B.L. Fox.
* Implementing Sobol's quasirandom sequence generator
* http://portal.acm.org/citation.cfm?id=42288
* ACM Trans. on Math. Software, 14(1):88-100, 1988
*
* S. Joe and F. Kuo.
* Remark on algorithm 659: implementing Sobol's quasirandom sequence generator.
* http://portal.acm.org/citation.cfm?id=641879
* ACM Trans. on Math. Software, 29(1):49-57, 2003
*/

#include <iostream>

#include <cuda_runtime.h>      // CUDA Runtime Functions
#include <helper_cuda.h>       // helper functions for CUDA error checking and initialization
#include <helper_functions.h>  // helper functions 

#include <stdexcept>
#include <math.h>

#include "sobol.h"
#include "sobol_gold.h"
#include "sobol_gpu.h"

#define L1ERROR_TOLERANCE (1e-6)

int main(int argc, char *argv[])
{
    bool ok = true;

    // We will generate n_vectors vectors of n_dimensions numbers
    int n_vectors;
    int n_dimensions = 1;

    // Use command-line specified CUDA device, otherwise use device with highest Gflops/s
    int devID = findCudaDevice(argc, (const char **)argv);
    
    if (devID < 0)
    {
        printf("No CUDA Capable devices found, exiting...\n");
    }
    
    checkCudaErrors(cudaGetDevice(&devID));

    char inBuf[200]; // ridiculously large input buffer.
    printf("OK\n");

    while (true) {

      fgets(inBuf, 200, stdin);

      if (sscanf(inBuf, "%u", &n_vectors) == 0)
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

    // Allocate memory for the arrays
    unsigned int *h_directions = 0;
    float        *h_outputGPU  = 0;

    try
    {
        h_directions = new unsigned int [n_dimensions * n_directions];
        h_outputGPU  = new float [n_vectors * n_dimensions];
    }
    catch (std::exception e)
    {
        std::cerr << "Caught exception: " << e.what() << std::endl;
        std::cerr << "Unable to allocate CPU memory (try running with fewer vectors/dimensions)" << std::endl;
        exit(EXIT_FAILURE);
    }

    unsigned int *d_directions;
    float        *d_output;

    try
    {
        cudaError_t cudaResult;
        cudaResult = cudaMalloc((void **)&d_directions, n_dimensions * n_directions * sizeof(unsigned int));

        if (cudaResult != cudaSuccess)
        {
            throw std::runtime_error(cudaGetErrorString(cudaResult));
        }

        cudaResult = cudaMalloc((void **)&d_output, n_vectors * n_dimensions * sizeof(float));

        if (cudaResult != cudaSuccess)
        {
            throw std::runtime_error(cudaGetErrorString(cudaResult));
        }
    }
    catch (std::runtime_error e)
    {
        std::cerr << "Caught exception: " << e.what() << std::endl;
        std::cerr << "Unable to allocate GPU memory (try running with fewer vectors/dimensions)" << std::endl;
        exit(EXIT_FAILURE);
    }

    // Initialize the direction numbers (done on the host)
    initSobolDirectionVectors(n_dimensions, h_directions);

    // Copy the direction numbers to the device
    checkCudaErrors(cudaMemcpy(d_directions, h_directions, n_dimensions * n_directions * sizeof(unsigned int), cudaMemcpyHostToDevice));
    checkCudaErrors(cudaDeviceSynchronize());

    // Execute the QRNG on the device
    sobolGPU(n_vectors, n_dimensions, d_directions, d_output);
    checkCudaErrors(cudaDeviceSynchronize());


    checkCudaErrors(cudaMemcpy(h_outputGPU, d_output, n_vectors * n_dimensions * sizeof(float), cudaMemcpyDeviceToHost));

    // Cleanup and terminate
    delete h_directions;
    checkCudaErrors(cudaFree(d_directions));
    checkCudaErrors(cudaFree(d_output));

    printf("RESULT ");

    for(int i = 0; i < 10; i++)
      printf("%f ", h_outputGPU[i]);

    printf("\n");

    delete h_outputGPU;

    }

    cudaDeviceReset();
    exit(EXIT_SUCCESS);
}
