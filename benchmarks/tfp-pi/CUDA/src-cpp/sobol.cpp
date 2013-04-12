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
#include <stdio.h>

#include "sobol.h"
#include "sobol_gold.h"
#include "sobol_gpu.h"
#include "computepi.h"

#define L1ERROR_TOLERANCE (1e-6)

// direction vector of two dimensions, 32 directions
unsigned int h_directions[] = {2147483648,1073741824,2684354560,1342177280,2281701376,603979776,301989888,754974720,1988100096,2654994432,136314880,1678770176,2988965888,2098462720,4272029696,3125346304,438599680,1226522624,3300237312,3816001536,4135585792,3728737280,2820672000,873465088,975702144,1494483520,3970040096,2538144464,1822721896,3613084132,3432358018,2271450689,2147483648,1073741824,3758096384,2952790016,2550136832,2483027968,2315255808,1526726656,864026624,3653238784,1914699776,1058013184,3250061312,2800484352,1401290752,703922176,171606016,455786496,3549618176,1778348032,3929540608,2871788544,1269173760,4259646208,1610779008,4026976576,2016733344,605713840,305826616,3475687836,3113412898,2197780721};

int main(int argc, char *argv[])
{

    // needed to work correctly with piped benchmarkrunner
    setlinebuf(stdout);
    setlinebuf(stdin);

    bool ok = true;

    // We will generate n_vectors vectors of n_dimensions numbers
    int n_vectors;
    int n_dimensions = 2;

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
    float  h_outputGPU  = 0;

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

    // Copy the direction numbers to the device
    checkCudaErrors(cudaMemcpy(d_directions, h_directions, n_dimensions * n_directions * sizeof(unsigned int), cudaMemcpyHostToDevice));
    checkCudaErrors(cudaDeviceSynchronize());

    // Execute the QRNG on the device
    sobolGPU(n_vectors, n_dimensions, d_directions, d_output);
    checkCudaErrors(cudaDeviceSynchronize());

    float pi = computepi(d_output, n_vectors);

    //    checkCudaErrors(cudaMemcpy(&h_outputGPU, d_output, sizeof(float), cudaMemcpyDeviceToHost));

    printf("RESULT %f\n", pi);

    // Cleanup and terminate
    checkCudaErrors(cudaFree(d_directions));
    checkCudaErrors(cudaFree(d_output));

    }
    cudaDeviceReset();
    exit(EXIT_SUCCESS);
}
