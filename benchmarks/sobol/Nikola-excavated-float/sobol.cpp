#include <iostream>
#include <stdexcept>
#include <math.h>
#include <stdio.h>
#include <string.h>

#include <cuda_runtime.h>      // CUDA Runtime Functions

#define L1ERROR_TOLERANCE (1e-6)

#include "nikola-unsimplified.h"

int main(int argc, char *argv[])
{
    // needed to work correctly with piped benchmarkrunner
    setlinebuf(stdout);
    setlinebuf(stdin);

    int n_indices = 1;
    int n_dimensions = 1;
    char inBuf[200]; // ridiculously large input buffer.
    
    bool isFirst = true;

  do {

    // Allocate memory for the arrays
    int *h_indices = 0;
    float        *h_outputGPU  = 0;

    try
    {
        h_indices = new int [n_indices * n_dimensions];
        h_outputGPU  = new float [n_indices * n_dimensions];
    }
    catch (std::exception e)
    {
        std::cerr << "Caught exception: " << e.what() << std::endl;
        std::cerr << "Unable to allocate CPU memory (try running with fewer vectors/dimensions)" << std::endl;
        return -1;
    }

    int *d_indices;
    float        *d_output;

    try
    {
        cudaError_t cudaResult;
        cudaResult = cudaMalloc((void **)&d_indices, n_dimensions * n_indices * sizeof(int));

        if (cudaResult != cudaSuccess)
        {
            throw std::runtime_error(cudaGetErrorString(cudaResult));
        }
    }
    catch (std::runtime_error e)
    {
        std::cerr << "Caught exception: " << e.what() << std::endl;
        std::cerr << "Unable to allocate GPU memory (try running with fewer vectors/dimensions)" << std::endl;
        return -1;
    }

    // Initialize the indices (done on the host)
    for(int i = 0; i < n_indices; i++) {
      h_indices[i] = i;
    }

    // Copy the indices to the device
    cudaMemcpy(d_indices, h_indices, n_dimensions * n_indices * sizeof(int), cudaMemcpyHostToDevice);
    cudaDeviceSynchronize();

    // Execute the QRNG on the device
    int n_vec;
    sobol_nikola_unsimplified(n_indices, d_indices, n_indices, &d_output, &n_vec);

    cudaDeviceSynchronize();

    cudaMemcpy(h_outputGPU, d_output, n_indices * n_dimensions * sizeof(float), cudaMemcpyDeviceToHost);

    // Cleanup and terminate
    delete h_indices;
    cudaFree(d_indices);
    cudaFree(d_output);

    if(!isFirst) {
      printf("RESULT ");

      for(int i = 0; i < std::min(n_indices,10); i++)
        printf("%f ", h_outputGPU[i]);

      printf("\n");
    }
    else {
      printf("OK\n");
      isFirst = false;
    }

    delete h_outputGPU;

      fgets(inBuf, 200, stdin);

      if (sscanf(inBuf, "%u", &n_indices) == 0)
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

    } while (true);

    cudaDeviceReset();
    return -1;
}
