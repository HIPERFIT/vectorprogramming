#include <iostream>
#include <stdexcept>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>

#include "cuda_runtime.h"
#include "binomial.h"



cudaError_t binomial(double* putValue, int32_t expiry)
{
    double* array_a = NULL;
    double* array_b = NULL;
    double* uPow = NULL;
    double* dPow = NULL;

    cudaError_t err;

    // Model parameters
    double S0=100; 
    double r=0.03; 
    double alpha=0.07; 
    double sigma=0.2; 

    double strike=100; 

    // We use a year length of 256, as 252 doesn't work well
    // with the binomial pricer from the CUDA SDK Samples library
    int32_t n=256*expiry;

    double dt=((double)expiry)/n;

    double u=exp(alpha*dt+sigma*sqrt(dt)); 
    double d=exp(alpha*dt-sigma*sqrt(dt)); 
    double R=exp(r*dt); 

    double q=(R-d)/(u-d);
    double qUR=q/R;
    double qDR =(1-q)/R;
    
    // Allocate memory
    err = cudaMalloc(&array_a, (n + 1) * sizeof(double));
    if (err != cudaSuccess)
        goto done;

    err = cudaMalloc(&array_b, (n + 1) * sizeof(double));
    if (err != cudaSuccess)
        goto done;

    err = cudaMalloc(&uPow, (n + 1) * sizeof(double));
    if (err != cudaSuccess)
        goto done;

    err = cudaMalloc(&dPow, (n + 1) * sizeof(double));
    if (err != cudaSuccess)
        goto done;

    finalPut(array_a, uPow, dPow, n, u, d, strike, S0);

    if (err != cudaSuccess)
        goto done;

    for(int32_t i = n-1; i >= 0; i-=2) {
      prevPut(array_a, uPow, dPow, i, array_b, qUR, qDR, S0, strike, u, d);
      if (err != cudaSuccess)
        goto done;

      prevPut(array_b, uPow, dPow, i-1, array_a, qUR, qDR, S0, strike, u, d);
      if (err != cudaSuccess)
        goto done;
    }

    err = cudaMemcpy(putValue, array_a, sizeof(double), cudaMemcpyDeviceToHost);

 done:
    cudaFree(array_a);
    cudaFree(array_b);
    return err;
}


int main(int argc, char *argv[])
{

    // needed to work correctly with piped benchmarkrunner
    setlinebuf(stdout);
    setlinebuf(stdin);

    int32_t expiry = 1;
    char inBuf[200]; // ridiculously large input buffer.
    
    bool isFirst = true;

  do {

    double        callValue;

    cudaError_t error = binomial(&callValue, expiry);

    if (error != cudaSuccess) {
      printf("CUDA error: %s\n", cudaGetErrorString(error));
      return -1;
    }


    if(!isFirst) {
      printf("RESULT %f\n", callValue);
    }
    else {
      printf("OK\n");
      isFirst = false;
    }

    fgets(inBuf, 200, stdin);

    if (sscanf(inBuf, "%u", &expiry) == 0) {
      // if input is not a number, it has to be "EXIT"
      if (strncmp("EXIT",inBuf,4)==0) {
        printf("OK\n");
        return 0;
      }
      else {
        printf("ERROR. Bad input: %s\n", inBuf);
        break;
      }
    }

  } while (true);

  cudaDeviceReset();
  return -1;
}
