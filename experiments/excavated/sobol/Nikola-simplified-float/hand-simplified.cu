#include "cuda.h"
#include "cuda_runtime_api.h"
#include <inttypes.h>

#include "hand-simplified.h"

extern "C" __global__ void kernel0(int32_t* input, int32_t input_length,
                                   float* output,
                                   int32_t output_length, int32_t output_size)
{
    for (int32_t j = blockIdx.x * blockDim.x + threadIdx.x;
         j < output_size;
         j += blockDim.x * gridDim.x) {
        uint32_t xa = 0U;
        uint8_t lastTestBit = 1;
  
        for (int i = 0; i < 30; ++i) {
            uint32_t n;
            uint8_t testBit;

            // j % (x_04 > (input_length > x_04 ? x_04 : input_length) ? input_length > x_04 ? x_04 : input_length : x_04)
            // (u > (t > u ? u : t) ? (t > u ? u : t) : u)
            // <=>
            // (u > min(u,t) ? min(u,t) : u)
            // <=>
            // min(u,t)
            n = (uint32_t) input[j % min(input_length, output_size)]; // this should be hoisted out of the loop (does nvcc do it?)

            uint32_t grayCode = n ^ (n >> 1U);
            testBit = (grayCode & 1 << i) != 0;

            if (testBit) {
                uint32_t v;
                v = 1U << 29U - (uint32_t) i; // direction numbers (these should have been arguments to the kernel)
                if (lastTestBit) {
                    xa = v ^ xa;
                } else {
                    xa = v;
                }
                lastTestBit = testBit;
            }
        }

        output[j] = (float) (int32_t) xa / (double) (1 << 30);
    }
}
void gc(void** allocs, int* marks, int nallocs)
{
    for (int i = 0; i < nallocs; ++i) {
        if (marks[i] == 0) {
            cudaFree((char*) allocs[i]);
            allocs[i] = NULL;
        }
        marks[i] = 0;
    }
}
void mark(void** allocs, int* marks, int nallocs, void* alloc)
{
    for (int i = 0; i < nallocs; ++i) {
        if (allocs[i] == alloc) {
            marks[i] = 1;
            return;
        }
    }
}
cudaError_t sobol_nikola_unsimplified(int32_t x_01, int32_t* arr_12, int32_t arr_1dim3,
                  float** ptr_resultparam22, int32_t* scalar_resultparam23)
{
    void* allocs[1];
    int marks[1];
    int nallocs = 0;
    float* alloc4 = NULL;
    
    if (cudaMalloc(&alloc4, x_01 * sizeof(float)) != cudaSuccess)
        goto done;
    
    allocs[nallocs] = (void*) alloc4;
    marks[nallocs++] = 0;
    {
        dim3 gdims;
        dim3 tdims;
        
        gdims.x = 128;
        gdims.y = 1;
        gdims.z = 1;
        tdims.x = 480;
        tdims.y = 1;
        tdims.z = 1;
        kernel0<<<gdims, tdims>>>(arr_12, arr_1dim3, alloc4, x_01, x_01);
    }
    mark(allocs, marks, nallocs, alloc4);
    *ptr_resultparam22 = alloc4;
    *scalar_resultparam23 = x_01;
    
  done:
    gc(allocs, marks, nallocs);
    return cudaGetLastError();
}
