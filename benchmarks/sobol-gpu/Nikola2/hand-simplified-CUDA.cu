#include "cuda.h"
#include "cuda_runtime_api.h"
#include <inttypes.h>
extern "C" __global__ void kernel0(int32_t* input, int32_t input_length,
                                   double* output,
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

        output[j] = (double) (int32_t) xa / (double) (1 << 30);
    }
}
