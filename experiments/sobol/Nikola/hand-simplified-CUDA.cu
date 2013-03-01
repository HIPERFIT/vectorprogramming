#include "cuda.h"
#include "cuda_runtime_api.h"
#include <inttypes.h>
extern "C" __global__ void kernel0(double* output, uint32_t n)
{
    for (int32_t ix = blockIdx.x * blockDim.x + threadIdx.x;
         ix < 1;
         ix += blockDim.x * gridDim.x) {

        int32_t j;
        uint32_t xa;
        uint8_t lastTestBit;
        
        xa = 0U;
        lastTestBit = 1;

        for (int i = 0; i < 30; ++i) {
            uint32_t grayCode = n ^ n >> 1U;
            uint8_t testBit = (grayCode & 1 << i) != 0;
            if (testBit) {
                uint32_t v;
                v = 1U << 29U - (uint32_t) i; // sobol_dirVs
                if (lastTestBit) {
                    xa = v ^ xa;
                }
                lastTestBit = testBit;
            }
        }
        output[ix] = (double) (int32_t) xa / (double) (1 << 30);
    }
}
