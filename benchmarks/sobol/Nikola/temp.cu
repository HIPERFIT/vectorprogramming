#include "cuda.h"
#include "cuda_runtime_api.h"
#include <inttypes.h>
extern "C" __global__ void kernel0(double* vec_alloca_10,
                                   int32_t vec_alloca_1dim1, int32_t x_02)
{
    for (int32_t i_2 = blockIdx.x * blockDim.x + threadIdx.x; i_2 < 1; i_2 +=
         blockDim.x * gridDim.x) {
        int32_t x_74;
        uint32_t x_75;
        uint8_t x_76;
        
        x_74 = 0;
        x_75 = 0U;
        x_76 = 1;
        if (30 > 0) {
            for (int i3 = 0; i3 < 30; ++i3) {
                uint32_t v_11_7;
                uint8_t v_18_8;
                uint32_t ifte_result9;
                uint8_t ifte_result10;
                
                v_11_7 = (uint32_t) (int32_t) x_02;
                v_18_8 = ((int32_t) (int32_t) (v_11_7 ^ v_11_7 >> 1U) & 1 <<
                          x_74) != 0;
                if (v_18_8) {
                    uint32_t v_23_11;
                    uint32_t ifte_result12;
                    uint8_t ifte_result13;
                    
                    v_23_11 = 1U << 29U - (uint32_t) x_74;
                    if (x_76) {
                        ifte_result12 = v_23_11 ^ x_75;
                        ifte_result13 = v_18_8;
                    } else {
                        ifte_result12 = v_23_11;
                        ifte_result13 = v_18_8;
                    }
                    ifte_result9 = ifte_result12;
                    ifte_result10 = ifte_result13;
                } else {
                    ifte_result9 = x_75;
                    ifte_result10 = x_76;
                }
                x_74 = 1 + x_74;
                x_75 = ifte_result9;
                x_76 = ifte_result10;
            }
        }
        vec_alloca_10[i_2] = (double) (int32_t) x_75 / (double) (1 << 30);
    }
}