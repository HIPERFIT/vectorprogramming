#include "cuda.h"
#include "cuda_runtime_api.h"
#include <inttypes.h>

#include "nikola-unsimplified.h"

extern "C" __global__ void kern5(int32_t* arr_16, int32_t arr_1dim7,
                                 float* vec_alloca_28,
                                 int32_t vec_alloca_2dim9, int32_t x_010)
{
    for (int32_t i_3 = blockIdx.x * blockDim.x + threadIdx.x; i_3 < x_010;
         i_3 += blockDim.x * gridDim.x) {
        int32_t x_812;
        uint32_t x_813;
        uint8_t x_814;
        
        x_812 = 0;
        x_813 = 0U;
        x_814 = 1;
        if (30 > 0) {
            for (int i11 = 0; i11 < 30; ++i11) {
                uint32_t v_126_15;
                uint8_t v_32_16;
                uint32_t ifte_result17;
                uint8_t ifte_result18;
                
                v_126_15 = (uint32_t) (int32_t) arr_16[i_3 % (x_010 >
                                                              (arr_1dim7 >
                                                               x_010 ? x_010 : arr_1dim7) ? arr_1dim7 >
                                                              x_010 ? x_010 : arr_1dim7 : x_010)];
                v_32_16 = ((int32_t) (int32_t) (v_126_15 ^ v_126_15 >> 1U) &
                           1 << x_812) != 0;
                if (v_32_16) {
                    uint32_t v_37_19;
                    uint32_t ifte_result20;
                    uint8_t ifte_result21;
                    
                    v_37_19 = 1U << 29U - (uint32_t) x_812;
                    if (x_814) {
                        ifte_result20 = v_37_19 ^ x_813;
                        ifte_result21 = v_32_16;
                    } else {
                        ifte_result20 = v_37_19;
                        ifte_result21 = v_32_16;
                    }
                    ifte_result17 = ifte_result20;
                    ifte_result18 = ifte_result21;
                } else {
                    ifte_result17 = x_813;
                    ifte_result18 = x_814;
                }
                x_812 = 1 + x_812;
                x_813 = ifte_result17;
                x_814 = ifte_result18;
            }
        }
        vec_alloca_28[i_3] = (float) (int32_t) x_813 / (double) (1 << 30);
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
        kern5<<<gdims, tdims>>>(arr_12, arr_1dim3, alloc4, x_01, x_01);
    }
    mark(allocs, marks, nallocs, alloc4);
    *ptr_resultparam22 = alloc4;
    *scalar_resultparam23 = x_01;
    
  done:
    gc(allocs, marks, nallocs);
    return cudaGetLastError();
}
