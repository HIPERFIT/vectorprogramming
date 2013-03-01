/*
\(arr_0 :: Array Double[], arr_1 :: Array Double[]) ->
  let { v_17 :: Int32 = min (dim#0 arr_0) (dim#0 arr_1) } in
  { vec_alloca_2 :: Array Double[] <- alloc (Array Double[])[v_17]
  ; call (\(arr_0 :: Array Double[], arr_1 :: Array Double[], v_17 :: Int32,
            vec_alloca_2 :: Array Double[]) ->
            parfor((0) <= (i_3) < (v_17))
                write vec_alloca_2[i_3] (max 0.0 (100.0 - 100.0 * arr_0[i_3] *
                                                  arr_1[i_3]))) arr_0 arr_1 v_17
    vec_alloca_2
  ; return vec_alloca_2
  }
*/

#include "cuda.h"
#include "cuda_runtime_api.h"
#include <inttypes.h>
extern "C" __global__ void kern7(double* uPow, int32_t arr_0dim9,
                                 double* dPow, int32_t arr_1dim11,
                                 int32_t v_1712, double* output,
                                 int32_t vec_alloca_2dim14)
{
    for (int32_t i = blockIdx.x * blockDim.x + threadIdx.x; i < v_1712;
         i += blockDim.x * gridDim.x) {

        double strike_minus_st = 100.0 - 100.0 * uPow[i] * dPow[i];
        output[i] = max(0.0, strike_minus_st);
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
cudaError_t host0(double* arr_01, int32_t arr_0dim2, double* arr_13,
                  int32_t arr_1dim4, double** ptr_resultparam15,
                  int32_t* scalar_resultparam16)
{
    void* allocs[1];
    int marks[1];
    int nallocs = 0;
    int32_t v_17_5;
    double* alloc6 = NULL;
    
    v_17_5 = arr_0dim2 > arr_1dim4 ? arr_1dim4 : arr_0dim2;
    if (cudaMalloc(&alloc6, v_17_5 * sizeof(double)) != cudaSuccess)
        goto done;
    
    allocs[nallocs] = (void*) alloc6;
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
        kern7<<<gdims, tdims>>>(arr_01, arr_0dim2, arr_13, arr_1dim4, v_17_5,
                                alloc6, v_17_5);
    }
    mark(allocs, marks, nallocs, alloc6);
    *ptr_resultparam15 = alloc6;
    *scalar_resultparam16 = v_17_5;
    
  done:
    gc(allocs, marks, nallocs);
    return cudaGetLastError();
}

