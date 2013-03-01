/*
\(arr_0 :: Array Double[], arr_1 :: Array Double[], x_2 :: Int32,
  arr_3 :: Array Double[], x_4 :: Int32) ->
  let { v_10 :: Int32 = 256 * x_2 } in
  let { v_12 :: Int32 = 1 + v_10 - x_4 } in
  let { v_128 :: Bool = v_12 < 0 } in
  let { v_132 :: Int32 = max 0 (-1 + dim#0 arr_3) } in
  let { v_83 :: Int32 = min (min (if x_4 < 0
                                  then 0
                                  else min x_4 (dim#0 arr_0)) (if v_128
                                                               then dim#0 arr_1
                                                               else max 0 (dim#0 arr_1 -
                                                                           v_12))) (min v_132 v_132) } in
  { vec_alloca_5 :: Array Double[] <- alloc (Array Double[])[v_83]
  ; call (\(arr_0 :: Array Double[], arr_1 :: Array Double[],
            arr_3 :: Array Double[], v_10 :: Int32, v_12 :: Int32,
            v_128 :: Bool, v_83 :: Int32, vec_alloca_5 :: Array Double[],
            x_2 :: Int32) ->
            parfor((0) <= (i_6) < (v_83))
                { let v_40 :: Double = (double) x_2 / (double) v_10
                ; let v_42 :: Double = exp (3.0e-2 * v_40)
                ; let v_99 :: Double = 7.0e-2 * v_40
                ; let v_142 :: Double = 0.2 * sqrt v_40
                ; let v_47 :: Double = exp (v_99 - v_142)
                ; let v_55 :: Double = (v_42 - v_47) / (exp (v_99 + v_142) -
                                                        v_47)
                ; write vec_alloca_5[i_6] (max (100.0 - 100.0 * arr_0[i_6] *
                                                arr_1[if v_128
                                                      then i_6
                                                      else i_6 + v_12]) (v_55 /
                                                                         v_42 *
                                                                         arr_3[1 +
                                                                               i_6] +
                                                                         (1.0 -
                                                                          v_55) /
                                                                         v_42 *
                                                                         arr_3[i_6]))
                }) arr_0 arr_1 arr_3 v_10 v_12 v_128 v_83 vec_alloca_5 x_2
  ; return vec_alloca_5
  }
*/

#include "cuda.h"
#include "cuda_runtime_api.h"
#include <inttypes.h>
extern "C" __global__ void kern17(double* uPow, int32_t uPow_length,
                                  double* dPow, int32_t dPow_length,
                                  double* put, int32_t put_length,
                                  int32_t n, int32_t v_1225,
                                  uint8_t v_12826, int32_t output_length,
                                  double* output,
                                  int32_t output_length, int32_t expiry)
{
    for (int32_t i = blockIdx.x * blockDim.x + threadIdx.x; i < output_length;
         i += blockDim.x * gridDim.x) {
        double dt;
        double stepR;
        double alpha_dt;
        double u;
        double d;
        double q;
        int32_t ifte_result37;
        
        dt = (double) expiry / (double) n;
        stepR = exp(3.0e-2 * dt);
        alpha_dt = 7.0e-2 * dt;
        u = 0.2 * sqrt(dt);
        d = exp(alpha_dt - u);
        q = (stepR - d) / (exp(alpha_dt + u) - d);
        if (v_12826) {
            ifte_result37 = i;
        } else {
            ifte_result37 = i + v_1225;
        }

        double strike_minus_st = 100.0 - 100.0 * uPow[i] * dPow[ifte_result37];
        double e = q / stepR * put[1 + i] +
           (1.0 - q) / stepR * put[i];

        output[i] = max(strike_minus_st, e);

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
cudaError_t host0(double* uPow, int32_t uPow_length, double* dPow,
                  int32_t dPow_length, int32_t expiry, double* put,
                  int32_t put_length, int32_t i, double** ptr_resultparam38,
                  int32_t* scalar_resultparam39)
{
    void* allocs[1];
    int marks[1];
    int nallocs = 0;
    int32_t n;
    int32_t v_12_10;
    uint8_t v_128_11;
    int32_t ifte_result13;
    int32_t ifte_result14;
    int32_t return_array_length;
    double* return_array = NULL;
    
    n = 256 * expiry;
    v_12_10 = 1 + n - i;
    v_128_11 = v_12_10 < 0;
    if (i < 0) {
        ifte_result13 = 0;
    } else {
        ifte_result13 = min(i, uPow_length);
    }
    if (v_128_11) {
        ifte_result14 = dPow_length;
    } else {
        ifte_result14 = max(0, dPow_length - v_12_10);
    }
    return_array_length = min(min(ifte_result13, ifte_result14),
                              max(0, -1 + put_length));
    if (cudaMalloc(&return_array, return_array_length * sizeof(double)) != cudaSuccess)
        goto done;
    
    allocs[nallocs] = (void*) return_array;
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
        kern17<<<gdims, tdims>>>(uPow, uPow_length, dPow, dPow_length, put,
                                 put_length, n, v_12_10, v_128_11, return_array_length,
                                 return_array, return_array_length, expiry);
    }
    mark(allocs, marks, nallocs, return_array);
    *ptr_resultparam38 = return_array;
    *scalar_resultparam39 = return_array_length;
    
  done:
    gc(allocs, marks, nallocs);
    return cudaGetLastError();
}

