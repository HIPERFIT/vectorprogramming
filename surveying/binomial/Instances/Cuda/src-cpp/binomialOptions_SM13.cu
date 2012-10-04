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

#ifndef CUDA_NO_SM_13_DOUBLE_INTRINSICS
#define DOUBLE_PRECISION
#endif // #ifndef CUDA_NO_SM_13_DOUBLE_INTRINSICS
#include "binomialOptions_kernel.cuh"


extern "C" void binomialOptions_SM13(
    float *callValue,
    TOptionData  *optionData,
    int optN, int NUM_STEPS
)
{
    binomialOptionsGPU(callValue, optionData, optN, NUM_STEPS);
}

