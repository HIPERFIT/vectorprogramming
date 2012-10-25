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



#ifndef BINOMIALOPTIONS_COMMON_H
#define BINOMIALOPTIONS_COMMON_H


////////////////////////////////////////////////////////////////////////////////
// Global types
////////////////////////////////////////////////////////////////////////////////
typedef struct
{
    float S_0;
    float strike;
    float expiry;
    float bankDays;
    float r;
    float sigma;
    float alpha;
} TOptionData;



#endif
