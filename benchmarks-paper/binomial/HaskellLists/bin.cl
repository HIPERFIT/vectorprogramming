// https://code.google.com/p/adsm/source/browse/tests/simple/gmac/opencl/eclThreadBinomialOptionKernel.cl
// With some added comments and simplifications

#define RISKFREE 0.02f
#define VOLATILITY 0.30f

__kernel
void
binomial_options(
    int numSteps,
    const __global float4* randArray,
    __global float4* output,
    __local float4* callA,
    __local float4* callB,
    int offset)
{
    // load shared mem
    unsigned int tid = get_local_id(0);
    unsigned int bid = get_group_id(0);


    float4 inRand = randArray[bid + offset];

    /* S0, random value in range [5, 30] */
    float4 s = (1.0f - inRand) * 5.0f + inRand * 30.f;
    /* Strike price, random value in range [1, 100] */
    float4 x = (1.0f - inRand) * 1.0f + inRand * 100.f;
    /* Expiry, random value in range [0.25; 10] */
    float4 optionYears = (1.0f - inRand) * 0.25f + inRand * 10.f;

    float4 dt = optionYears / (float)numSteps;
    float4 vsdt = VOLATILITY * sqrt(dt);
    float4 r = exp(RISKFREE * dt);
    float4 rInv = 1.0f / r;
    float4 u = exp(vsdt);
    float4 d = 1.0f / u;
    float4 pu = (r - d)/(u - d);
    float4 pd = 1.0f - pu;
    float4 puByr = pu * rInv;
    float4 pdByr = pd * rInv;

    float4 profit = s * exp(vsdt * (2.0f * tid - (float)numSteps)) - x;
    callA[tid].x = max(profit.x, 0);
    callA[tid].y = max(profit.y, 0);
    callA[tid].z = max(profit.z, 0);
    callA[tid].w = max(profit.w, 0);

    barrier(CLK_LOCAL_MEM_FENCE);

    for(int j = numSteps; j > 0; j -= 2)
    {
        if(tid < j)
        {
            callB[tid] = puByr * callA[tid] + pdByr * callA[tid + 1];
        }
        barrier(CLK_LOCAL_MEM_FENCE);

        if(tid < j - 1)
        {
            callA[tid] = puByr * callB[tid] + pdByr * callB[tid + 1];
        }
        barrier(CLK_LOCAL_MEM_FENCE);
    }

    // write result for this block to global mem
    if(tid == 0) output[bid + offset] = callA[0];
}
