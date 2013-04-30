#include "computepi.h"

#include <thrust/device_vector.h>
#include <thrust/reduce.h>
#include <thrust/transform.h>

struct dist_to_origo
{
    __host__ __device__
        float operator()(const float& x, const float& y) const {
            return trunc(sqrt(x*x + y*y));
        }
};

// Compute \pi from a two-dimensional Sobol-sequence
float computepi(thrust::device_vector<float> *d_sobol, unsigned int n)
{
    thrust::device_vector<float> output(n);
    
    thrust::transform(d_sobol->begin(),
                      d_sobol->begin() + n,
                      d_sobol->begin() + n + 1,
                      output.begin(),
                      dist_to_origo());

    float x = thrust::reduce(output.begin(), output.end(), 0.0f, thrust::plus<float>());
    return 4 * ((n-x)/ n);
}
