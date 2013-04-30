#include "computepi.h"

#include <thrust/device_ptr.h>
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

float computepi(float *d_sobol, unsigned int n)
{
    thrust::device_ptr<float> sobol_ptr(d_sobol);
    thrust::device_vector<float> dv_sobol(sobol_ptr, sobol_ptr + 2*n);
    thrust::device_vector<float> output(n);
    
    thrust::transform(dv_sobol.begin(),
                      dv_sobol.begin() + n,
                      dv_sobol.begin() + n + 1,
                      output.begin(),
                      dist_to_origo());

    float x = thrust::reduce(output.begin(), output.end(), 0.0f, thrust::plus<float>());

    return 4 * ((n-x)/ n);
}
