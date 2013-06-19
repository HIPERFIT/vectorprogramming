#include "computepi.h"

#include <thrust/device_ptr.h>
#include <thrust/device_vector.h>
#include <thrust/reduce.h>
#include <thrust/transform.h>

struct dist_to_origo : public thrust::unary_function<thrust::tuple<float,float>,float>
{
  __host__ __device__
  float operator()(const thrust::tuple<float, float>& p) const {
    float x = p.get<0>();
    float y = p.get<1>();
      return trunc(sqrt(x*x + y*y));
  }
};

float computepi(float *d_sobol, unsigned int n)
{
    thrust::device_ptr<float> sobol_ptr(d_sobol);
    thrust::device_vector<float> dv_sobol(sobol_ptr, sobol_ptr + 2*n);

    typedef thrust::device_vector<float>::iterator NormalIterator;
    typedef thrust::tuple<NormalIterator, NormalIterator> IteratorTuple;
    typedef thrust::zip_iterator<IteratorTuple> ZipIterator;

    ZipIterator iter_zip(thrust::make_tuple(dv_sobol.begin(),
                                            dv_sobol.begin()+n+1));
    typedef thrust::transform_iterator<dist_to_origo, ZipIterator> TransformIteratorDist;
    TransformIteratorDist iter_dist = thrust::make_transform_iterator(iter_zip, dist_to_origo());

    float x = thrust::reduce(iter_dist, iter_dist + n , 0.0f, thrust::plus<float>());
    return 4 * ((n-x)/ n);
}
