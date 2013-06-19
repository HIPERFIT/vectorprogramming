#include "computepi.h"
#include "sobol.h"

#include <thrust/device_vector.h>
#include <thrust/generate.h>
#include <thrust/device_vector.h>
#include <thrust/reduce.h>
#include <thrust/transform.h>

#define TESTBIT(var,pos) (((var) & (1<<(pos))) != 0)
#define k_2powneg32 2.3283064E-10F

/* SOBOL GENERATOR */
// Inductively generates a Sobol number from a direction vector and
// the index in the sequence
__host__ __device__
float sobolInductive(const unsigned int *dir, unsigned int i) {
  unsigned int acc = 0;
  for(int j = 0; j < n_directions; j++) {
    acc ^= TESTBIT(i,j) * dir[j];
  }
  return ((float)acc) * k_2powneg32;
}

struct sobolInductiveFunctor : public thrust::unary_function<int,float> {
private:
  const int n;
  const unsigned int *d_directions_raw;

public:
  sobolInductiveFunctor(unsigned int _n, unsigned int* _d_directions_raw)
    : n(_n), d_directions_raw(_d_directions_raw)  {}

  __host__ __device__
  float operator()(const int& i) const {
    int dirvec_id = i / n;
    int sobol_id = i % n;
    return sobolInductive(&d_directions_raw[dirvec_id*n_directions], sobol_id);
  }
};


/* COMPUTING PI */
struct dist_to_origo : public thrust::unary_function<thrust::tuple<float,float>,float>
{
  __host__ __device__
  float operator()(const thrust::tuple<float, float>& p) const {
    float x = p.get<0>();
    float y = p.get<1>();
      return trunc(sqrt(x*x + y*y));
  }
};

// Compute \pi from a two-dimensional Sobol-sequence
float computepi(thrust::device_vector<unsigned int> *d_directions,
                unsigned int n,
                unsigned int dimensions)
{

  // Convert to raw CUDA pointer
  unsigned int *d_directions_raw = thrust::raw_pointer_cast(&(*d_directions)[0]);

  typedef thrust::counting_iterator<int> CountingIterator;
  typedef thrust::transform_iterator<sobolInductiveFunctor, CountingIterator> TransformIteratorSobol;

  CountingIterator iter_counting(0);
  TransformIteratorSobol iter_sobol = 
    thrust::make_transform_iterator(iter_counting, sobolInductiveFunctor(n, d_directions_raw));

  typedef thrust::tuple<TransformIteratorSobol, TransformIteratorSobol> IteratorTuple;
  typedef thrust::zip_iterator<IteratorTuple> ZipIterator;

  ZipIterator iter_zip(thrust::make_tuple(iter_sobol, iter_sobol+n+1));

  typedef thrust::transform_iterator<dist_to_origo, ZipIterator> TransformIteratorDist;

  TransformIteratorDist iter_dist = thrust::make_transform_iterator(iter_zip, dist_to_origo());

  float x = thrust::reduce(iter_dist, iter_dist + n, 0.0f, thrust::plus<float>());
  return 4 * ((n-x)/ n);
}
