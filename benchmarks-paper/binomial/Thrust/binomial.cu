#include <stdio.h>

#include <thrust/device_vector.h>
#include <thrust/transform.h>

#define numSteps 2048

struct Option {
  bool iscall;
  float s0;
  float strike;
  int expiry;
  float volatility;
  float riskless;
};

// struct computeFinal {
//   private:
//     const Option option;
//   public:
//     computeFinal(Option opt) : option(opt) {}
//     __host__ __device__
//     float operator()(const int& i) const {
//       float dt = (float)option.expiry/(float)numSteps;
//       float vsdt = option.volatility / sqrt(dt);
//       float x = option.s0 * exp(vsdt * (float)(2 * i - numSteps));
//       if(option.iscall) {
//         return max(0.0, x - option.strike);
//       } else {
//         return max(0.0, option.strike - x);
//       }
//     }
// };

struct computeFinalCall {
private:
  const float vsdt;
  const float s0;
  const float strike;

public:
  computeFinalCall(float _vsdt, float _s0, float _strike) : vsdt(_vsdt), s0(_s0), strike(_strike) {}

  __host__ __device__
  float operator()(const int& i) const {
    float x = s0 * exp(vsdt * (float)(2 * i - numSteps));
    return max(0.0, x - strike);
  }
};

struct computeFinalPut {
private:
  const float vsdt;
  const float s0;
  const float strike;

public:
  computeFinalPut(float _vsdt, float _s0, float _strike) : vsdt(_vsdt), s0(_s0), strike(_strike) {}

  __host__ __device__
  float operator()(const int& i) const {
    float x = s0 * exp(vsdt * (float)(2 * i - numSteps));
    return max(0.0, strike - x);
  }
};


struct discount {
private:
  const float puByr;
  const float pdByr;
public:
  discount(float _puByr, float _pdByr) : puByr(_puByr), pdByr(_pdByr) {}

  __host__ __device__
  float operator()(const float& x1, const float& x2) const {
    return puByr * x1 + pdByr * x2;
  }
};

// Compute \pi from a two-dimensional Sobol-sequence
float binom(Option option) {
  float dt = (float)option.expiry/(float)numSteps;
  float vsdt = option.volatility * sqrt(dt);
  float u = exp(vsdt);
  float d = 1/u;
  float rr = exp(option.riskless*dt);
  float rrInv = 1.0 / rr;
  float pu = (rr - d)/(u - d);
  float pd = 1.0 - pu;
  float puByr = pu * rrInv;
  float pdByr = pd * rrInv;

  thrust::device_vector<float> stepA(numSteps+1);
  thrust::device_vector<float> stepB(numSteps+1);

  thrust::counting_iterator<int> first(0);
  thrust::counting_iterator<int> last = first + numSteps + 1;

  if(option.iscall) {
    thrust::transform(first, last, stepA.begin(), computeFinalCall(vsdt, option.s0, option.strike));
  } else {
    thrust::transform(first, last, stepA.begin(), computeFinalPut(vsdt, option.s0, option.strike));
  }

  // for(int j = 0; j <= numSteps; j++) {
  //   float x = stepA[j];
  //   printf("%f, ", x);
  // }
  // printf("\n");

  for(int i = 0; i < numSteps/2; i++) {
    thrust::transform(stepA.begin() + 1, stepA.begin() + numSteps - i + 1, stepA.begin(), stepB.begin(), discount(puByr, pdByr));

    // for(int j = 0; j < numSteps - 2*i; j++) {
    //   float x = stepB[j];
    //   printf("%f, ", x);
    // }
    // printf("\n");

    thrust::transform(stepB.begin() + 1, stepB.end() + numSteps - i, stepB.begin(), stepA.begin(), discount(puByr, pdByr));

    // for(int j = 0; j < numSteps - 2*i - 1; j++) {
    //   float x = stepA[j];
    //   printf("%f, ", x);
    // }
    // printf("\n");

  }

  return stepA[0];
}



int main(int argc, char *argv[]) {
  Option a = {true, 60.0, 65.0, 1, 0.2, 0.1 };

  float price = binom(a);
  printf("%f\n", price);
}
