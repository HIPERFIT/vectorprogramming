#include <stdio.h>

#include <thrust/device_vector.h>
#include <thrust/transform.h>

struct Option {
  float s0;
  float strike;
  int expiry;
  float volatility;
  float riskless;
};

struct computeFinalCall {
private:
  const float vsdt;
  const float s0;
  const float strike;
  const int   numSteps;

public:
  computeFinalCall(float _vsdt, float _s0, float _strike, int _numSteps)
    : vsdt(_vsdt), s0(_s0), strike(_strike), numSteps(_numSteps) {}

  __host__ __device__
  float operator()(const int& i) const {
    float x = s0 * exp(vsdt * (float)(2 * i - numSteps));
    return max(0.0, x - strike);
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
float binom(Option option, int numSteps) {
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

  thrust::transform(first, last, stepA.begin(), computeFinalCall(vsdt, option.s0, option.strike, numSteps));

  for(int i = 0; i < numSteps/2; i++) {
    thrust::transform(stepA.begin() + 1, stepA.begin() + numSteps - i + 1, stepA.begin(), stepB.begin(), discount(puByr, pdByr));

    thrust::transform(stepB.begin() + 1, stepB.end() + numSteps - i, stepB.begin(), stepA.begin(), discount(puByr, pdByr));
  }

  return stepA[0];
}

int main(int argc, char *argv[])
{
    Option a = {60.0, 65.0, 1, 0.2, 0.1 };
    // needed to work correctly with piped benchmarkrunner
    setlinebuf(stdout);
    setlinebuf(stdin);

    int numSteps;

    char inBuf[200]; // ridiculously large input buffer.
    printf("OK\n");

    while (true) {
      fgets(inBuf, 200, stdin);

      if (sscanf(inBuf, "%u", &numSteps) == 0)
      {
        // if input is not a number, it has to be "EXIT"
        if (strncmp("EXIT",inBuf,4)==0)
        {
          printf("OK\n");
          break;
        }
        else
        {
          printf("ERROR. Bad input: %s\n", inBuf);
          break;
        }
      }

      float price = binom(a, numSteps);
      printf("RESULT %f\n", price);
    }
    exit(EXIT_SUCCESS);
}
