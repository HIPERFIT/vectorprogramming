#include <iostream>
#include <stdexcept>
#include <math.h>
#include <stdio.h>

#include <thrust/device_vector.h>
#include <thrust/host_vector.h>
#include <thrust/sequence.h>

#include "sobol.h"
#include "computepi.h"

#define L1ERROR_TOLERANCE (1e-6)

// Direction vector of two dimensions, 32 directions per dimension
unsigned int h_directions[] = 
  {2147483648,1073741824,2684354560,1342177280,2281701376,603979776,301989888,754974720,
   1988100096,2654994432,136314880,1678770176,2988965888,2098462720,4272029696,3125346304,
   438599680,1226522624,3300237312,3816001536,4135585792,3728737280,2820672000,873465088,
   975702144,1494483520,3970040096,2538144464,1822721896,3613084132,3432358018,2271450689,
   2147483648,1073741824,3758096384,2952790016,2550136832,2483027968,2315255808,1526726656,
   864026624,3653238784,1914699776,1058013184,3250061312,2800484352,1401290752,703922176,
   171606016,455786496,3549618176,1778348032,3929540608,2871788544,1269173760,4259646208,
   1610779008,4026976576,2016733344,605713840,305826616,3475687836,3113412898,2197780721};

int main(int argc, char *argv[])
{
    // needed to work correctly with piped benchmarkrunner
    setlinebuf(stdout);
    setlinebuf(stdin);

    // We will generate n_vectors vectors of n_dimensions numbers
    int n_vectors;
    int n_dimensions = 2;

    char inBuf[200]; // ridiculously large input buffer.
    printf("OK\n");

    while (true) {
      fgets(inBuf, 200, stdin);

      if (sscanf(inBuf, "%u", &n_vectors) == 0)
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

      thrust::device_vector<unsigned int> d_directions(h_directions, h_directions + n_directions*n_dimensions);
      thrust::device_vector<float> d_sobol(n_vectors * n_dimensions);

      generate_sobol(&d_directions, &d_sobol, n_vectors, n_dimensions);

      float pi = computepi(&d_sobol, n_vectors);

      printf("RESULT %f\n", pi);
    }
    exit(EXIT_SUCCESS);
}
