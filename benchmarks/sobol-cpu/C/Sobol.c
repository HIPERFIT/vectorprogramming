#include<stdio.h>
#include<math.h>
#include <strings.h>

int powInt(int x, int y)
{
  int res = 1;
  int i = 0;
  for (i = 0; i < y; i++) {
    res *=x;
  }
  return res;
}


unsigned int sobolRec(int i, unsigned int last, unsigned int *v) {
  printf("i: %d, ffs: %d\n", i, ffs(i)-1);
  return last ^ v[ffs(i)-1];
}

/* n: Length of sobol Sequence
 * result: return array
 * v: Direction numbers
 * bitcount: Number of bits (and length of direction vector)
 */
int sobol(int n, 
          double *result,
          unsigned int *v,
          unsigned int bitcount) {
  int i;
  double sobol_divisor = powl(2, (double)bitcount);
  //printf("sobol_divisor: %f\n", sobol_divisor);
  unsigned int acc = 0;

  for (i = 0; i < n; i++) {
    //    printf("i: %d, acc: %d\n", i, acc);
    acc = sobolRec(i+1, acc, v);
    result[i] = ((double)acc)/sobol_divisor;
  }
  return 0;
}

int initDirectionVector(unsigned int* v, unsigned int bitcount) {
  int i;
  for(i = 0; i < bitcount; i++) {
    int k = bitcount - i - 1;
    v[i] = powInt(2, k);
    //    printf("i: %d, v: %d\n", i, v[i]);
  }
  return 0;
}

// Underlying Words
// Should print [0.5,0.75,0.25,0.375,0.875,0.625,0.125,0.1875,0.6875,0.9375]
int main(int argc, char **argv)
{
    setlinebuf(stdout);

    double errorVal;
    double callValueGPU;
    int i;
    unsigned int bitcount = 30;
    unsigned int directionVector[bitcount];
    initDirectionVector(directionVector, bitcount);

    char inBuf[200]; // ridiculously large input buffer.
    int sobolLength = 0;
    printf("OK\n");
    while (1) {
      fgets(inBuf, 200, stdin);
      if (sscanf(inBuf, "%u", &sobolLength) == 0)
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
      double res[sobolLength];
      sobol(sobolLength, res, directionVector, bitcount);


    printf("RESULT ");

    for(i = 0; i < 10; i++)
      printf("%f ", res[i]);

    printf("\n");
    }
}
