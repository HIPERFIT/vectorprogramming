/*
module Main where

gaussianElem ::  Double -> Double
gaussianElem q =
        let dq = q - 0.5
        in if( abs dq <= 0.425 ) then
               42.0
           else
               let pp = if dq < 0.0 then q else (1.0 - q)
                   s  = sqrt (0.0 - (log pp))
                   x  = if (s <= 5.0) then 3.14 * s else 2.5+s
               in if dq < 0.0 then (-x) else x

gaussian = map gaussianElem

main = defaultMain gaussian
*/

#include <stdio.h>
#include <math.h>

#define MIN(x,y) (x < y ? x : y)
typedef char bool;

void gaussian(int n, double inp[], double out[]) {

  int i;
  for (i = 0; i <n; i++) {

    double q = inp[i];
    double dq = q - 0.5;

    if (abs(dq) <= 0.4525){
      out[i] = 42.0;
    } else {

      double pp;
      if (dq < 0.0 ) {
        pp = q;
      } else {
        pp = 1.0-q;
      }

      double s  = sqrt(-log(pp));
      double x;

      if (s <= 5.0 ) {
        x = 3.14 * s;
      } else {
        x = 2.5+s;
      }
      if (dq < 0.0) {
        out[i] = -x;
      } else {
        out[i] = x;
      }
    }
  }

  return;
}

/* Branch Divergence Array expansion strategy below: */

int itperm(int size, char* sigma, bool* conds) {
    int beg = 0, end = size - 1;

    int j;
    for(j = 0; j < size /* -1 */; j++) {
        if( conds[beg] ) {
            beg = beg + 1;
        } else {
            char tmp   = sigma[beg];
            sigma[beg] = sigma[end];
            sigma[end] = tmp;
            end        = end - 1;
        }
    }

    //if( conds[size-1] ) { beg = beg + 1; }

    return beg;
}

int tile = 10;

void gaussianBrDiv(int n, double inp[], double out[]) {

  int i;
  int j;
  /*par*/for (i = 0; i < n; i+=tile) {

    double qs[tile];
    double dqs[tile];
    bool   conds[tile];
    char   sigma1[tile];
    double pps[tile];

    int M = MIN(i+tile,n)-i;

    /*seq*/for (j = 0; j < M; j++) {
      qs[j] = inp[i+j];
      dqs[j] = qs[j] - 0.5;
      conds[j] = abs(dqs[j])<= 0.4525;
      sigma1[j] = j;
    }

    int split = itperm(M, sigma1, conds);

    /*seq*/for (j = 0; j < split; j++) {
      out[sigma1[j]+i] = 42.0;
    }

    /*seq*/for (j = split; j < M; j++) {
      conds[j] = dqs[j] < 0.0;
    }

    bool *conds1 = &conds[split];
    char *sigma2 = &sigma1[split];
    int MF = M-split;
    int splitF = itperm(MF, sigma2, conds1);

    for (j = split; j < splitF; j++) {
      pps[j] = qs[j];
    }

    for (j = splitF; j < M; j++) {
      pps[j] = 1.0-qs[j];
    }

    for (j = split; j < M; j++) {
      double s  = sqrt(-log(pps[j]));
      double x;

      if (s <= 5.0 ) {
        x = 3.14 * s;
      } else {
        x = 2.5+s;
      }
      if (dqs[j] < 0.0) {
        out[sigma2[j]+i] = -x;
      } else {
        out[sigma2[j]+i] = x;
      }
    }
  }

  return;
}

int main(int argc, char *argv[]) {
  double inp;
  double out;

  sscanf(argv[1], "%lf", &inp);
  gaussian(1, &inp, &out);
  printf("gaussian(%.2f) = %.2f\n", inp,out);
  gaussianBrDiv(1, &inp, &out);
  printf("gaussianBrDiv(%.2f) = %.2f\n", inp,out);
}

