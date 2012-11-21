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
#include <stdlib.h>
#include <math.h>

#define MIN(x,y) (x < y ? x : y)
typedef char bool;

void showarrayf(int n, double arr[]) {
  int i;
  for (i = 0; i < n; i++) {
    printf("%.0lf ", arr[i]);
  }
}

void showarrayb(int n, bool arr[]) {
  int i;
  for (i = 0; i < n; i++) {
    printf("%d ", arr[i]);
  }
}

void showarrayi(int n, int arr[]) {
  int i;
  for (i = 0; i < n; i++) {
    printf("%d ", arr[i]);
  }
}

void gaussian(int N, double inp[], double out[]) {

  int i;
  for (i = 0; i < N; i++) {

    double q = inp[i];
    double dq = q - 0.5;

    if (fabs(dq) <= 0.425){
      out[i] = 1;
    } else {

      double pp;
      if (dq < 0.0 ) {
        pp = q;
      } else {
        pp = 1.0-q;
      }

      double s  = sqrt(-log(pp));
      double x = s <= 5.0 ? 2 : 3;
      out[i] = dq < 0.0 ? 4 : 5;
    }
  }

  return;
}

/* Branch Divergence Array expansion strategy below: */

int itperm(int size, char* sigma, bool* conds) {
    int beg = 0, end = size - 1;

    int j;
    for(j = 0; j < size-1; j++) {
        if( conds[sigma[beg]] ) {
            beg = beg + 1;
        } else {
            char tmp   = sigma[beg];
            sigma[beg] = sigma[end];
            sigma[end] = tmp;
            end        = end - 1;
        }
    }

    if(conds[sigma[beg]]) {
      beg++;
    }

    return beg;
}

int tile = 7;

void gaussianBrDiv(int N, double inp[], double out[]) {

  int i;
  int j;
  /*par*/for (i = 0; i < N; i+=tile) {

    double qs[tile];
    double dqs[tile];
    bool   conds[tile];
    char   sigma1[tile];
    double pps[tile];
    double ss[tile];

    int M = MIN(tile,N-i);

    /*seq*/for (j = 0; j < M; j++) {
      qs[j] = inp[i+j];
      dqs[j] = qs[j] - 0.5;
      conds[j] = fabs(dqs[j])<= 0.425;
      sigma1[j] = j;
    }

    int split = itperm(M, sigma1, conds);
    /*seq*/for (j = 0; j < split; j++) {
      out[sigma1[j]+i] = 1;
    }

    /* printf("split = %d\n", split); */
    /* printf("conds   = { "); */
    /* showarrayb(M, conds); */
    /* printf("}\n"); */

    /* printf("sigma1  = { "); */
    /* showarrayb(M, sigma1); */
    /* printf("}\n"); */

    /* printf("out i=%d = { ", i); */
    /* showarrayf(N, out); */
    /* printf("}\n"); */


    bool *conds1 = &conds[split];
    int MF = M-split;

    /*seq*/for (j = 0; j < MF; j++) {
      conds1[j] = dqs[j+split] < 0.0;
      ss[sigma1[j+split]]  = sqrt(-log(pps[sigma1[j+split]]));
    }
    char *sigma2 = &sigma1[split];

    int splitF = itperm(MF, sigma2, conds1);

    /*printf("conds1 = {");
    showarrayb(MF, conds1);
    printf("}\n");

    printf("sigma2 = {");
    showarrayb(MF, sigma2);
    printf("}\n");

    printf("conds1 = {");
    showarrayb(MF, conds1);
    printf("}\n");

    printf("sigma2 = {");
    showarrayb(MF, sigma2);
    printf("}\n");
    */

    for (j = split; j < splitF; j++) {
      pps[sigma1[j]] = qs[sigma1[j]];
    }

    for (j = splitF; j < M; j++) {
      pps[sigma1[j]] = 1.0-qs[sigma1[j]];
    }

    /*
    printf("pps = { ");
    showarrayf(M, pps);
    printf("} \n");
    printf("sigma1 = {");
    showarrayb(M, sigma1);
    printf("}\n");
    */

    for (j = split; j < M; j++) {
      //printf("%.2lf ", s);
      //printf("%d ", j);
      double x;

      if (ss[sigma1[j]] <= 5.0) {
        x = 2; // 1000000 * s;
      } else {
        x = 3; //5555555+s;
      }
      if (dqs[sigma1[j]] < 0.0) {
        out[sigma1[j]+i] = 4; // -x;
      } else {
        out[sigma1[j]+i] = 5; // x;
      }
    }
  }

  return;
}

int main(int argc, char *argv[]) {

  //sscanf(argv[1], "%lf", &inp);
  double inp[] = {0.0,   0.1,  0.2,  0.3,  0.4,  0.5,  0.6,  0.7,
                  0.85, 0.90, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96,
                  0.85, 0.90, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96};
  int n = 24;
  double out1[n];
  double *out2 = (double*) calloc(n, sizeof(double));

  printf("inp = {");
  showarrayf(n, inp);
  printf("}\n");

  gaussian(n, inp, out1);
  printf("gaussian(..)      = ");
  showarrayf(n, out1);
  printf("\n");

  gaussianBrDiv(n, inp, out2);
  printf("gaussianBrDiv(..) = ");
  showarrayf(n, out2);
  printf("\n");
}
