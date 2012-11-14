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

void showarrayb(char title[], int n, bool arr[]) {
  int i;

  printf("%s  = { ", title);
  for (i = 0; i < n; i++) {
    printf("%d ", arr[i]);
  }
  printf("}\n");
}

void showarrayi(char title[], int n, int arr[]) {
  int i;

  printf("%s  = { ", title);
  for (i = 0; i < n; i++) {
    printf("%d ", arr[i]);
  }
  printf("}\n");
}

void gaussian(int N, int inp[], int out[]) {

  int i;
  for (i = 0; i < N; i++) {

    int q = inp[i];

    if (q % 2 == 0){
      out[i] = 1;
    } else {

      int pp;
      if (q % 3 == 0) {
        pp = q;
      } else {
        pp = -q;
      }

      out[i] = pp; //q % 5 == 0 ? pp : 100*pp;
    }
  }

  return;
}

/* Branch Divergence Array expansion strategy below: */

int itperm(int size, char* sigma, bool* conds) {
    int beg = 0, end = size - 1;

    if(size == 1) {
      return conds[0];
    }

    int j;
    for(j = 0; j < size-1; j++) {
        if( conds[beg] ) {
            beg = beg + 1;
        } else {
            char tmp   = sigma[beg];
            sigma[beg] = sigma[end];
            sigma[end] = tmp;
            tmp   = conds[beg];
            conds[beg] = conds[end];
            conds[end] = tmp;
            end        = end - 1;
        }
    }

    if(conds[beg]) {
      beg++;
    }

    return beg;
}



int tile = 7;

void gaussianBrDiv(int N, int inp[], int out[]) {

  int i;
  int j;
  /*par*/for (i = 0; i < N; i+=tile) {

    int qs[tile];
    bool   conds1[tile];
    char   sigma1[tile];
    bool   conds2[tile];
    char   sigma2[tile];
    int pps[tile];

    int M = MIN(tile,N-i);

    /* Evaluate condition for complete tile */
    /*seq*/for (j = 0; j < M; j++) {
      qs[j] = inp[i+j];
      conds1[j] = qs[j] % 2 == 0;
      sigma1[j] = j;
    }
    int split = itperm(M, sigma1, conds1);

    /* True branch */
    /*seq*/for (j = 0; j < split; j++) {
      out[sigma1[j]+i] = 1;
    }

    /* False branch */

    int MF = M-split;
    /* printf("1: split = %d, M = %d, MF = %d\n", split, M, MF); */
    /* showarrayi("1: out", M, out); */
    /* showarrayi("1: qs", M, qs); */
    /* showarrayb("1: conds1", M, conds1); */
    /* showarrayb("1: sigma1", M, sigma1); */

    /*seq*/for (j = split; j < M; j++) {
      conds2[j] = qs[sigma1[j]] % 3 == 0;
      sigma2[j] = sigma1[j];
    }

    bool *conds2_ = &conds2[split];
    char *sigma2_ = &sigma2[split];

    int splitF = itperm(MF, sigma2_, conds2_);

    /* showarrayb("2: conds2", M, conds2); */
    /* showarrayb("2: sigma2", M, sigma2); */
    /* printf("2: splitF = %d\n", splitF); */

    for (j = split; j < split+splitF; j++) {
      pps[sigma2[j]] = qs[sigma1[j]];
    }

    for (j = split+splitF; j < M; j++) {
      pps[sigma2[j]] = -qs[sigma1[j]];
    }

    for (j = split; j < M; j++) {
      out[sigma2[j]+i] = pps[sigma2[j]];//qs[sigma2[j]] % 5 == 0 ? pps[sigma2[j]] : 100*pps[sigma2[j]];
    }
  }

  return;
}

int main(int argc, char *argv[]) {

  int n = 6;

  /* for(n = 0; n < 30; n++) { */

  int inp[n];
  int i;
  for(i = 0; i < n; i++) {
    inp[i] = i;
  }

  int *out1 = (int*) calloc(n, sizeof(int));
  int *out2 = (int*) calloc(n, sizeof(int));

  showarrayi("inp", n, inp);

  gaussian(n, inp, out1);
  showarrayi("gaussian(..)     ", n, out1);

  gaussianBrDiv(n, inp, out2);
  showarrayi("gaussianBrDiv(..)", n, out2);
  /* } */
}
