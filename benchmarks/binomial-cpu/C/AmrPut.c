#include<stdio.h>
#include<math.h>

double max (double x, double y) {
  if (x>= y) return x; 
  else return y; 
}

double american_put_pricer(int expiry) {
  double S; 
  double S0=100; double r=0.03; double alpha=0.07; double sigma=0.2; 

  double strike=100; 

  const int n=256*expiry;

  double dt=((double)expiry)/n;

  double u=exp(alpha*dt+sigma*sqrt(dt)); 
  double d=exp(alpha*dt-sigma*sqrt(dt)); 
  double R=exp(r*dt); 

  double q=(R-d)/(u-d); double qUR=q/R; double qDR =(1-q)/R;

  double putA[n+1];

  double uvec[n+1], dvec[n+1]; 

  int i, j;

  for (i=0; i<=n; i++)  { 
    putA[i]=max(strike-S0*pow(u,i)*pow(d,n-i),0);
    uvec[i]=pow(u,i); dvec[i]=pow(d,i); 
  }

  for (i=(n-1); i>=0;  i--) {
    for (j = 0; j<=i; j++){
      S=S0*uvec[j]*dvec[i-j];  
      putA[j]=max(strike-S,qUR*putA[j+1]+qDR*putA[j]); 
    }
  }

  return putA[0];
}


int main(int argc, char **argv)
{

    setlinebuf(stdout);

    double errorVal;
    double callValueGPU;
    int i;

    char inBuf[200]; // ridiculously large input buffer.
    int expiry = 0;
    printf("OK\n");
    while (1) {

      fgets(inBuf, 200, stdin);

      if (sscanf(inBuf, "%u", &expiry) == 0)
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


      double result = american_put_pricer(expiry);

      printf("RESULT %f\n", result);
    }
}
