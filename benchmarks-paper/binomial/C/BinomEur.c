#include <math.h>
#include <stdio.h>

/* European option pricing. 
 *  type == 0 ==> Call option
 *  type == 1 ==> Put option
 */
float binom(int numSteps,
            int type,
            float s0,
            float strike,
            float optionYears,
            float riskfree,
            float volatility) {
    float dt = optionYears / (float)numSteps;
    float vsdt = volatility * sqrt(dt);
    float rr = exp(riskfree * dt);
    float rrInv = 1.0f / rr;
    float u = exp(vsdt);
    float d = 1.0f / u;
    float pu = (rr - d)/(u - d);
    float pd = 1.0f - pu;
    float puByr = pu * rrInv;
    float pdByr = pd * rrInv;
    float price[numSteps+1];
    int i, j;

    /* Compute leaf profits */
    for(i = 0; i <= numSteps; i++) {
      //float profit = s0 * powf(u,i) * powf(d,numSteps-i) - strike;
      float profit = s0 * exp(vsdt * (2.0f * i - (float)numSteps));

      if(type == 0) {
        price[i] = fmax(profit - strike, 0.0);
      }
      else {
        price[i] = fmax(strike - profit, 0.0);
      }
    }

    /* Discount backwards */
    for (i = numSteps-1; i>=0;  i--) {
      for (j = 0; j<=i; j++){
        price[j] = pdByr * price[j] + puByr * price[j + 1];
      }
    }
    return price[0];
}

int main(int argc, char **argv) {
    setlinebuf(stdout);
    char inBuf[200]; // ridiculously large input buffer.
    int numSteps = 0;
    printf("OK\n");
    while (1) {
      if(!fgets(inBuf, 200, stdin)) {
        printf("ERROR reading from standard in");
        break;
      }
      if (sscanf(inBuf, "%u", &numSteps) == 0) {
        // if input is not a number, it has to be "EXIT"
        if (strncmp("EXIT",inBuf,4)==0) {
          printf("OK\n");
        }
        else {
          printf("ERROR. Bad input: %s\n", inBuf);
        }
        break;
      }
      
      double result = binom(numSteps, 1, 60.0, 65.0, 1, 0.1, 0.2);
      printf("RESULT %f\n", result);
    }
}
