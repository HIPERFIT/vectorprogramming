#include <math.h>
#include <stdio.h>

/* European Call option pricing. 
 */
float binom(int numSteps,
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
      price[i] = fmax(profit - strike, 0.0);
    }

    /* Discount backwards */
    for (i = numSteps-1; i>=0;  i--) {
      for (j = 0; j<=i; j++){
        price[j] = pdByr * price[j] + puByr * price[j + 1];
      }
    }
    return price[0];
}
