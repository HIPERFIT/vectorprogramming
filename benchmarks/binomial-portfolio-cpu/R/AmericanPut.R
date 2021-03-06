# Option pricer code from Rolf Poulsen
AmericanPutOptionPricer <- function(S0, strike, r, alpha, sigma, expiry, n){
  dt<-expiry/n
  u<-exp(alpha*dt+sigma*sqrt(dt)); d<-exp(alpha*dt-sigma*sqrt(dt))
  R<-exp(r*dt)
  q<-(R-d)/(u-d)
  qUR<-q/R; qDR<-(1-q)/R
  u.pow<-u^(0:n); d.pow<-d^(0:n)

  St<-S0*u.pow[1:(n+1)]*d.pow[(n+1):1]
  put<-pmax(strike - St,0)

  for (i in n:1) {
    St<-S0*u.pow[1:i]*d.pow[i:1]
    put[1:i]<-pmax(strike-St,(qUR*put[2:(i+1)]+qDR*put[1:i]))
  }

  return(put[1])
}

# Default model parameters
S0<-100;
strike<-100;
r<-0.03;
alpha<-0.07;
sigma<-0.20
bankDays <- 256;
expiry <- 2
n <- expiry * bankDays;

cat("OK\n")

while(TRUE) {
  str = readLines(con="stdin", 1)
  if(str == "EXIT") {
    cat("OK\n");
    break;
  }

  n_options = as.numeric(str)

  results <- rep(0, n_options)
  for (i in 1:n_options) {
    results[i] = AmericanPutOptionPricer(S0, strike, r, alpha, sigma, expiry, n)
  }

  cat("RESULT ")
  # Print the first 10 results
  for (i in 1:min(n_options,10)) {
    cat(results[i])
    cat(", ")
  }
  cat("\n")
}
