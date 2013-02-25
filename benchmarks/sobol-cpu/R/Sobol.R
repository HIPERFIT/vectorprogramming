library(randtoolbox, quietly=TRUE)


# Default model parameters
S0<-100;
strike<-100;
r<-0.03;
alpha<-0.07;
sigma<-0.20
bankDays <- 256;

cat("OK\n")

while(TRUE) {
  str = readLines(con="stdin", 1)
  if(str == "EXIT") {
    cat("OK\n");
    break;
  }

  n = as.numeric(str)

  sobol_sequence <- sobol(n, dim = 1)

  cat("RESULT ")
  cat(sobol_sequence[1:min(100,n)])
  cat("\n")
}
