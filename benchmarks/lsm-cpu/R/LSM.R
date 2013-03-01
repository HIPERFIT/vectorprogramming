# Simulation parameters
N.paths <- 20000
N.points <-252


# Option parameters
S.0<-S0<-100
S.vol<-0.2
r<-0.03
K<-100 # strike price
Maturity <- 1

source("LSforput_simplified.R")

cat("OK\n")

while(TRUE) {
  str = readLines(con="stdin", 1)
  if(str == "EXIT") {
    cat("OK\n");
    break;
  }

  n = as.numeric(str)

  putprice <- LSM(n, N.points)

  cat("RESULT ")
  cat(putprice)
  cat("\n")
}
