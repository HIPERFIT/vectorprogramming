## Rolfs code
## http://www.math.ku.dk/~rolf/FAMOES/

# Simulation parameters
N.paths <- 100 #20000
N.points <-252


# Option parameters
S.0<-S0<-100
S.vol<-0.2
r<-0.03
K<-100 # strike price
Maturity <- 1
t.vec<-Maturity*0:N.points/N.points
dt<-Maturity/N.points

options(object.size=2000*(N.points+1)*N.paths)
#S.matrix<-matrix(scan('LS.testcasedata'),ncol=4,byrow=T)
S.matrix<-matrix(ncol=(N.points+1),nrow=(N.paths))
S.matrix[,1]<-S.0

coef1<-dt*(r-0.5*S.vol*S.vol)
coef2<-S.vol*sqrt(dt)

rnorm_antithetic <- function (count) {
  ran <- rnorm(count/2)
  return(c(ran,-ran))
}

for (k in 2:(N.points+1)){
  dWk<-rnorm_antithetic(N.paths)
  S.matrix[,k]<- S.matrix[,(k-1)]*exp(coef1+ coef2*dWk)
}

## temp = read.csv("paths1.csv", sep=",")
## S.matrix = t(as.matrix(temp))

Put.sim.matrix<-matrix(ncol=(N.points+1),nrow=(N.paths))
Put.sim.matrix[,(N.points+1)]<-pmax(K-S.matrix[,(N.points+1)],0)
