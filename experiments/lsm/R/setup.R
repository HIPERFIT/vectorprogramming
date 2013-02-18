## Rolfs code
## http://www.math.ku.dk/~rolf/FAMOES/

# Simulation parameters
N.paths <- 20000
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
S.matrix<-matrix(ncol=(N.points+1),nrow=(2*N.paths))
S.matrix[,1]<-S.0

 coef1<-dt*(r-0.5*S.vol*S.vol)
 coef2<-S.vol*sqrt(dt)

for (k in 2:(N.points+1)){
  dWk<-rnorm(N.paths)
  S.matrix[1:N.paths,k]<- S.matrix[1:N.paths,(k-1)]*exp(coef1+ coef2*dWk)
  S.matrix[(N.paths+1):(2*N.paths),k]<- S.matrix[(N.paths+1):(2*N.paths),(k-1)]*exp(coef1-coef2*dWk)
}


Put.sim.matrix<-matrix(ncol=(N.points+1),nrow=(2*N.paths))
Put.sim.matrix[,(N.points+1)]<-pmax(K-S.matrix[,(N.points+1)],0)
