## This is a simplification of Rolf's code obtained from
## http://www.math.ku.dk/~rolf/FAMOES/


rnorm_antithetic <- function (count) {
  ran <- rnorm(count/2)
  return(c(ran,-ran))
}

LSM <- function(N.paths, N.points) {

t.vec<-Maturity*0:N.points/N.points
dt<-Maturity/N.points

#options(object.size=2000*(N.points+1)*N.paths)
#S.matrix<-matrix(scan('LS.testcasedata'),ncol=4,byrow=T)
S.matrix<-matrix(ncol=(N.points+1),nrow=(N.paths))
S.matrix[,1]<-S.0

coef1<-dt*(r-0.5*S.vol*S.vol)
coef2<-S.vol*sqrt(dt)

for (k in 2:(N.points+1)){
  dWk<-rnorm_antithetic(N.paths)
  S.matrix[,k]<- S.matrix[,(k-1)]*exp(coef1+ coef2*dWk)
}

Put.sim.matrix<-matrix(ncol=(N.points+1),nrow=(N.paths))
Put.sim.matrix[,(N.points+1)]<-pmax(K-S.matrix[,(N.points+1)],0)
  
disccashflow.vec<-Put.sim.matrix[,(N.points+1)]

for (k in N.points:2){ # fold

  Sk<-S.matrix[,k]
  intrinsicvalue<-pmax(K-Sk,0)
  pick<-intrinsicvalue>0
  X<-S.matrix[pick,k]
  RHS<-cbind(X,X*X)

  if(length(X) <= 2) {
    # It seems that this happens very rarely, at least for large
    # number of paths. If this occurs repeatedly, try with more paths
    # or use Rolf's original code
    print("Mission aborted. Try again.")
    quit(1);
  }
  

  RHS<-cbind(X,X*X) 
  Y<-exp(-r*dt)*disccashflow.vec[pick]

  est<-lsfit(RHS,Y, intercept=T)$coef

  # polyval
  estimatedtimevalue<-est[1]+est[2]*Sk+est[3]*Sk*Sk


  exercise<-(pick) & (intrinsicvalue>estimatedtimevalue)
  notexercise<-!exercise

  disccashflow.vec[notexercise]<-exp(-r*dt)*disccashflow.vec[notexercise]

  disccashflow.vec[exercise]<-intrinsicvalue[exercise]
}

putprice<-exp(-r*dt)*mean(disccashflow.vec)

if(K-S0 > putprice ) {
  putprice <-K-S0
}

#print(c(putprice, 1.96*sd(exp(-r*dt)*disccashflow.vec)/sqrt(N.paths)))

return(putprice)

}
