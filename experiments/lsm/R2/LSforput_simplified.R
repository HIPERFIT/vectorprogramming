
## This is a simplification of Rolf's code obtained from
## http://www.math.ku.dk/~rolf/FAMOES/

## We have removed code for a case that is rarely touched. We should
## ask Rolf when this is an okay simplification.

source("setup.R")
source("polyfit.r")

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

  #est<-lsfit(RHS,Y, intercept=T)$coef
#  Rprof(append=TRUE)
  est<-polyfit(X,Y, 3)
#  Rprof(NULL)
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

print(c(putprice, 1.96*sd(exp(-r*dt)*disccashflow.vec)/sqrt(N.paths)))
