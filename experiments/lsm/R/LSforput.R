## Rolfs code
## http://www.math.ku.dk/~rolf/FAMOES/
source("setup.R")

alpha.ls.vec<-0*0:N.points
alpha.ls.vec[N.points+1]<-K
exercisetime.vec<-0/0+0*1:(N.paths) # ~ikke exerciset
disccashflow.vec<-Put.sim.matrix[,(N.points+1)]
exercisetime.vec[disccashflow.vec>0]<-t.vec[(N.points+1)]

for (k in N.points:2){

  Sk<-S.matrix[,k]
  intrinsicvalue<-pmax(K-Sk,0)
  pick<-intrinsicvalue>0
  X<-S.matrix[pick,k]
  RHS<-cbind(X,X*X)
  Y<-exp(-r*dt)*disccashflow.vec[pick]

  if(length(X) >= 3) {
    RHS<-cbind(X,X*X) 
    Y<-exp(-r*dt)*disccashflow.vec[pick]

    est<-lsfit(RHS,Y, intercept=T)$coef
    estimatedtimevalue<-est[1]+est[2]*Sk+est[3]*Sk*Sk

    exercise<-(pick) & (intrinsicvalue>estimatedtimevalue)
    notexercise<-!exercise

    disccashflow.vec[notexercise]<-exp(-r*dt)*disccashflow.vec[notexercise]

    disccashflow.vec[exercise]<-intrinsicvalue[exercise]
    exercisetime.vec[exercise]<-t.vec[k]

    soln<-Re(polyroot(c((est[1]-K), (est[2]+1),est[3])))
    alpha.ls.vec[k]<-max((soln[soln<= K]))

  }
  
  if(length(X) <= 2){

    exercise<-(pick) & (Sk <=alpha.ls.vec[k+1])
    notexercise<-!exercise

    disccashflow.vec[notexercise]<-exp(-r*dt)*disccashflow.vec[notexercise]

    disccashflow.vec[exercise]<-intrinsicvalue[exercise]
    exercisetime.vec[exercise]<-t.vec[k]
    alpha.ls.vec[k]<-alpha.ls.vec[k+1]
  }
}

alpha.ls.vec[1]<-alpha.ls.vec[2]

putprice<-exp(-r*dt)*mean(disccashflow.vec)

if(K-S0 > putprice ) {
  putprice <-K-S0
  exercisetime.vec<-0
}

print(c(putprice, 1.96*sd(exp(-r*dt)*disccashflow.vec)/sqrt(N.paths)))
