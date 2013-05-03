from math import *

# Cumulative normal distribution
def CND(X):
    (a1,a2,a3,a4,a5) = (0.31938153, -0.356563782, 1.781477937
                        , -1.821255978, 1.330274429)
    L = abs(X)
    K = 1.0 / (1.0 + 0.2316419 * L)
    w = 1.0 - 1.0 / sqrt(2*pi)*exp(-L*L/2.) * (a1*K + a2*K*K + a3*pow(K,3) +
    a4*pow(K,4) + a5*pow(K,5))
    if X<0:
        w = 1.0-w
    return w

# Black Sholes Function
def BlackScholes((IsCall,S,X,T,r,v)):
    d1 = (log(S/X)+(r+v*v/2.)*T)/(v*sqrt(T))
    d2 = d1-v*sqrt(T)
    if IsCall:
        return S*CND(d1)-X*exp(-r*T)*CND(d2)
    else:
        return X*exp(-r*T)*CND(-d2)-S*CND(-d1)

def BlackScholesPortfolio(options):
    return map(BlackScholes, options)

print(BlackScholesPortfolio([(True,60.0,65.0,1,0.1,0.2)]))
