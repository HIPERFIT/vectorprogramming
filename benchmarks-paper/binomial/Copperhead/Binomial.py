from copperhead import *
import numpy as np

@cutype("[a] -> [a]")
@cu
def tail(xs):
    n = len(xs)
    return gather(xs, map(lambda i: i+1, range(n-1)))

@cutype("[a] -> [a]")
@cu
def init(xs):
    n = len(xs)
    return gather(xs, range(n-1))

@cutype("(Bool,Double,Double,Int,Int) -> [Double]")
@cu
def final(isCall,s0,strike,expiry,numSteps):
    volatility = 0.2
    riskless = 0.1
    dt = float64(expiry)/float64(numSteps)
    vsdt = volatility * sqrt(dt)

    leafs = map(lambda i: s0 * exp(vsdt * float64(2*i - numSteps)),
                range(numSteps+1))
    def maximum(a,b):
        if a > b:
            return a
        else:
            return b

    if isCall:
        return map(lambda x: maximum(x - strike, 0.0), leafs)
    else:
        return map(lambda x: maximum(strike - x, 0.0), leafs)

@cutype("([Double], Int, Int) -> [Double]")
@cu
def stepBack(vPrev,expiry,numSteps):
    volatility = 0.2
    riskless = 0.1

    dt = float64(expiry)/float64(numSteps)
    vsdt = volatility * sqrt(dt)
    u = exp(vsdt) 
    d = 1/u
    rr = exp(riskless*dt)
    rrInv = 1.0 / rr
    pu = (rr - d)/(u - d)
    pd = 1.0 - pu
    puByr = pu * rrInv
    pdByr = pd * rrInv

    def back(x1,x2):
        return puByr * x1 + pdByr * x2
    return map(back, tail(vPrev), init(vPrev))

def binom(isCall,s0,strike,expiry,numSteps):
    vFinal = final(isCall,s0,strike,expiry,numSteps)
    def stepBackClosure(vPrev,i):
        return stepBack(vPrev,expiry,numSteps)
    return reduce(stepBackClosure, range(numSteps), vFinal)

print(binom(True,60.0,65.0,1,10))
