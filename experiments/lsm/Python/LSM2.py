
# Valuation of American Put Option
# with Least-Squares Monte Carlo

from numpy import *
from numpy.random import standard_normal, seed
from time import time
t0 = time ()

## Simulation Parameters
#seed(150000)       # seed for Python RNG
Npaths   = 20000   # paths for valuation
Npoints  = 252     # time steps
reg = 2            # no of basis functions

## Parameters -- American Put Option
S0  = 100.0        # initial stock level
vol = 0.2          # volatility
r   = 0.03         # short rate
K   = 100.0        # strike price
T   = 1.0          # time - to - maturity
dt  = T/Npoints          # length of time interval
df  = exp(-r*dt)   # discount factor per time interval

def RNG(Npaths):
    """Generate Npaths random numbers (w. antithetic variates)"""
    ran = standard_normal(Npaths/2)
    ran = concatenate((ran, -ran))
    return ran

def GenS(Npaths):
    coef1 = dt*(r-0.5*vol*vol)
    coef2 = vol*sqrt(dt)
    S = zeros((Npoints+2, Npaths), 'd')   # index level matrix
    S[0,:] = S0                # initial values
    for k in range(1, Npoints+2, 1): # index level paths
        ran = RNG(Npaths)
        S[k,:]= S[k-1,:] * exp(coef1 + ran*coef2)
    return S

def IV(S):
    return maximum(K-S,0)

## Valuation by LSM
S = GenS(Npaths) # generate stock price paths
# savetxt("paths.csv", S, delimiter=",")
# S = loadtxt("paths1.csv", delimiter=",")

h = IV(S)   # inner value matrix

disccashflow = IV(S[Npoints+1,:])   # value matrix
for k in range(Npoints, 1, -1):
    Sk = S[k,:]
    intrinsicvalue = h[k,:]
    pick = intrinsicvalue > 0
    Y = df*disccashflow[pick]
    rg = polyfit(Sk[pick], Y, reg)                # regression at time k
    estimatedtimevalue  = polyval(rg, S[k,:])     # continuation values
    exercise = logical_and(pick, (intrinsicvalue > estimatedtimevalue))
    disccashflow = where(exercise, intrinsicvalue, disccashflow*df) # exercise decision
V0 = df*(sum(disccashflow)/Npaths) # LSM estimator

if K-S0 > V0:
    V0 = K-S0

# ## Output
# print("Time elapsed in Seconds   %8.3f" % (time()-t0))
# print("-----------------------------------------")
# print("Right Value               %8.3f" % V0_right)
# print("-----------------------------------------")
# print("LSM Value for Am. Option  %8.3f" % V0)
# print("Absolute Error            %8.3f" % (V0-V0_right))
# print("Relative Error in Percent %8.3f" % ((V0-V0_right)/V0_right * 100))
# print("-----------------------------------------")

print(V0)
print(1.96*std(df*disccashflow)/sqrt(Npaths))
