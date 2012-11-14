# Valuation of American Put Option
# with Least-Squares Monte Carlo

from numpy import *
from numpy.random import standard_normal, seed
from time import time
t0 = time ()
## Simulation Parameters
seed(150000)       # seed for Python RNG
M   = 50           # time steps
I   = 4*4096       # paths for valuation
reg = 9            # no of basis functions
AP  = True         # antithetic paths
MM  = True         # moment matching of RN

## Parameters -- American Put Option                               
r   = 0.06         # short rate                                      
vol = 0.2          # volatility                                      
S0  = 36.0         # initial stock level                             
T   = 1.0          # time - to - maturity                            
V0_right = 4.48637 # American Put Option ( 500 steps bin . model )   
dt  = T/M          # length of time interval                         
df  = exp(-r*dt)   # discount factor per time interval


def RNG(I):
    """Generate I random numbers following specified AP and MM parameters"""
    if AP == True:
        ran = standard_normal(I/2)
        ran = concatenate((ran, -ran))
    else:
        ran = standard_normal(I)
    if MM == True:
        ran = ran-mean(ran)
        ran = ran/std(ran)
    return ran

def GenS(I):
    S = zeros((M+1, I), 'd')   # index level matrix
    S[0,:] = S0                # initial values
    for t in range(1, M+1, 1): # index level paths
        ran = RNG(I)
        S[t,:]= S[t-1,:] * exp((r-vol**2/2)*dt + vol*ran*sqrt(dt))
    return S

def IV(S):
    return maximum(40.0-S,0)

## Valuation by LSM
S = GenS(I) # generate stock price paths
h = IV(S)   # inner value matrix
V = IV(S)   # value matrix
for t in range(M-1, -1, -1):
    rg = polyfit(S[t,:], V[t+1,:]*df, reg)          # regression at time t
    C  = polyval(rg, S[t,:])                        # continuation values
    V[t,:] = where(h[t,:] > C, h[t,:], V[t+1,:]*df) # exercise decision
V0 = sum(V[0,:])/I # LSM estimator

## Output
print("Time elapsed in Seconds   %8.3f" % (time()-t0))
print("-----------------------------------------")
print("Right Value               %8.3f" % V0_right)
print("-----------------------------------------")
print("LSM Value for Am. Option  %8.3f" % V0)
print("Absolute Error            %8.3f" % (V0-V0_right))
print("Relative Error in Percent %8.3f" % ((V0-V0_right)/V0_right * 100))
print("-----------------------------------------")
