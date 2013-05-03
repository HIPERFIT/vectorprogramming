# Example taken from the Copperhead repository
# https://github.com/copperhead/copperhead/blob/master/samples/black_scholes.py
from copperhead import *
import numpy as np

@cu
def cnd(d):
    A1 = 0.31938153
    A2 = -0.356563782
    A3 = 1.781477937
    A4 = -1.821255978
    A5 = 1.330274429
    RSQRT2PI = 0.39894228040143267793994605993438

    K = 1.0 / (1.0 + 0.2316419 * abs(d))

    cnd = RSQRT2PI * exp(- 0.5 * d * d) * \
        (K * (A1 + K * (A2 + K * (A3 + K * (A4 + K * A5)))))

    if d > 0:
        return 1.0 - cnd
    else:
        return cnd

@cu
def black_scholes(IsCall, S, X, T, R, V):
    def black_scholes_el(isCall, si, xi, ti):
        sqrt_ti = sqrt(ti)
        d1 = (log(si/xi) + (R + .5 * V * V) * ti) / (V * sqrt_ti)
        d2 = d1 - V * sqrt_ti
        cnd_d1 = cnd(d1)
        cnd_d2 = cnd(d2)
        exp_Rti = exp(-R * ti)
        if isCall:
            return si * cnd_d1 - xi * exp_Rti * cnd_d2;
        else:
            return xi * exp_Rti * (1.0 - cnd_d2) - si * (1.0 - cnd_d1)
    return map(black_scholes_el, IsCall, S, X, T)

options = [(True, 60.0, 65.0, 1.0)]

# Convert to cuarrays
(opttype,S,X,T) = unzip(options)
IsCall = cuarray(np.array(opttype, dtype=np.bool))
s0s = cuarray(np.array(S, dtype=np.double))
strikeprices = cuarray(np.array(X, dtype=np.double))
expiries = cuarray(np.array(T, dtype=np.double))

print(black_scholes(IsCall,s0s,strikeprices,expiries,0.1,0.2))
