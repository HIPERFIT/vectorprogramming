from math import sqrt, trunc
from copperhead import *
import numpy as np

# dirvs = cuarray(np.array([[2147483648,1073741824,2684354560,1342177280,
#                    2281701376,603979776,301989888,754974720,
#                    1988100096,2654994432,136314880,1678770176,
#                    2988965888,2098462720,4272029696,3125346304,
#                    438599680,1226522624,3300237312,3816001536,
#                    4135585792,3728737280,2820672000,873465088,
#                    975702144,1494483520,3970040096,2538144464,
#                    1822721896,3613084132,3432358018,2271450689],

#                   [2147483648,1073741824,3758096384,2952790016,
#                    2550136832,2483027968,2315255808,1526726656,
#                    864026624,3653238784,1914699776,1058013184,
#                    3250061312,2800484352,1401290752,703922176,
#                    171606016,455786496,3549618176,1778348032,
#                    3929540608,2871788544,1269173760,4259646208,
#                    1610779008,4026976576,2016733344,605713840,
#                    305826616,3475687836,3113412898,2197780721]]
#                  ,dtype=np.int))


dirvs0 = cuarray(np.array([2147483648,1073741824,2684354560,1342177280,
                           2281701376,603979776,301989888,754974720,
                           1988100096,2654994432,136314880,1678770176,
                           2988965888,2098462720,4272029696,3125346304,
                           438599680,1226522624,3300237312,3816001536,
                           4135585792,3728737280,2820672000,873465088,
                           975702144,1494483520,3970040096,2538144464,
                           1822721896,3613084132,3432358018,2271450689]
                          ,dtype=np.int64))


@cutype("Int -> Float")
@cu
def normalise(x):
    twoPow32 = 4294967296
    return float32(x) / twoPow32 # pow(2.0, n_directions)

@cutype("Int -> [Int]")
@cu
def bitVector(e):
    n_directions = 32
    def isbitset(i):
        if (e & (1 << i)) > 0:
            return 1
        else:
            return 0
    return map(isbitset, range(n_directions))

@cutype("([Int], Int) -> Float")
@cu
def sobol(v,i):
    return normalise(reduce(op_xor,(map(op_mul, v, bitVector(i))),0))

@cutype("([Int], [Int]) -> [Float]")
@cu
def sobol1D(v,m):
    def f(i):
        return sobol(v,i)
    return map(f, m)

# Could the problem be something about using int64?
arr = cuarray(np.array([0,1,2,3,4,5], dtype=np.int64))

print(sobol1D(dirvs0,arr))

# @cu
# def sobolND(vs,m):
#     return np.vstack(imap(lambda v: sobol1D(v, m), vs))

# def pi2d(sobols):
#     xs = sobols[0]
#     ys = sobols[1]
#     n = len(xs)
#     dists = np.fromiter(imap(lambda x,y: trunc(sqrt (pow(x,2) + pow(y,2))), xs, ys), dtype=int)
#     return 4 * (float(n - np.sum(dists))/ n)