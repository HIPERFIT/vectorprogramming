from math import sqrt, trunc
from functools import reduce
n_directions = 32

dirvs = [[2147483648,1073741824,2684354560,1342177280,
          2281701376,603979776,301989888,754974720,
          1988100096,2654994432,136314880,1678770176,
          2988965888,2098462720,4272029696,3125346304,
          438599680,1226522624,3300237312,3816001536,
          4135585792,3728737280,2820672000,873465088,
          975702144,1494483520,3970040096,2538144464,
          1822721896,3613084132,3432358018,2271450689],

         [2147483648,1073741824,3758096384,2952790016,
          2550136832,2483027968,2315255808,1526726656,
          864026624,3653238784,1914699776,1058013184,
          3250061312,2800484352,1401290752,703922176,
          171606016,455786496,3549618176,1778348032,
          3929540608,2871788544,1269173760,4259646208,
          1610779008,4026976576,2016733344,605713840,
          305826616,3475687836,3113412898,2197780721]]

def normalise(x):
    return float(x) / pow(2.0, n_directions)

def bitVector(e):
    def isbitset(i):
        return ((e & (1 << i)) > 0)*1
    return map(isbitset, range(0,n_directions))

def sobol(v,i):
    return normalise(reduce(lambda a,b: a ^ b, map(lambda a,b: a*b, v, bitVector(i))))

def sobol1D(v,m):
    return map(lambda i: sobol(v, i), range(1,m+1))

def sobolND(vs,m):
    return map(lambda v: sobol1D(v, m), vs)

def pi2d(sobols):
    xs = sobols[0]
    ys = sobols[1]
    n = len(xs)
    dists = map(lambda x,y: trunc(sqrt (pow(x,2) + pow(y,2))), xs, ys)
    return 4 * (float(n - sum(dists))/ n)
