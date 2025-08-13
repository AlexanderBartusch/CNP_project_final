from os import getpid
from time import sleep
from math import sin, cos, sqrt, atan2, radians
import numpy as np

def worker(x):
    return x*x
def double(i):
    print("I'm process", getpid())
    sleep(1)
    return i * 2

def fundist(x, y):
    """Function to compute the distance in km between two points x , y: (lon,lat)"""
    lon1 = radians(x[0])
    lat1 = radians(x[1])
    lon2 = radians(y[0])
    lat2 = radians(y[1])

    R = 6373.0

    dlon = lon2 - lon1
    dlat = lat2 - lat1

    a = sin(dlat / 2) ** 2 + cos(lat1) * cos(lat2) * sin(dlon / 2) ** 2
    c = 2 * atan2(sqrt(a), sqrt(1 - a))

    distance = R * c

    return round(distance, 4)

def proc(input_):
    start, k_step, k_max, n, data = input_
    dist = []
    k1 = start
    k2 = min(start + k_step, k_max)
    for k in range(k1, k2):
        # get (i, j) for 2D distance matrix knowing (k) for 1D distance matrix
        i = int(n - 2 - int(np.sqrt(-8 * k + 4 * n * (n - 1) - 7) / 2.0 - 0.5))
        j = int(k + i + 1 - n * (n - 1) / 2 + (n - i) * ((n - i) - 1) / 2)
        # store distance
        a = data[i, :]
        b = data[j, :]
        d = fundist(a, b)
        dist.append(d)
    return k1, k2, dist