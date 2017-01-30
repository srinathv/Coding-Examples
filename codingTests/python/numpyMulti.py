import numpy as np
import os
from timeit import timeit
from multiprocessing import Pool


def mmul(matrix):
    for i in range(100):
        matrix = matrix * matrix
    return matrix

if __name__ == '__main__':

    matrices = []
    for i in range(4):
        matrices.append(np.random.random_integers(100, size=(1000, 1000)))

    print timeit(lambda: map(mmul, matrices), number=20)

    # after importing numpy, reset the CPU affinity of the parent process so
    # that it will use all cores
    #os.system("taskset -p 0xff %d" % os.getpid())

    pool = Pool(8)
    print timeit(lambda: pool.map(mmul, matrices), number=20)
