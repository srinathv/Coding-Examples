import numpy
s=numpy.arange(10)
t=numpy.zeros( [2,20], numpy.int)
t[0,:]=numpy.arange(20)
t[1,5:15]=s
print t.shape
print t
