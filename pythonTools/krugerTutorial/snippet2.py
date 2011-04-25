# Create x as a list and then convert
from snippet1 import *
xlist=range(10)
type(xlist)
x=numpy.array(xlist)
type(x)
x.dtype
# Create an empty array and fill it.  Note that default is double so
# don't really need it here, but good for illustration
yp=numpy.zeros([10],numpy.double)
for i in range(10): yp[i]=numpy.cos(xp[i])
