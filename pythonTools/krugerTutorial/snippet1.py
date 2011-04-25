import numpy
x=numpy.arange(10)
type(x)
x.dtype
# Create periodic grid
xp=numpy.arange(10)*2*numpy.pi/9
print xp
xp.dtype
# Create numpy array via an operation on existing array
y=numpy.sin(x)
type(y)

