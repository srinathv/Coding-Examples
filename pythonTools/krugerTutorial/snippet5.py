from matplotlib import pylab
import numpy
# 2D line plot
r=4.
t=numpy.arange(20)*2*numpy.pi/(20-1)
x=r*numpy.cos(t)
y=r*numpy.sin(t)
pylab.plot(x,y)
pylab.axis('equal')
pylab.show()
# Contour plot
xg,yg=numpy.meshgrid(t,t)
z=numpy.cos(xg)*numpy.sin(yg)
print z.shape
pylab.imshow(z)
pylab.show()
