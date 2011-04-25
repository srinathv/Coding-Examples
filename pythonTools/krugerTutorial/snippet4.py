from matplotlib import pylab
from snippet2 import *
pylab.plot(x,y)
pylab.show()
xaxmin=x.min(); xaxmax=x.max(); yaxmin=1.2*y.min(); yaxmax=1.2*y.max()
pylab.axis([xaxmin,xaxmax,yaxmin,yaxmax])
pylab.xlabel(" X (m) ")
pylab.ylabel(" Signal (eV) ")
pylab.plot(x,y)
pylab.plot(xp,yp,marker="+", linestyle="-", color="red",ms=10) 
pylab.show()

