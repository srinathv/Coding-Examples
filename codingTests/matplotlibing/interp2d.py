

# Example 2-d interpolation with interp2d


import numpy as np
from scipy import interpolate
import pylab as py

def func(x,y):
  return (x*y)*np.exp(-5.0*(x**2+y**2))

x,y = np.mgrid[-1:1:15j,-1:1:15j]
fvals=func(x,y)

newfunc= interpolate.interp2d(x,y,fvals, kind='cubic')


#Notice that we evaluate on the new 1-d arrays
xnew = np.linspace(-1,1,100)
ynew=xnew

fnew=newfunc(xnew,ynew)


py.figure(1)
py.clf()
py.imshow(fnew, extent=[-1,1,-1,1],cmap=py.cm.jet)


from enthought.mayavi import mlab

#Need fully-flesehd out gride for surface

x2,y2=np.mgrid[-1:1:100j,-1:1:100j]

mlab.clf()
mlab.surf(x2,y2,fnew*2)
