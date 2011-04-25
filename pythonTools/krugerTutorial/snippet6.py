from matplotlib import pylab
import numpy
rdArray=pylab.loadtxt('dataFile1.txt',comments='x1')
rdArray.shape
x1=rdArray[:,0]
y1=rdArray[:,1]
x2=rdArray[:,2]
y2=rdArray[:,3]
from plotExamples import *
makeXyPlot(x1,y1,x2,y2)


