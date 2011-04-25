#!/usr/bin/env python

import numpy
import optparse
import numpyExamples 
from matplotlib import pylab

def makeXyPlot(x1,y1,x2,y2):
     """ Make an (x,y) plot with two different x,y functions"""
     xaxmin=numpy.min([x1,x2])
     xaxmax=numpy.max([x1,x2])
     yaxmin=1.2*numpy.min([y1,y2])
     yaxmax=1.2*numpy.max([y1,y2])
     pylab.axis([xaxmin,xaxmax,yaxmin,yaxmax])
     pylab.xlabel(" X (m) ")
     pylab.ylabel(" Signal (eV) ")
     pylab.plot(x1,y1,marker="+", label="y1",linestyle="-", color="red",ms=10) 
     pylab.plot(x2,y2,marker="o", label="y2",color="blue",ms=10) 
     pylab.show()
     return 

def make2Dline(N,r):
     """ Plot an circle of radius r"""
     t=numpy.arange(N)*2*numpy.pi/(N-1)
     x=r*numpy.cos(t)
     y=r*numpy.sin(t)
     pylab.axis('equal')
     pylab.plot(x,y)
     pylab.show()
     return 

def makeContour(x1,y1):
     """ Make a contour plot """ 
     xg,yg=numpy.meshgrid(x1,y1)
     z=numpy.cos(xg)*numpy.sin(yg)
     pylab.imshow(z)
     pylab.show()
     return 

def main():
    parser = optparse.OptionParser(usage="%prog [options]")
    parser.add_option('-n','--number', dest='number', default='20',
                      help='Size of arrays to use in examples')
    parser.add_option('-x', '--xy', dest='plotXy',
                      help='Make an xy plot', action='store_true')
    parser.add_option('-l', '--line2d', dest='plot2d',
                      help='Make an 2D line plot', action='store_true')
    parser.add_option('-c', '--contour', dest='plotContour',
                      help='Make an 2D contour', action='store_true')

    options, args = parser.parse_args()

    # Process arguments
    if len(args) >= 1:
      parser.print_usage()
      return
    nArrSize=numpy.int(options.number)

    # Return various arrays that come from different places.
    x1,y1=numpyExamples.setArray1(nArrSize)
    x2,y2=numpyExamples.setArray2(nArrSize)

    if options.plotXy:
      makeXyPlot(x1,y1,x2,y2)

    if options.plot2d:
      make2Dline(nArrSize,4.)

    if options.plotContour:
      makeContour(x1,y1)

if __name__ == "__main__":
        main()
