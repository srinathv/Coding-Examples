#!/usr/bin/env python

import numpy
import optparse

def setArray1(nSize):
     """ Return 2 numpy arrays, each created in different ways """
     # x->[0, 2 * pi]
     x=numpy.arange(nSize)*2*numpy.pi/(nSize-1)
     y=numpy.sin(x)
     return x,y

def setArray2(nSize):
     """ Return 2 numpy arrays, each created in different ways """
     xlist=range(nSize) # This is of list type
     x=numpy.array(xlist)*2*numpy.pi/(nSize-1)
     # Create an empty array and fill it.  Note that default is double so
     # don't really need it here, but good for illustration
     y=numpy.zeros([nSize],numpy.double)
     for i in range(nSize): 
       y[i]=numpy.cos(x[i])
     return  x,y

def main():
    parser = optparse.OptionParser(usage="%prog [options]")
    parser.add_option('-n','--number', dest='number', default='20',
                      help='Size of arrays to use in examples')


    options, args = parser.parse_args()

    # Process arguments
    if len(args) >= 1:
      parser.print_usage()
      return
    nArrSize=numpy.int(options.number)

    # Return various arrays that come from different places.
    x1,y1=setArray1(nArrSize)
    x2,y2=setArray2(nArrSize)

    print "    x1        y1         x2      y2 "
    for i in range(nArrSize): 
        print  x1[i], y1[i], x2[i], y2[i]

if __name__ == "__main__":
        main()
