#!/usr/bin/env python

import numpy
import optparse
from numpyExamples import *
from plotExamples import *

def main():
    parser = optparse.OptionParser(usage="%prog [options] inputFile")
    parser.add_option('-i', '--input', dest='input',
                      help='Name of input file if not specified as argument.',
                      default='')

    options, args = parser.parse_args()

    # Process arguments
    if len(args) > 1:
      parser.print_usage()
      return
    elif len(args)==1:
      inputFile=args[0]
    else:
      if options.input == '':
        print "Must specify an input file"
        return
      else:
        inputFile=options.input
    
    rdArray=pylab.loadtxt('dataFile1.txt',comments='x1')
    rdArray.shape
    x1=rdArray[:,0]
    y1=rdArray[:,1]
    x2=rdArray[:,2]
    y2=rdArray[:,3]
    makeXyPlot(x1,y1,x2,y2)
    return

if __name__ == "__main__":
        main()
