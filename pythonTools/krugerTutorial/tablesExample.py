#!/usr/bin/env python

import numpy
import optparse
from matplotlib import pylab
import tables

def convertTxtFile(iFile,oFile):
    hf=tables.openFile(oFile,'w')
    rdArray=pylab.loadtxt(iFile,comments='x1')
    rdArray.shape
    # It'd be nice to grab the names from the file, but it should be
    # obvious how one would do so
    for idx in range(rdArray.shape[1]):
       name="x"+str(idx)
       hf.createArray(hf.root,name,rdArray[:,idx])
     
    hf.close()
    return

def main():
    parser = optparse.OptionParser(usage="%prog [options] inputFile outputFile")

    options, args = parser.parse_args()

    # Process arguments
    if not len(args) == 2:
      parser.print_usage()
      return
    else:
      inputFile=args[0]
      outputFile=args[1]
    
    convertTxtFile(inputFile,outputFile)
    return

if __name__ == "__main__":
        main()
