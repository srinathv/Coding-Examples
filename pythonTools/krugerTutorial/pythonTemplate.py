#!/usr/bin/env python

import numpy
import optparse

def something(nfoo, file):
     return 

def main():
    parser = optparse.OptionParser(usage="%prog [options] inputFile")
    parser.add_option('-i', '--input', dest='input',
                      help='Name of input file if not specified as argument.',
                      default='')
    parser.add_option('-d', '--do', dest='doSomething',
                      help='Set a logical variable (default is true) ',
                      action='store_true')
    parser.add_option('-n','--number', dest='number', default='20',
                      help='Number of something to control something else')


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

    # If output not written, then just write to stdout:
    if options.doSomething:
      something(options.number,inputFile)

if __name__ == "__main__":
        main()
