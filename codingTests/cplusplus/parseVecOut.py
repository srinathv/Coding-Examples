#! /usr/bin/env python

#  parser.add_argument('integers', metavar='N', type=int, nargs='+',
#                   help='an integer for the accumulator')
#this above syntax is for positional arguments
  #parser.add_argument('-r','--rundir', , action='store_const', ### dest defaults to --<name> if --<name> is used

#exampleVec
#This is vector push_back time
# 0.141068s wall, 0.050000s user + 0.080000s system = 0.130000s CPU (92.2%)
#This is boost::container::vector push_back time
# 0.160467s wall, 0.070000s user + 0.090000s system = 0.160000s CPU (99.7%)
#This is vector push_back time
# 0.137892s wall, 0.040000s user + 0.090000s system = 0.130000s CPU (94.3%)
#This is boost::container::vector push_back time
# 0.158518s wall, 0.060000s user + 0.090000s system = 0.150000s CPU (94.6%)



import sys,os,getopt,argparse,math
import numpy as np


def main():
  parser = argparse.ArgumentParser(description='Given a directory with multiple post-indexed HommeTime files, this will '\
                                                'calculate averages and standard deviations.  Also, a histogram will be generated')
  parser.add_argument('-r','--rundir', dest='rundir', default='.',
                      help='Name of directory.')

  parser.add_argument('-n','--numbins', default=50,type=int,
                      help='Number of bins for historgram.')

  parser.add_argument('-f','--filename', default=None,
                      help='Name of vector out file.')

  parser.add_argument('-fg','--figurename', default=None,
                      help='Name of histogram figure.')

  parser.add_argument('-t','--figuretitle', default="NE=3, 1 mpi rank at full device thread use" ,
                      help='Title on histogram figure.')

  parser.add_argument('-g','--grouptime', default="prim_run",
                      help='Group timing desired.')

  parser.add_argument('-z','--zToRundir', dest='zToRundir', default=None,
                      help='Name of second directory of which to calculate z score.')

  parser.add_argument('-p','--plot',action="store_true",
                      help='Will plot.  If -f is given then figurename is used for saved figure.')
  parser.add_argument('-per','--plotpercent',action="store_true",
                      help='Distribution plots Std as percentage.')

  args = parser.parse_args()

  if (args.plot) or (args.figurename):
    try:
      import matplotlib.pyplot as py
    except:
      print "Error: no pyplot with matplotlib"
      sys.exit(1)

#initialize 2 arrays 
  stdVec=np.array([])
  bstVec=np.array([])
#open file
  vecFile=open(args.filename,'r')  
#read lines and add to arrays
  ivecFile=iter(vecFile)
  for line in ivecFile:
    print line
    if ('vector push_back' in line):
      line=next(ivecFile)
      time=line.split()[0].strip('s')
      if ('boost' in line): 
        bstVec=np.append(bstVec,time)
      else:
        stdVec=np.append(stdVec,time)

  print stdVec

        
#if line has "vector push_back" add next line first number (strip s) to a vector
#if line has "boost::container::vector push_back" add next line first number (strip s) to other vector


if __name__ == "__main__":
   main()



