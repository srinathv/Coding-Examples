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

def calcAvg(array):
  avg =  np.average(array)
  std = np.std(array)  
  num=len(array)

  print "avg = ", avg
  print "std = ", std 
  print " % = ", std/avg * 100
  print "number of members = ", num
  return avg,std,num

def calcZScore(avg1,std1,num1,avg2,std2,num2):
  numerator=abs(avg1-avg2)
  denom=math.sqrt(((std1**2)/num1)+(std2**2)/num2)
  zScore=numerator/denom
  return zScore

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

  parser.add_argument('-t','--figuretitle', default="Distrbution of testVec trials." ,
                      help='Title on histogram figure.')

  parser.add_argument('-g','--grouptime', default="Standard Vector",
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
      import matplotlib
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
    if ('vector push_back' in line):
      nline=next(ivecFile)
      time=nline.split()[0].strip('s')
      if ('boost' in line): 
        bstVec=np.append(bstVec,float(time))
      else:
        stdVec=np.append(stdVec,float(time))

  print "*** standard vector stats"
  stdAvg,stdStd,stdNum=calcAvg(stdVec)
  
  print "*** boost container vector stats"
  bstAvg,bstStd,bstNum=calcAvg(bstVec)

  zScore=calcZScore(stdAvg,stdStd,stdNum,bstAvg,bstStd,bstNum)
  print zScore, "is the zScore > 3.22 => significance difference"

  if (args.plot) or (args.figurename):

      matplotlib.rcParams.update({'font.size': 6})
      
      fig1=py.figure(num=None, figsize=(8, 4), dpi=80, facecolor='w', edgecolor='k')
      ax1 = fig1.add_subplot(1,2,1,)
      n,bins,patches=ax1.hist(stdVec,bins=args.numbins)
      py.xlabel(args.grouptime + "[sec]")
      py.ylabel("Number of testVec trials")
      plotTitle = "std::vec " + args.figuretitle + "\n" + "Avg =" + str(stdAvg) + " [sec] "
      if args.plotpercent:
        plotTitle= plotTitle + ", Std % = " + str(stdStd/stdAvg * 100 )
      else:
        plotTitle= plotTitle + ", Std = " + str(stdStd)
      py.title(plotTitle )

      ax2 = fig1.add_subplot(1,2,2,)
      n,bins,patches=ax2.hist(bstVec,bins=args.numbins)
      py.xlabel(args.grouptime + "[sec]")
      py.ylabel("Number of testVec trials")
      plotTitle = "boost::container::vector "+ args.figuretitle + "\n" + "Avg =" + str(bstAvg) + " [sec] "
      if args.plotpercent:
        plotTitle= plotTitle + ", Std % = " + str(bstStd/bstAvg * 100 )
      else:
        plotTitle= plotTitle + ", Std = " + str(bstStd)
      py.title(plotTitle )


      if args.figurename:
        py.savefig(args.figurename)
      else:
        py.show()

if __name__ == "__main__":
   main()



