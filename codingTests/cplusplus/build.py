#! /usr/bin/env python

import sys,os,getopt,argparse


SOURCE=testVec.cpp
EXECUTABLE=testVec
FLAGS='-O2 -fPIC'
LIBS='/scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_system.a
       /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_timer.a
       /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_chrono.a
       /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_thread.a -lrt'
INCLUDES='-I/scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/include'

TAU_EXPORTS='export
             TAU_MAKEFILE=/work/02463/srinathv/tau2/x86_64/lib/Makefile.tau-intelImpiCuda.aac-icpc-papi-mpi-cupti-pdt;
             export TAU_OPTIONS="-optLinkOnly -optVerbose"'




#def main():
#  parser = argparse.ArgumentParser(description='Given a directory with multiple post-indexed HommeTime files, this will' +
#                                   'calculate averages and standard deviations.  Also, a histogram will be generated')
#
#  parser.add_argument('-r','--rundir', dest='rundir', default='.',
#                      help='Name of directory.')
#
#  parser.add_argument('-n','--numbins', default=50,type=int,
#                      help='Number of bins for historgram.')
#
#  parser.add_argument('-f','--filename', default=None,
#                      help='Name of vector out file.')
#
#  parser.add_argument('-fg','--figurename', default=None,
#                      help='Name of histogram figure.')
#
#  parser.add_argument('-t','--figuretitle', default="Distrbution of testVec trials." ,
#                      help='Title on histogram figure.')
#
#  parser.add_argument('-g','--grouptime', default="Wall clock",
#                      help='Group timing desired.')
#
#  parser.add_argument('-z','--zToRundir', dest='zToRundir', default=None,
#                      help='Name of second directory of which to calculate z score.')
#
#  parser.add_argument('-p','--plot',action="store_true",
#                      help='Will plot.  If -f is given then figurename is used for saved figure.')
#
#  parser.add_argument('-per','--plotpercent',action="store_true",
#                      help='Distribution plots Std as percentage.')
#
#  args = parser.parse_args()





if __name__ == "__main__":
   main()

