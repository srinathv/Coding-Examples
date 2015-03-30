#! /usr/bin/env python

import sys,os,getopt,argparse
import subprocess,logging

def shellCommand(command,errorMessage):
#command initiated where this script is ran
  try:
    print command
    subprocess.check_call(command, stderr=subprocess.STDOUT, shell=True)
  except :
    print errorMessage
    pass
  return

BASE=' testVec'
EXE_BASE='testVec_'
BASE_FLAGS=' -O2 -fPIC '
LIBS=' -L./. -lcupti -lcuda /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_system.a \
    /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_timer.a \
    /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_chrono.a \
    /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_thread.a -lrt '
INCLUDES=' -I/scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/include '
COMPILER='icpc '
TAU_COMPILER='tau_cxx.sh '

TAU_EXPORTS='export \
             TAU_MAKEFILE=/work/02463/srinathv/tau2/x86_64/lib/Makefile.tau-intelImpiCuda.aac-icpc-papi-mpi-cupti-pdt; \
             export TAU_OPTIONS="-optLinkOnly -optVerbose"'
OMP_FLAG=' -DOMP -openmp '




def main():
  parser = argparse.ArgumentParser(description='Build testVec for different implemenations.')

  parser.add_argument('-n','--numelems', default=1000000,type=int,
                      help='Number of elemens of arrays.')

  parser.add_argument('-t','--tau',action="store_true",
                      help='Profile with TAU')
  
  parser.add_argument('-sv','--stdvec',action="store_true",
                      help='Do std::vector test.')

  parser.add_argument('-bv','--boostvec',action="store_true",
                      help='Do boost::container::vector test.')

  parser.add_argument('-sq','--stddeq',action="store_true",
                      help='Do std::deque test.')
  
  parser.add_argument('-d','--debug',action="store_true",
                      help='Debug log')

  parser.add_argument('-o','--openmp',action="store_true",
                      help='Compile with OpenMP')
  args = parser.parse_args()

  if (args.debug):
   logging.basicConfig(level=logging.DEBUG)

  IFDEF=' '
  FLAGS= BASE_FLAGS

  if (args.tau):
     IFDEF = IFDEF + '-DUSE_TAU '
  if (args.stdvec):
     IFDEF = IFDEF + '-DVEC '
  if (args.boostvec):
     IFDEF= IFDEF + '-DBOOST_VEC '
  if (args.stddeq):
     IFDEF = IFDEF + '-DDEQ '
  if (args.numelems):
     IFDEF = IFDEF + '-DSET_N='+str(args.numelems)
  logging.debug('IFDEF is ' + IFDEF)


  if (args.openmp):
     FLAGS = FLAGS + OMP_FLAG 

  
#build commands
  if (args.tau):
    EXE_NAME=EXE_BASE+'tau_N'+str(args.numelems)
    COMMAND=TAU_EXPORTS + '; '
    COMMAND=COMMAND + TAU_COMPILER + BASE+'.cpp' + ' -c ' + FLAGS + INCLUDES + IFDEF
    COMMAND = COMMAND + ' ; ' + TAU_COMPILER + BASE+'.o -o ' + EXE_NAME + FLAGS + LIBS
  else:
    EXE_NAME=EXE_BASE+'N_'+str(args.numelems)
    COMMAND=COMPILER + BASE+'.cpp' + FLAGS + INCLUDES + IFDEF + LIBS +  ' -o ' + EXE_NAME

  logging.debug('COMMAND is ' + COMMAND)
  if not (args.debug):
    errorMsg=' trouble building'
    shellCommand(COMMAND,errorMsg)


if __name__ == "__main__":
   main()
