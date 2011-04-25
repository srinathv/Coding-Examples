#!/bin/env python

import numpy
import os
from configobj import ConfigObj
from matplotlib import pylab
import tables
import optparse

class simpleWall:
    def __init__(self,configFile):

        # Get the data which is in a configObj format
        if os.path.exists(configFile):
           swConfig = ConfigObj(configFile)
        else:
           print "Error: "+configFile+ "not found."
           return 0
        # There is only one key
        self.configFile=configFile
        self.swInstance=swConfig.keys()[0]
        self.machine=swConfig[self.swInstance]["machine"]
        self.nWall=numpy.int(swConfig[self.swInstance]["nWall"])
        nw=self.nWall

        RwList=swConfig[self.swInstance]["Rwall"]
        ZwList=swConfig[self.swInstance]["Zwall"]

        ## Unfortunately configObj lists don't come in with members as
        # numbers
        Rw = numpy.zeros( nw, numpy.float)
        Zw = numpy.zeros( nw, numpy.float)
        for i in range(nw):
          Rw[i]=numpy.float(RwList[i])
          Zw[i]=numpy.float(ZwList[i])

        # Ensure that the order is CCW AND that the zeroth element is at
        # R=Rmax.  This makes algorithms using simpleWall easier.
        # First find where Rmax is located
        for i in range(nw):
          if Rw[i] == Rw.max():
            irmax=i
            iprev=irmax-1
            if iprev==-1: 
              iprev=nw-1

        # Figure out the mapping of the new order
        ioList=range(nw)
        if Zw[irmax]<Zw[iprev]:
          ioList.reverse()
          irmax=nw-irmax-1
        io= numpy.array(ioList)
        iorder= numpy.zeros( nw, numpy.int)
        iorder[0:nw-irmax]=io[irmax:nw]
        iorder[nw-irmax:nw]=io[0:irmax]
          
        if Rw[iorder[0]]!=Rw[iorder[nw-1]] or Zw[iorder[0]]!=Zw[iorder[nw-1]]:
          self.nWall=self.nWall+1
          makePeriodic=True
        else:
          makePeriodic=False


        self.Rwall = numpy.zeros( self.nWall, numpy.float)
        self.Zwall = numpy.zeros( self.nWall, numpy.float)
        for i in range(nw):
           self.Rwall[i] = Rw[iorder[i]]
           self.Zwall[i] = Zw[iorder[i]]
        if makePeriodic:
           self.Rwall[nw]=self.Rwall[0] 
           self.Zwall[nw]=self.Zwall[0] 

    def printStdout(self):
          """ Print the data to standard output."""
          print "nWall ", self.nWall
          print "Rwall, Zwall ", self.nWall
          for i in range(self.nWall):
              print self.Rwall[i], self.Zwall[i]

    def plot2d(self):
          pylab.plot(self.Rwall,self.Zwall, linestyle="-", color="red")
          pylab.axis('equal')
          pylab.xlabel('R (m)')
          pylab.ylabel('Z (m)')
          wallTitle="Simple wall from "+self.swInstance
          pylab.title(wallTitle)
          pylab.show()

    def dumpH52D(self,h52dfile=""):
          """ Dump the simple wall data to an h5file. 
              If the file name is not given as an argument, the default
              file name is swInstance+"_2d.h5"
          """
          if h52dfile=="":
                h52dfile=self.swInstance+"_2d.h5"
          h5file=tables.openFile(h52dfile, mode="w", title="Simple wall description")
          h5file.createArray(h5file.root,'Rwall', self.Rwall)
          h5file.createArray(h5file.root,'Zwall', self.Zwall)
          h5file.setNodeAttr(h5file.root,"nWall", self.nWall)
          h5file.setNodeAttr(h5file.root,"configFile", self.configFile)
          h5file.setNodeAttr(h5file.root,"swInstance", self.swInstance)
          h5file.close()


    def dumpH53D(self,h53dfile="",nPhi=20):
          """ Calculate the positions in of a wall in 3D configuration
              space and then dump the data in h5 file format for easy 
              visualization in a program like VISIT. 
              The number of toroidal slices is given as an argument:
              nPhi.  The default is 20 slices.
              If the file name is not given as an argument, the default
              file name is swInstance+"_3D.h5"
          """
          if h53dfile=="":
                h53dfile=self.swInstance+"_3D.h5"
          h5file=tables.openFile(h53dfile, mode="w", title="Simple wall description")
          h5file.setNodeAttr(h5file.root,"configFile", self.configFile)
          h5file.setNodeAttr(h5file.root,"swInstance", self.swInstance)
          h5file.setNodeAttr(h5file.root,"configFile", self.configFile)

          # Create a 3D mesh and dump with vsSchema attributes
          wallMesh = numpy.zeros( (self.nWall, nPhi,1,3), numpy.float)
          for i in range(nPhi):
              phi=numpy.float(i)*2.*numpy.pi/numpy.float(nPhi-1)
              wallMesh[:,i,0,0]=self.Rwall[:]*numpy.cos(phi)
              wallMesh[:,i,0,1]=self.Rwall[:]*numpy.sin(phi)
              wallMesh[:,i,0,2]=self.Zwall[:]
          wset=h5file.createArray(h5file.root,'wallMesh', wallMesh)
          h5file.setNodeAttr(wset,"vsType", "mesh")
          h5file.setNodeAttr(wset,"vsKind", "structured")
          h5file.close()

def main():
    parser = optparse.OptionParser(usage="%prog [options] configFile")
    parser.add_option('-i', '--input', dest='input',
                      help='Name of simple Wall configuration file if not specified as argument.',
                      default='')
    parser.add_option('-p', '--print', dest='doPrint',
                      help='Print all possible fields to standard out along with description',
                      action='store_true')
    parser.add_option('-l', '--plot', dest='doPlot',
                      help='Make 2D plot of the simple wall', action='store_true')
    parser.add_option('-a', '--all', dest='doAll', action='store_true',
                      help='Write out all hdf5 output files (using default names if not specified by other options).',
                      default='')
    parser.add_option('--h52D', dest='h52Dname', default='',
                      help='Name of 2D output file (.h5 format).')
    parser.add_option('--h53D', dest='h53Dname', default='',
                      help='Name of HDF5 3D file to write data')
    parser.add_option('-n','--nphi', dest='nphi', default='20',
                      help='Number of toroidal phi planes to use in writing HDF5 3D file')

    options, args = parser.parse_args()

    # Too many arguments
    if len(args) > 2:
      parser.print_usage()
      return
    elif len(args) == 2:
      configFile=args[0]
      configShotFile=args[1]
    # No arguments
    else:
      if options.input == '':
        print "Must specify an configuration file"
        return
      else:
        configFile=options.input

    sw=simpleWall(configFile)

    # If output not written, then just write to stdout:
    if options.doPrint:
      sw.printStdout()

    printH52D=False; printH53D=False
    if options.doAll: 
      printH52D=True; printH53D=True

    nPhiIn=numpy.int(options.nphi)

    if options.h52Dname: 
        sw.dumpH52D(h52dfile=h52Dname)
    elif printH52D:
        sw.dumpH52D()
    if options.h53Dname: 
        sw.dumpH53D(h53dfile=h53Dname,nPhi=nPhiIn)
    elif printH53D:
        sw.dumpH53D(nPhi=nPhiIn)

    if options.doPlot: 
        sw.plot2d()

if __name__ == "__main__":
        main()
