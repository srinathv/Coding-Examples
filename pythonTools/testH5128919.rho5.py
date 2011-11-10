#!/usr/bin/env python

from synthModules import psfObj, wedgeObj,  psfWedgeFuncs as pF
import tables
import pylab as py
import os

timeStep=.005
#3.61871383E+05  csD/a (1/s)

psf1=psfObj.psfField('psf.sav')
psf1Norm=psf1.calcNorm()
print "psf norm on its grid = ", psf1Norm
#psf1.plotSurface(1)
psf1List=psfObj.putPsfFieldAtBesCords(psf1,'diiidBES.conf','diiidBESshotData.conf')
psfObj.dumpPsfListBesLoctoH5(psf1List,"besloc.h5")
#now that we have the bes and psf loaded lets load up the wedge data
# set directory paths
path='.'
dirlist=os.listdir(path)
#make list of wedge file numbers
numList=[]
for files in dirlist:
  if ('gyrowedge' in files):
    num=files.split('gyrowedge')[1].split('.h5')[0]
    numList.append(num)
#list is done now set up dictionary and set values

wedgeDict={}
psfOnWedgeList=[]
for i in numList:
  fileName='gyrowedge'+i+'.h5'
  wedgeDict['wedge'+i]=wedgeObj.wedgeField(fileName)
  wedgeFile=tables.openFile(fileName)
  tempWedge=wedgeDict['wedge'+i] 
  tempWedge.setValues(wedgeFile.root.density_electron_toroidal.density_electron[:,:,0],"density_electron_torangle_0")
  wedgeFile.close()
  psfOnWedgeDict={}
  for j in range(0,len(psf1List)):
    psfOnWedgeDict['psfWedge'+str(j)]=pF.psfInterp2WedgeRZGrid(psf1List[j],wedgeDict['wedge'+i])
    print "psfOnWedge for i=",i," and bes index ",j
  psfOnWedgeList.append(psfOnWedgeDict)

#wedge1=wedgeObj.wedgeField('gyrowedge02000.h5')
#
#wedgefile=tables.openFile('gyrowedge02000.h5')
#wedge1.setValues(wedgefile.root.density_electron_toroidal.density_electron[:,:,0],"density_electron_torangle_0")
#wedgefile.close()

#wedge1.plotRZContour(2)
#wedge1.plotRminThetaContour(3)


#psf1Wedge.plotRZContour(4)
#psf1Wedge.plotRminThetaContour(5)
#
#synDenElecJac,besRindex,besZindex=pF.synthSignalviaSumJac(psf1Wedge,wedge1)
#print "synDenElecJac,besRindex,besZindex =", synDenElecJac,besRindex,besZindex
#synDenElecNoJac,besRindexNoJac,besZindexNoJac=pF.synthSignalviaSumNoJac(psf1Wedge,wedge1)
#print "synDenElecNoJac,besRindexNoJac,besZindexNoJac =", synDenElecNoJac,besRindexNoJac,besZindexNoJac
