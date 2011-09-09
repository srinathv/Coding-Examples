from netCDF4 import Dataset
import numpy as np
import pylab as py
import sys, os, re

files=sys.argv


#read in regrid from from libcf

rootgrp=Dataset('tst_lonlat_to_lonlat_regrid_weights.nc')

#get  temp weight matrix
libcfWeightsTemp=np.zeros(rootgrp.variables['weights'].shape)
#libcfWeightsTemp.shape[0] is number of destination nodes
#libcfWeightsTemp.shape[1] gives the stencil/link size

# load up libcfWeights
for i in range(libcfWeightsTemp.shape[0]):
  for j in range(libcfWeightsTemp.shape[1]):
    libcfWeightsTemp[i,j]=rootgrp.variables['weights'][i,j]


#load up libcfIndices
libcfIndicesTemp=np.zeros(rootgrp.variables['indices'].shape,dtype=np.int8)


# load up libcfIndices
for i in range(libcfIndicesTemp.shape[0]):
  for j in range(libcfIndicesTemp.shape[1]):
    libcfIndicesTemp[i,j]=rootgrp.variables['indices'][i,j]


#Make the true weight matrix such that W.x=y
#First find number of nodes of origin grid
nOrigNodes=libcfIndicesTemp.max()+1
libcfWeights=np.zeros( [libcfIndicesTemp.shape[0],nOrigNodes])

#load weights:
for i in range(libcfWeights.shape[0]):
  for j in range(libcfIndicesTemp.shape[1]):
    libcfWeights[i,libcfIndicesTemp[i,j]]=libcfWeights[i,libcfIndicesTemp[i,j]]+libcfWeightsTemp[i,j]


#fig1=py.figure()
py.matshow(libcfWeights)
py.colorbar()
#py.show()

####LOAD the SCRIP remap matrices
## I think SCRIP is a cell centered remapper.

#scripLinGrp=Dataset('rmp_longlat_2b2_to_4b4_lin.nc')
#scripConsGrp=Dataset('rmp_longlat_2b2_to_4b4_conserv.nc')

def scrip_remap_matrix(filename):
  scripGrp=Dataset(filename)
  numSrcCells=scripGrp.variables['src_grid_imask'].shape[0]
  numDstCells=scripGrp.variables['dst_grid_imask'].shape[0]

  scripWeights=np.zeros([numDstCells,numSrcCells])

  for i in range(scripGrp.variables['remap_matrix'].shape[0]):
    rowLoc=scripGrp.variables['dst_address'][i]-1
    colLoc=scripGrp.variables['src_address'][i]-1
    scripWeights[rowLoc,colLoc]=scripGrp.variables['remap_matrix'][i][0]

  return scripWeights



scripLinWeights=scrip_remap_matrix('rmp_longlat_2b2_to_4b4_lin.nc')
scripConsWeights=scrip_remap_matrix('rmp_longlat_2b2_to_4b4_conserv.nc')

#get the grid locations
orgGrp=Dataset('tst_lonlat_to_lonlat_grid_ori_scrip.nc')
desGrp=Dataset('tst_lonlat_to_lonlat_grid_tgt_scrip.nc')

#create function eval original grid.


#eval on original grid

#eval on destination grid

#apply interpolation weights to original grid evaluation to
#obtain interpolated values


#get absolute difference at each point


#sum up absolute differences


#plot absolute differenes at each point
