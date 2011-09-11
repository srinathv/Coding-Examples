from netCDF4 import Dataset
import numpy as np
import pylab as py
import sys, os, re
import math as mt

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
#orgGrp=Dataset('tst_lonlat_to_lonlat_grid_ori_scrip.nc')
#desGrp=Dataset('tst_lonlat_to_lonlat_grid_tgt_scrip.nc')

def get_vertices(filename):
  libcfGrp=Dataset(filename)
  tempLon=libcfGrp.variables['lon'][:].flatten()
  tempLat=libcfGrp.variables['lon'][:].flatten()
  vertices=np.zeros([tempLon.size,2])
  for i in range(vertices.shape[0]):
    vertices[i,0]=tempLon[i]
    vertices[i,1]=tempLat[i]
  return vertices
  

src_vertices=get_vertices('tst_lonlat_to_lonlat_ori.nc')
dst_vertices=get_vertices('tst_lonlat_to_lonlat_tgt.nc')

#def get_dst_vertices
#  scripGrp=Dataset(filename)


def get_src_cellCenters(filename):
  scripGrp=Dataset(filename)
  src_cellCenters=np.zeros([scripGrp.variables['src_grid_center_lat'].shape[0],2])
  for i in range(src_cellCenters.shape[0]):
    src_cellCenters[i,0]=scripGrp.variables['src_grid_center_lon'][i]
    src_cellCenters[i,1]=scripGrp.variables['src_grid_center_lat'][i]
  return src_cellCenters

def get_dst_cellCenters(filename):
  scripGrp=Dataset(filename)
  dst_cellCenters=np.zeros([scripGrp.variables['dst_grid_center_lat'].shape[0],2])
  for i in range(dst_cellCenters.shape[0]):
    dst_cellCenters[i,0]=scripGrp.variables['dst_grid_center_lon'][i]
    dst_cellCenters[i,1]=scripGrp.variables['dst_grid_center_lat'][i]
  return dst_cellCenters


src_cellCenters=get_src_cellCenters('rmp_longlat_2b2_to_4b4_conserv.nc')
dst_cellCenters=get_dst_cellCenters('rmp_longlat_2b2_to_4b4_conserv.nc')
#create function eval original grid.
def func1(gridVect):
  z=np.zeros(gridVect.shape[0])
  for i in range(gridVect.shape[0]):
      z[i]=mt.sin(mt.pi/2 * gridVect[i,0])*mt.sin(mt.pi/2 * gridVect[i,1])
  return z

z1=func1(dst_cellCenters)

#eval on original grid

#eval on destination grid

#apply interpolation weights to original grid evaluation to
#obtain interpolated values


#get absolute difference at each point


#sum up absolute differences


#plot absolute differenes at each point
