from netCDF4 import Dataset
import numpy as np
import pylab as py
import sys, os, re
import math as mt
import mpl_toolkits.mplot3d.axes3d as p3

files=sys.argv


#read in regrid from from libcf


libcfWeightsFile='tst_lonlat_to_lonlat_regrid_weights.nc'
scripsLinWeightsFile='rmp_longlat_8b8_to_8b8_lin.nc'
scripsConWeightsFile='rmp_longlat_8b8_to_8b8_conserv.nc'

sourceVertxFile='tst_lonlat_to_lonlat_ori.nc'
destVertxFile='tst_lonlat_to_lonlat_tgt.nc'

rootgrp=Dataset(libcfWeightsFile)

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

##Plot the libcf transform matrix
#fig1=py.figure()
#py.matshow(libcfWeights)
#py.colorbar()
#py.show()

####LOAD the SCRIP remap matrices
## I think SCRIP is a cell centered remapper.


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



scripLinWeights=scrip_remap_matrix(scripsLinWeightsFile)
scripConsWeights=scrip_remap_matrix(scripsConWeightsFile)

##get the grid locations

def get_vertices(filename):
  libcfGrp=Dataset(filename)
  londim=len(libcfGrp.variables['lon'])
  latdim=len(libcfGrp.variables['lat'])
  tempLon=libcfGrp.variables['lon'][:].flatten()
  tempLat=libcfGrp.variables['lat'][:].flatten()
  vertices=np.zeros([tempLon.size,2])
  for i in range(vertices.shape[0]):
    vertices[i,0]=tempLon[i]
    vertices[i,1]=tempLat[i]
  return vertices,londim,latdim
  

src_vertices,src_lonDim,dst_latDim=get_vertices(sourceVertxFile)
dst_vertices,dst_lonDim,dst_latDim=get_vertices(destVertxFile)

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


src_cellCenters=get_src_cellCenters(scripsConWeightsFile)
dst_cellCenters=get_dst_cellCenters(scripsConWeightsFile)
#create function eval original grid.
#simple constant [2] function
def func1(gridVect):
  z=np.zeros(gridVect.shape[0])
  for i in range(gridVect.shape[0]):
      #z[i]=mt.sin(mt.pi/2 * gridVect[i,0])*mt.cos(mt.pi/2 * gridVect[i,1])
      #z[i]=mt.sin(mt.pi/2 * gridVect[i,0]) * gridVect[i,1]
      z[i]=2.
  return z

zAtDstCellCenters=func1(dst_cellCenters)
zAtSrcCellCenters=func1(src_cellCenters)

zAtDstVertices=func1(dst_vertices)
zAtSrcVertices=func1(src_vertices)

####apply interpolation weights to original grid evaluation to
## apply libcf interpolation at vertices
interpZLibcf=np.dot(libcfWeights,zAtSrcVertices)
## apply scrip 1st order convervative interpolation to cell centers
interpZscripConsv=np.dot(scripConsWeights,zAtSrcCellCenters)
## apply scrip linear interpolation to cell centers
interpZscripLin=np.dot(scripLinWeights,zAtSrcCellCenters)

### Plot surfaces
#plot libcf interpolation data
fig1=py.figure(1)
X=dst_vertices[0:dst_lonDim,0]
Y=dst_vertices[::dst_latDim,1]
Z=np.reshape(interpZLibcf,[dst_lonDim,dst_latDim])
ct=py.contour(X,Y,Z)
cbar= py.colorbar()
py.xlabel('lon')
py.ylabel('lat')

py.show()




#get absolute difference at each point


#sum up absolute differences


#plot absolute differenes at each point
