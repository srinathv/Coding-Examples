from netCDF4 import Dataset
import numpy as np
import pylab as py


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
py.show()
