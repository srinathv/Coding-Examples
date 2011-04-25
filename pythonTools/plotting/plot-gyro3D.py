import tables
import numpy
from enthought.mayavi import mlab
from enthought.tvtk.api import tvtk

# open file for reading data
fh = tables.openFile("gyro3D20000.h5")
# read grid
cartMesh = fh.root.grid.cartMesh.read()
dims = cartMesh[:,:,:,0].shape
xyz_pts = cartMesh.transpose(2, 1, 0, 3).copy()
xyz_pts.shape = xyz_pts.size/3, 3
# delete extra memory
del cartMesh
# create structured grid object
sg = tvtk.StructuredGrid(dimensions=dims, points=xyz_pts)
# now read in electron density
density_electron = fh.root.density_electron.read()
density_electron = density_electron.T.copy()
sg.point_data.scalars = density_electron.ravel()
sg.point_data.scalars.name = 'Electron density'

# delete extra memory
del density_electron

# viz it
surf = mlab.pipeline.surface(sg, opacity=0.1)
cut_plane = mlab.pipeline.scalar_cut_plane(surf)

mlab.show()
