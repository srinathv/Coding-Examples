import numpy as np

# Generate grids of coordinates from a min, max, and spacing
dx, dy = 0.5, 0.5

# For the larger grid...
Axmin, Axmax = -180, 180
Aymin, Aymax = -90, 90

# For the smaller grid...
Bxmin, Bxmax = -5, 10
Bymin, Bymax = 30, 40

# Generate the indicies on a 2D grid
Ax = np.arange(Axmin, Axmax+dx, dx)
Ay = np.arange(Aymin, Aymax+dy, dy)
Ax, Ay = np.meshgrid(Ax, Ay)

Bx = np.arange(Bxmin, Bxmax+dx, dx)
By = np.arange(Bymin, Bymax+dy, dy)
Bx, By = np.meshgrid(Bx, By)

# Find the corner of where the two grids overlap...
ix, iy = np.argwhere((Ax == Bxmin) & (Ay == Bymin))[0]

# Assert that the coordinates are identical.
assert np.all(Ax[ix:ix+Bx.shape[0], iy:iy+Bx.shape[1]] == Bx) 
assert np.all(Ay[ix:ix+Bx.shape[0], iy:iy+Bx.shape[1]] == By) 
