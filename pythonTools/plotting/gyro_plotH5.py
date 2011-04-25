
import sys
import tables
import pylab

#Get time
gtime=sys.argv[1]

# Get data
gp=tables.openFile("gyro_profile.h5")
n_n=gp.root.n_n.read()
d_n=gp.root.d_n.read()
n0=gp.root.n0.read()
n_x=gp.root.n_x.read()  #240 radial points
n_field=gp.root.n_field.read()
n_kinetic=gp.root.n_kinetic.read()
nu=gp.root.nu_s.read()
gp.close()

gf=tables.openFile("gyrofine"+gtime+".h5")
alpha=gf.root.grid.alpha.read()
R=gf.root.grid.R.read()
Z=gf.root.grid.Z.read()
fineion=gf.root.density_ion1_phi01.read()
#gf.close()

#gf=tables.openFile("gyro"+gtime+".h5")
#for now build the density array for (species, fourier number, radial loc, poloidal loc)
radial_size=pylab.shape(gf.root.densityfine_real001)[1]
poloidal_size=pylab.shape(gf.root.densityfine_real001)[2]

real_ndensity_n=pylab.zeros((n_kinetic,n_n,radial_size,poloidal_size))
imag_ndensity_n=pylab.zeros((n_kinetic,n_n,radial_size,poloidal_size))
Denmag=pylab.zeros((n_kinetic,n_n,radial_size,poloidal_size+1))

for n in range(1,n_n+1):
    if n<10:
      realDataName="densityfine_real00"+str(n)
      imagDataName="densityfine_imag00"+str(n)
    else:
      realDataName="densityfine_real0"+str(n)
      imagDataName="densityfine_imag0"+str(n)
    cmd1="gf.root."+realDataName+".read()"
    cmd2="gf.root."+imagDataName+".read()"
    real_ndensity_n[:,n-1,:,:]=eval(cmd1)
    imag_ndensity_n[:,n-1,:,:]=eval(cmd2)
gf.close()

#g3df=tables.openFile("gyro3D"+gtime+".h5")
#g3dtheta_slices=g3df.root.density_ion1.shape[0]
#g3dradial_size=g3df.root.density_ion1.shape[1]
#g3dpoloidal_size=g3df.root.density_ion1.shape[2]
#g3dDen=pylab.zeros((g3dtheta_slices,g3dradial_size,g3dpoloidal_size))
#g3dDen=g3df.root.density_ion1.read()
#g3dR=g3df.root.grid.R.read()
#g3dZ=g3df.root.grid.Z.read()
#g3df.close()

Denmag[:,:,:,0:poloidal_size]=pylab.sqrt(real_ndensity_n**2+imag_ndensity_n **2)
Denmag[:,:,:,poloidal_size]=Denmag[:,:,:,0]


#pylab.figure()
#pylab.subplot(2,2,1)
#CS1=pylab.contourf(pylab.transpose(Denmag[1,0,:,:]),cmap=pylab.cm.get_cmap('cool'))
#pylab.title('Mode 1 magnitude')
#
##pylab.figure()
#pylab.subplot(2,2,2)
#CS1=pylab.contourf(pylab.transpose(Denmag[1,1,:,:]),cmap=pylab.cm.get_cmap('cool'))
#pylab.title('Mode 2 magnitude')
#
#
##pylab.figure()
#pylab.subplot(2,2,3)
#CS1=pylab.contourf(pylab.transpose(Denmag[1,10,:,:]),cmap=pylab.cm.get_cmap('cool'))
#pylab.title('Mode 11 magnitude')
#
#
##pylab.figure()
#pylab.subplot(2,2,4)
#CS1=pylab.contourf(pylab.transpose(Denmag[1,19,:,:]),cmap=pylab.cm.get_cmap('cool'))
#pylab.title('Mode 20 magnitude')
#
#pylab.savefig('oxyRun_t_2_8_124_modes.png')

pylab.figure()
#pylab.subplot(2,2,1)
CS2=pylab.contourf(R,Z,fineion,cmap=pylab.cm.get_cmap('cool'))
pylab.title('High resolution contour of density')

#pylab.subplot(2,2,2)
#CS2=pylab.contourf(g3dR,g3dZ,g3dDen[0,:,:],cmap=pylab.cm.get_cmap('cool'))
#pylab.title('From 3D file at 0 alpha slice')

#pylab.subplot(2,2,3)
#CS2=pylab.contourf(g3dR,g3dZ,g3dDen[5,:,:],cmap=pylab.cm.get_cmap('cool'))
#pylab.title('From 3D file at 5 alpha slice')
#
#pylab.subplot(2,2,4)
#CS2=pylab.contourf(g3dR,g3dZ,g3dDen[10,:,:],cmap=pylab.cm.get_cmap('cool'))
#pylab.title('From 3D file at 10 alpha slice')

pylab.show()
