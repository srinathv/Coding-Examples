import tables,pylab

profile=tables.openFile('gyro_profile.h5')
n_field=profile.root.n_field.read()
n_kinetic=profile.root.n_kinetic.read()
profile.close()

fluxfile=open('gbflux.out')
fluxstr=fluxfile.read()
newfluxstr=fluxstr.replace('\n',' ')
fluxarray=pylab.fromstring(newfluxstr,sep=' ')
fluxtensor=fluxarray.reshape([n_kinetic,n_field,4,457])
fluxfile.close()

pylab.plot(fluxtensor[0,0,1,:])
pylab.show()
