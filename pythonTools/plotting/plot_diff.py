import tables,pylab

profile=tables.openFile('gyro_profile.h5')
n_field=profile.root.n_field.read()
n_kinetic=profile.root.n_kinetic.read()
n_x=profile.root.n_x.read()
profile.close()

timefile=pylab.loadtxt('t.out')
tflength=timefile.shape[0]
lastime=timefile[timefile.shape[0]-1,0]

dfile=pylab.loadtxt('diff.out')
transdfile=dfile.transpose()
dtensor=transdfile.reshape([n_kinetic,n_field,2,tflength])



pylab.figure()
numoplots=n_kinetic*n_field*2
halfnum=numoplots/2
for i in range(1,numoplots+1):
  pylab.subplot(halfnum,2,i)
  pylab.plot(dfile[:,i-1])

pylab.show()
