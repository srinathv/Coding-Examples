import newp3m

mySim=newp3m.N2Sim()
mySim.set_nptcls(2**10)
mySim.init_ptcls()
mySim.initialize_opencl()
mySim.calc_radii_gpu()

