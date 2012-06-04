
import random
import numpy
import pyopencl as cl
import math

### Usage:
### mySim=N2Sim()
### mySim.init_ptcls()
### mySim.take_steps(100)

class N2Sim():
  nptcls = 2
  ptclarrsize = 12
  ptcllist = 0.*numpy.ndarray([nptcls,ptclarrsize])
  rarr = 0.*numpy.ndarray([nptcls,nptcls])
  Exarr = 0.*numpy.ndarray([nptcls,nptcls])
  Eyarr = 0.*numpy.ndarray([nptcls,nptcls])
  Ezarr = 0.*numpy.ndarray([nptcls,nptcls])
  dt = 1.e-9
  minfact = 0.1
  k = 1./(4.*numpy.pi*8.854187817620e-12)
  rmax = 250.e-6
  vth = 1.
  vbar = 0.
  goodfact = 0.1
  impactfact = (1./(0.01*rmax))**2
  energy=0.
  #fundcharge=1.602176565e-19
  #fundmass=9.10938215e-31
  fundcharge=1.0
  fundmass=1.0
  output = 0
  openCLInitialized = 0

  def calc_energy(self):
    self.energy=0.
    for p1 in range(self.nptcls):
      self.energy+=0.5*self.ptcllist[p1][7]*(self.ptcllist[p1][3]**2+self.ptcllist[p1][4]**2+self.ptcllist[p1][5]**2)
      self.energy+=self.k*self.ptcllist[p1][6]*numpy.sum(self.rarr[p1,:p1])

  def set_nptcls(self,np):
    self.nptcls=np
    self.ptcllist.resize([self.nptcls,self.ptclarrsize])
    self.rarr.resize([self.nptcls,self.nptcls])
    self.Exarr.resize([self.nptcls,self.nptcls])
    self.Eyarr.resize([self.nptcls,self.nptcls])
    self.Ezarr.resize([self.nptcls,self.nptcls])

  def init_ptcls(self,pstart=0,ptot=0,charge=0,mass=0,time=0.):
    if charge==0:
      charge=-1.*self.fundcharge
    if mass==0:
      mass=self.fundmass
    if ptot==0:
      ptot=self.nptcls
    for j in range(pstart,pstart+ptot):
      r = self.vth*random.gauss(0.,self.rmax)
      theta = numpy.pi*random.random()
      phi = 2.*numpy.pi*random.random()
      vx = self.vth*random.gauss(self.vbar,self.vth)
      vy = self.vth*random.gauss(self.vbar,self.vth)
      vz = self.vth*random.gauss(self.vbar,self.vth)
      self.ptcllist[j][0]=r*numpy.sin(theta)*numpy.cos(phi)
      self.ptcllist[j][1]=r*numpy.sin(theta)*numpy.sin(phi)
      self.ptcllist[j][2]=r*numpy.cos(theta)
      self.ptcllist[j][3]=vx
      self.ptcllist[j][4]=vy
      self.ptcllist[j][5]=vz
      self.ptcllist[j][6]=charge
      self.ptcllist[j][7]=mass
      self.ptcllist[j][8]=time
      self.ptcllist[j][9]=0. # x-acceleration
      self.ptcllist[j][10]=0. # y-acceleration
      self.ptcllist[j][11]=0. # z-acceleration
    #self.calc_radii()

  def calc_radii(self):
    self.rarr*=0.
    self.Exarr*=0.
    self.Eyarr*=0.
    self.Ezarr*=0.
    for p1 in range(self.nptcls-1):
      x1=self.ptcllist[p1,0:3]
      x2=self.ptcllist[p1+1:,0:3]
      d0=numpy.subtract.outer(x1[0],x2[:,0].T)
      d1=numpy.subtract.outer(x1[1],x2[:,1].T)
      d2=numpy.subtract.outer(x1[2],x2[:,2].T)
      self.rarr[p1,p1+1:]=numpy.power(numpy.sqrt(numpy.sum(numpy.array([d0,d1,d2])**2,axis=0)),-3)
      self.Exarr[p1,p1+1:]=d0*self.rarr[p1,p1+1:]
      self.Eyarr[p1,p1+1:]=d1*self.rarr[p1,p1+1:]
      self.Ezarr[p1,p1+1:]=d2*self.rarr[p1,p1+1:]

    self.rarr=numpy.add(self.rarr,self.rarr.T)
    self.Exarr=numpy.subtract(self.Exarr,self.Exarr.T)
    self.Eyarr=numpy.subtract(self.Eyarr,self.Eyarr.T)
    self.Ezarr=numpy.subtract(self.Ezarr,self.Ezarr.T)
      
    self.Exarr*=self.ptcllist[:,6]*self.k
    self.Eyarr*=self.ptcllist[:,6]*self.k
    self.Ezarr*=self.ptcllist[:,6]*self.k

  def calc_radii_gpu(self):
    if self.openCLInitialized == 0:
      self.initialize_opencl()
    cl.enqueue_write_buffer(self.queue, self.ptcllist_d,
        self.ptcllist).wait()
    blockSize = 1024
    numBlocks = int(
        math.ceil(numpy.float64(self.nptcls) /
          numpy.float64(blockSize)))
    print(blockSize)
    print(numBlocks)
    self.prg.calc_radii_kernel(
        self.queue,
        (numBlocks * blockSize,), 
        (blockSize,),
        self.ptcllist_d,
        numpy.int32(self.nptcls),
        numpy.float64(self.k))
    cl.enqueue_read_buffer(self.queue, self.ptcllist_d, self.ptcllist).wait()

  def is_good(self,myfact,p1):
    isgood=0.            
    if self.ptcllist[p1][3]<>0.:
      isgood+=abs(myfact*self.ptcllist[p1][0]*self.ptcllist[p1][9]/self.ptcllist[p1][3])
    if self.ptcllist[p1][4]<>0.:
      isgood+=abs(myfact*self.ptcllist[p1][1]*self.ptcllist[p1][9]/self.ptcllist[p1][4])
    if self.ptcllist[p1][5]<>0.:
      isgood+=abs(myfact*self.ptcllist[p1][2]*self.ptcllist[p1][9]/self.ptcllist[p1][5])

    return isgood

  def take_steps(self,nsteps):

    for n in range(nsteps):
      if self.output:
        print "Percent done: ",1.*(n+1)/nsteps
      t0=self.ptcllist[0][8]
      for p1 in range(self.nptcls):
        while(self.ptcllist[p1][8]<t0+self.dt):
          if self.output:
            print "Time is ",self.ptcllist[p1][8], " and going to ",t0+self.dt
          trystep = 1
          myfact0 = (1.0/self.ptcllist[p1][7])*self.dt
          myfact = myfact0
          force = 0
          while(trystep):
            good=0.
            if force:
              if self.output:
                print "Forcing now..."
            else:
              good=self.is_good(myfact,p1)
            if good < self.goodfact or force:
              trystep = 0
              self.ptcllist[p1][3]+=0.5*myfact*numpy.sum(self.Exarr[p1,:])
              self.ptcllist[p1][4]+=0.5*myfact*numpy.sum(self.Eyarr[p1,:])
              self.ptcllist[p1][5]+=0.5*myfact*numpy.sum(self.Ezarr[p1,:])
              self.ptcllist[p1][0]+=(myfact/myfact0)*self.dt*self.ptcllist[p1][3]
              self.ptcllist[p1][1]+=(myfact/myfact0)*self.dt*self.ptcllist[p1][4]
              self.ptcllist[p1][2]+=(myfact/myfact0)*self.dt*self.ptcllist[p1][5]
              self.calc_radii_gpu()
              self.ptcllist[p1][3]+=0.5*myfact*numpy.sum(self.Exarr[p1,:])
              self.ptcllist[p1][4]+=0.5*myfact*numpy.sum(self.Eyarr[p1,:])
              self.ptcllist[p1][5]+=0.5*myfact*numpy.sum(self.Ezarr[p1,:])
              self.ptcllist[p1][8]+=(myfact/myfact0)*self.dt
              if self.output:
                print "Step for", p1," to time",self.ptcllist[p1][8]," ",myfact/myfact0
            else:
              if self.output:
                print "Dropping down 0.5..."
              myfact*=0.5
              if self.ptcllist[p1][8]+(myfact/myfact0)*self.dt>t0+self.dt:
                myfact=myfact0*(t0+self.dt-self.ptcllist[p1][8])/self.dt
                force=1
              if abs(myfact)<abs(self.minfact*myfact0):
                force=1

  def initialize_opencl(self):
    self.ctx = cl.create_some_context()
    self.queue = cl.CommandQueue(self.ctx)
    self.mf = cl.mem_flags
    self.ptcllist_d = cl.Buffer(self.ctx, self.mf.READ_WRITE,
        self.ptcllist.nbytes)
    self.calc_radii_kernel_src = open('calc_radii_kernel.cl','r').read()
    self.prg = cl.Program(self.ctx, self.calc_radii_kernel_src)
    try:
      self.prg.build()
    except:
      print("Error:")
      print(self.prg.get_build_info(self.ctx.devices[0], cl.program_build_info.LOG))
      raise
    self.openCLInitialized = 1

