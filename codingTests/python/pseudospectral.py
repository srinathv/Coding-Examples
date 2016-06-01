# pseudospectral.py
#
# Compute the pseudospectral differentiation matrix for a set of grid points

import scipy as sp 
from operator import mul

def diffmat(x): # x is an ordered array of grid points
  n = sp.size(x)
  e = sp.ones((n,1))
  Xdiff = sp.outer(x,e)-sp.outer(e,x)+sp.identity(n)
  xprod = -reduce(mul,Xdiff) # product of rows
  W = sp.outer(1/xprod,e)
  D = W/sp.multiply(W.T,Xdiff)
  d = 1-sum(D)
  
  for k in range(0,n):  # Set diagonal elements
    D[k,k] = d[k]

  return -D.T