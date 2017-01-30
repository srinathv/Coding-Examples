import numpy as np
import orthopoly as op
from pseudospectral import diffmat
from scipy.linalg import solve

def poisson_2d(n,f,g,h,d):
    """
    Solve the Poisson equation on the square [-1,1]^2 
    
    -Delta u = f(x,y) for -1<x,y<1
    
    With Dirichlet or Neumann conditions as specified by an
    indicator function d(x,y)
    
    Inputs:
    n - trial polynomial order in both dimensions
    f - right hand side (set to None for Laplace eqn)
    g - Dirichlet data
    h - Neumann data
    d - an indicator function for where to impose
        Dirichlet conditions if d(x,y)>0
          
    Outputs:
    xx,yy - grid
    U - solution on the grid
    
    """

    alpha,beta = op.rec_jacobi(n,0,0)  # Legendre recursion coefficients
    x,w = op.lobatto(alpha,beta,-1,1)  # LGL quadrature
    D = diffmat(x)                     # Pseudospectral differentiation matrix
    M = np.diag(w)                     # Approximate 1D mass matrix
    K = np.dot(D.T,np.dot(M,D))        # 1D stiffness matrix
    xx,yy = np.meshgrid(x,x)           # Tensor product grid
    xf = xx.flatten()                  # Column stack nodes
    yf = yy.flatten()    

    # In this section, identify the indices of different types of points
    k = np.arange(1,n-1)
    dex = set(np.arange(n*n))
    bdex = np.hstack((0,k,n-1,n*k,(k+1)*n-1,n*(n-1),n*(n-1)+k,n*n-1))
    
    dbool = d(xf[bdex],yf[bdex])>-1e-9 # Returns True for Dirichlet points
    ddex = set(bdex[dbool])            # Indices of Dirichlet points
    bdex = set(bdex)
    ndex = list(bdex.difference(ddex)) # Indices of Neumann points
    udex = list(dex.difference(ddex))  # Indices of Unknowns
    ddex = list(ddex); ndex=list(ndex)
    
    W = np.zeros((n,n))                # Surface quadrature (lazy)
    W[0,:] = w;    W[:,0] = w
    W[-1,:] = w;   W[:,-1] = w
    W = W.flatten()

    H = np.zeros(n*n)
    H[ndex] = h(xf[ndex],yf[ndex])     # Neumann surface data
    
    A = np.kron(K,M) + np.kron(M,K)    # Galerkin approximation of -Delta
    
    if f is None:                      # Laplace equation
        F = np.zeros(len(udex))
    else:                              # Poisson equation
        F = np.kron(w,w)*f(xf,yf)
        F = F[udex]
    
    G = g(xf[ddex],yf[ddex])           # Dirichlet data
    Au = A[udex,:][:,udex]             # Restrict system to unknowns
    F -= np.dot(A[udex,:][:,ddex],G)   # Modify RHS for Dirichlet data
    F += H[udex]*W[udex]               # Modify RHS for Neumann data

    u = np.zeros(n*n)
    u[ddex] = G                        # Set known values of u
    u[udex] = solve(Au,F,sym_pos=True) # Solve for unknown values 
    U = np.reshape(u,(n,n)) 

    Uexact = g(xx,yy)
    return xx,yy,U,Uexact


def manufactured_solution():
    import sympy as sy
    from sympy.abc import x,y

    u = x*sy.exp(x-y) + y*sy.exp(x+y)
    ux = sy.diff(u,x)
    uy = sy.diff(u,y)
    f = -sy.diff(ux,x) - sy.diff(uy,y)

    F = sy.lambdify([x,y],f,"numpy")
    G = sy.lambdify([x,y],u,"numpy")

    # Impose Dirichlet conditions on north and south sides
    d = (y+1)*(y-1)
    D = sy.lambdify([x,y],d,"numpy")
     
    # Neumann condition
    h = ux*sy.sign(x)
    H = sy.lambdify([x,y],h,"numpy")

    return F,G,H,D


if __name__ == '__main__':
    from matplotlib import cm
    from matplotlib.pyplot import figure, show
    from mpl_toolkits.mplot3d import Axes3D
    import numpy as np


    f,g,h,d = manufactured_solution()

    xx,yy,U,Uexact = poisson_2d(25,f,g,h,d)
    
    fig1 = figure(1)
    ax1 = fig1.gca(projection='3d')
    ax1.plot_surface(xx,yy,U, rstride=1, cstride=1, cmap=cm.jet,
           linewidth=0.1, antialiased=True)
    ax1.set_xlabel('x' )
    ax1.set_ylabel('y')
    ax1.set_title('Computed Solution')

    Uerr = U-Uexact
    fig2 = figure()
    ax2 = fig2.gca(projection='3d')
    ax2.plot_surface(xx,yy,Uerr, rstride=1, cstride=1, cmap=cm.jet,
           linewidth=0.1, antialiased=True)
    ax2.set_xlabel('x' )
    ax2.set_ylabel('y')
    ax2.set_title('Error')
    print 'Maximum pointwise error = ', max(abs(Uerr.flatten()))
    show()
