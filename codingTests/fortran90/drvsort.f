c<html>
c<head><title>drvsort.f</title></head>
c<body>
c<pre>      
	program drvsort
c
c   Program to drive sorting subroutines
c
c   John Mahaffy 3/13/95
c
      integer ndim, j
c
c   Set the array size
c
      parameter (ndim=10)
      real x(ndim), xo(ndim)
      integer ix(ndim)
c
c   Load x with pseudo-random numbers bounded by 0 and 1
c   using a Fortran 90 standard Intrinsic Subroutine
c<a name=1><font color=FF0000>
      call random_number (x(1:ndim))
C
c
c   Load ix with the array indices for the original values
c</font>
      ix = (/(j,j=1,ndim)/)
c
c   Load xo with the contents of x to save the original order
c
      xo(1:ndim)=x(1:ndim)
c
c   Sort x from highest to lowest value.  When an element in x
c   is shifted, shift the corresponding element in ix to the
c   same new position
c

      call ssort( x, ix, ndim, -2)
      write(*,2000)
 2000 format(' Original ',5x,'  Sorted  ',/,'  Array  ',
     $ 5x,'   Array')
      write(*,2001) (i, xo(i), ix(i), x(i), i=1,ndim)
 2001 format(i3,2x,f5.3,5x, i3,2x, f5.3)
      stop
      end
c</pre>
c</body>
c</html>
