! I want to really test out vpu use
! I need to look at rates for doing a FMA => y = a*x +b 
!       and z= x*y + a*b
! need arrays that are nthreads segements of vpuLength (num. of doubles) each length
! We will do the loop it number of times 


  program testVpu

  implicit none


  integer, parameter :: it =1000
  integer, parameter :: nThreads = 8
  integer, parameter :: vpuLength = 4


  real, dimension(nThreads*vpuLength) :: a,b,x,y,z



   
  end program testVpu
