! I want to really test out vpu use
! I need to look at rates for doing a FMA => z = a*x +b 
!       and z= x*y + a*b
! need arrays that are nthreads segements of vpuLength (num. of doubles) each length
! We will do the loop it number of times 


  program testVpu
#if defined(HAVE_OMP)
  use omp_lib
#endif

  implicit none


#if defined(SET_IT)
  integer, parameter :: it = SET_IT
#else
  integer, parameter :: it =10E6
#endif
#if defined(SET_NT)
  integer, parameter :: nThreads = SET_NT
#else
  integer, parameter :: nThreads = 4
#endif
#if defined(SET_VL)
  integer, parameter :: vpuLength = SET_VL
#else
  integer, parameter :: vpuLength = 4
#endif

  integer :: i,j,k
  real :: fmaNumFlop,dubNumFlop


  real, dimension(nThreads*vpuLength) :: a,b,x,y,z
  real :: fmaStartTime=0.,fmaEndTime=0.,dubStartTime=0.,dubEndTime=0.


!initialize arrays
  a=1.
  b=1.
  x=1.
  y=1.
  z=0.

!initialize threads
#if defined(HAVE_OMP)
!$omp parallel
  call omp_set_num_threads(nThreads)
!$omp end parallel
#endif

  
!each thread will have a vpuLength of the array to do the work

!test FMA

!start time
#if defined(HAVE_OMP)
      fmaStartTime = omp_get_wtime()
#else
      call cpu_time(fmaStartTime)
#endif

  do k = 1, 100000
  do i = 1, it
!omp parallel do shared(z) private(j)
    do j=1,nThreads*vpuLength
  
      z(j)=a(j)*x(j)+b(j)
    enddo
!omp end parallel do
  enddo 
  enddo 
#if defined(HAVE_OMP)
      fmaEndTime = omp_get_wtime()
#else
      call cpu_time(fmaEndTime)
#endif

    write(*,*) "fma maxval(z) =", maxval(z) ,"should be 2"
!---------------------------------------------

!reinitialize arrays
  a=1.
  b=1.
  x=1.
  y=1.
  z=0.

#if defined(HAVE_OMP)
      dubStartTime = omp_get_wtime()
#else
      call cpu_time(dubStartTime)
#endif

!add and mults
 do k = 1, 100000
 do i = 1, it
!omp parallel do shared(z) private(j)
    do j=1,nThreads*vpuLength
  
      z(j)=a(j)+x(j)+b(j)
    enddo
!omp end parallel do
  enddo 
  enddo 
#if defined(HAVE_OMP)
      dubEndTime = omp_get_wtime()
#else
      call cpu_time(dubEndTime)
#endif

    write(*,*) "dub maxval(z) =", maxval(z) ,"should be 3"

!report times and flops
write(*,*) " fma time = ", fmaEndTime-fmaStartTime
write(*,*) " dub time = ", dubEndTime-dubStartTime

!total number of flops
fmaNumFlop=real(it)*100000*real(nThreads)*real(vpuLength)*2
dubNumFlop=fmaNumFlop

write(*,*) " FmaNumFlop = ",real(fmaNumFlop)
write(*,*) " dubNumFlop = ",real(dubNumFlop)

write(*,*) "Gflops for FMA = ", (real(fmaNumFlop) * 1E-9)/(fmaEndTime-fmaStartTime)
write(*,*) "Gflops for dub = ", (real(dubNumFlop) * 1E-9)/(dubEndTime-dubStartTime)

end program testVpu
