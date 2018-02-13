      program single_task


      integer, parameter :: n = 10000000
      real, allocatable :: a(:), b(:)
      real*8 :: t0, omp_get_wtime
      integer :: omp_get_thread_num

      allocate (a(n),b(n))

c$omp parallel
#ifdef SINGLE
c$omp single
#endif
c$omp task
      write(*,*) "my thread id is ", omp_get_thread_num()
c$omp end task

#ifdef MORETASK
c$omp task
      write(*,*) "my thread id is ", omp_get_thread_num()
c$omp end task

c$omp task
      write(*,*) "my thread id is ", omp_get_thread_num()
c$omp end task
#endif

#ifdef SINGLE
c$omp end single
#endif
c$omp end parallel
      end program
