      program single_task


      integer, parameter :: n = 10000000
      real, allocatable :: a(:), b(:)
      real*8 :: t0, omp_get_wtime
      integer :: omp_get_thread_num,omp_get_num_threads,num_threads


      allocate (a(n),b(n))

c$omp parallel
      num_threads=omp_get_num_threads()
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

#ifdef TASKLOOP
c$omp taskloop private(i)
      do i=1,num_threads
      write(*,*) " Iteration i=",i," in the taskLoop, my thread id is " &
     & , omp_get_thread_num()
      end do
c$omp end taskloop
#endif


#ifdef SINGLE
c$omp end single
#endif
c$omp end parallel
      end program
