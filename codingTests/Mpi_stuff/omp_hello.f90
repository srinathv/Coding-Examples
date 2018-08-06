program hello

  ! Include the MPI library definitons:
  include 'mpif.h'

  integer numtasks, rank, ierr, rc, len, i
  character*(MPI_MAX_PROCESSOR_NAME) name
  integer omp_get_num_threads, iam,  omp_get_thread_num , mycpu
  integer, external :: findmycpu

  ! Initialize the MPI library:
  call MPI_INIT(ierr)
  if (ierr .ne. MPI_SUCCESS) then
     print *,'Error starting MPI program. Terminating.'
     call MPI_ABORT(MPI_COMM_WORLD, rc, ierr)
  end if

  ! Get the number of processors this job is using:
  call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)

  ! Get the rank of the processor this thread is running on.  (Each
  ! processor has a unique rank.)
  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)

  ! Get the name of this processor (usually the hostname)
  call MPI_GET_PROCESSOR_NAME(name, len, ierr)
  if (ierr .ne. MPI_SUCCESS) then
     print *,'Error getting processor name. Terminating.'
     call MPI_ABORT(MPI_COMM_WORLD, rc, ierr)
  end if

!$omp parallel default(shared) private(iam,np,mycpu)
  np = omp_get_num_threads()
   iam = omp_get_thread_num()
   mycpu =  findmycpu()
!  print "('hello_parallel.f: Number of tasks=',I3,' My rank=',I3,' My name=',A, &
!          ' total theads= ',I2, ' thread num=',I2,  ' my cpu id=',I3,'')",&
!       numtasks, rank, trim(name), np, iam , mycpu 
   write (*,*) mycpu
!$omp end parallel
  ! Tell the MPI library to release all resources it is using:
  call MPI_FINALIZE(ierr)


end program hello
