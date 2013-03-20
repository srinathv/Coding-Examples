       PROGRAM HELLO

       INTEGER NTHREADS, TID, OMP_GET_NUM_THREADS, &
     &   OMP_GET_THREAD_NUM

!     Fork a team of threads with each thread having a private TID variable
!$OMP PARALLEL PRIVATE(TID)

!     Obtain and print thread id
      TID = OMP_GET_THREAD_NUM()
      PRINT *, 'Hello World from thread = ', TID

!     Only master thread does this
      IF (TID .EQ. 0) THEN
        NTHREADS = OMP_GET_NUM_THREADS()
        PRINT *, 'Number of threads = ', NTHREADS
      END IF

!     All threads join master thread and disband
!$OMP END PARALLEL

       END PROGRAM
