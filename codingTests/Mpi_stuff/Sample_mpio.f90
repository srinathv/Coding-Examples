!
! The following example demonstrates how to create and close a parallel
! file using MPI-IO calls.
!

     PROGRAM MPIOEXAMPLE

     USE MPI 
        
     IMPLICIT NONE

     CHARACTER(LEN=80), PARAMETER :: filename = "/tmp/filef.h5" ! File name
     INTEGER     ::   ierror  ! Error flag
     INTEGER     ::   fh      ! File handle
     INTEGER	 ::   amode   ! File access mode
     
     call MPI_INIT(ierror)
     amode = MPI_MODE_RDWR + MPI_MODE_CREATE + MPI_MODE_DELETE_ON_CLOSE
     call MPI_FILE_OPEN(MPI_COMM_WORLD, filename, amode, MPI_INFO_NULL, fh, ierror)
     print *, "Trying to create ", filename
     if ( ierror .eq. MPI_SUCCESS ) then
	print *, "MPI_FILE_OPEN succeeded"
	call MPI_FILE_CLOSE(fh, ierror)
     else
	print *, "MPI_FILE_OPEN failed"
     endif

     call MPI_FINALIZE(ierror);
     END PROGRAM
