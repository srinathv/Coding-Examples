

program simple

include 'f90papi.h'


!Declare the events you want to count and other error-related variables,
!for example:
       integer, parameter :: numevents = 1
       integer events (numevents),  ierr
       character*(PAPI_MAX_STR_LEN) errorstring
!Declare variables to hold the event counts:
       integer*8 values (numevents)
!Set each event to the desired type, listed in f77papi.h (or below):
       events(1) = PAPI_L1_DCM
!       events(3) = PAPI_BR_MSP 
!Start and clear the counters:
       call PAPIF_start_counters(events, numevents, ierr)
!Do some computation, then read and reset them but leave them running:
!       call PAPIF_read_counters(values, numevents, ierr)
!A similar routine, PAPIF_accum_counters, accepts the same arguments but
!adds the current values to the running totals already contained in the
!values array.
!Compute some more and then stop the counters and retrieve the values:
       call PAPIF_stop_counters(values, numevents, ierr)
!Each of those calls returns an error code that you can handle this way:
       if ( ierr .ne. PAPI_OK ) then
         call PAPIF_perror(ierr, errorstring, PAPI_MAX_STR_LEN)
         print *, errorstring
       endif


      print *, " PAPI_L1_DCM is ", values(1)
end program
