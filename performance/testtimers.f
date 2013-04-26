c------------------------------------------------------------- 
     
     
      program testtimers
c
c  This program measures wall clock time 
c
c  wdiff returns the time elapsed between the call to wdiff
c    and the previous call to wtime.  
c    wdiff requires that the same vector time used in the 
c    call to wtime 
c
c    wdiff and wtime are C functions calling gettimeofday 
c 

      real*8 sum, tim(2), elapsed , smax 
      integer i, ierr, ierr2
      i = 1
      sum = 0.d0 
      print*,' input max for sum'
      print*,' suggest numbers around 10 to 20' 
      read*, smax
      call wtime(tim, ierr)
      do while (sum .lt. smax)
        sum = sum + 1./i
        i = i+1
      end do
      elapsed = wdiff(tim,ierr2) 
      print*,' elapsed time is ', elapsed,' seconds' 
      stop 
      end 
     
c----------------------------------------------------------
