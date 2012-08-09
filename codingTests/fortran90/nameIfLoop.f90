
  program nameIfLoop
  IMPLICIT NONE

   integer:: i,j

    i=3

   ifName: if (i>2) then
      write(*,*) "in if "
   endif ifName


   doName: do j=1,4
      write(*,*) " j is now =" ,j
   enddo doName

 


  end program nameIfLoop
