program test

#include 'fpapi.h'

  use ifport


  integer,parameter :: index = 1000
  real,dimension(index,index):: mat1
  real,dimension(index,index):: mat2
  real,dimension(index,index):: result
  integer :: i,j,k

  real :: papi1, papi2

  INTEGER*8 :: papilonglong
  !implicit none

 ! call seed(1995)

!inialize matrix
 do i = 1, index, 1
   do j=1,index
   mat1(i,j)= rand()
 end do
enddo



end program
