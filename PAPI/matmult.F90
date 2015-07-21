program test

#include 'fpapi.h'

  use ifport


  integer,parameter :: index = 1000
  real,dimension(index,index):: mat1
  real,dimension(index,index):: mat2
  real,dimension(index,index):: resultMat
  integer :: i,j,k

  real :: papi1, papi2 ,rhs

  INTEGER*8 :: papilonglong
  !implicit none

 ! call seed(1995)

!inialize matrix
 do i = 1, index, 1
   do j=1,index
   mat1(i,j)= rand()
   mat2(i,j)= rand()*(1.1)
 end do
enddo

  resultMat=0.


CALL PAPIF_num_counters(check)
!multiply
 do i = 1, index, 1
   do j=1,index
     rhs=0.
     do k=1,index
       rhs=mat1(i,k)*mat2(j,k)
     enddo
     resultMat(i,j)=rhs
 end do
enddo



end program
