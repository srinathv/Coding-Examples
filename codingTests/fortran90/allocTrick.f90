

  Program allocTrick

  ! I want to get all the info from the allocated array itself

  implicit none


  integer :: n1=1,n2=2,n3=3
  integer :: ranks(1),i
  real, allocatable :: array(:,:,:)
  integer, allocatable :: dims(:)




  allocate(array(n1,n2,n3))

  ranks=shape(shape(array))
  allocate(dims(ranks(1)))
  dims=shape(array)
  write(*,*) "shape of array = ", dims
  write(*,*) "dimension of array = ", ranks
  

  End Program allocTrick
