

  program testIndex

  real, dimension( -3:0) :: testarray
  real, dimension( 0:-2) :: testarray2
  real, dimension(1:2,1:2):: matrix


  testarray=0
  testarray2=0

  testarray(-3)=3.0


  write(*,*) testarray ," size of testarray = ", size(testarray) 
  write(*,*) "2 = ",testarray2 , " size of testarray2 = ", size(testarray2)
    
  matrix(1,1)=1
  matrix(1,2)=2
  matrix(2,1)=3
  matrix(2,2)=4

  write(*,*) matrix
  write(*,*) matrix(2:1,2:1)
  end program testIndex
