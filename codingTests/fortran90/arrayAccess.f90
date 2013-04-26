!Trying to test speed of accessing working with arrays

  program workingarrays 
  implicit none

  integer,parameter :: maxsize = 100000
  real :: a(maxsize,maxsize)
  integer :: i,j
  real :: time1start,time1end
  real :: time2start,time2end
  real :: time3start,time3end
  
  a=0.
  call cpu_time(time1start)
  a=a+1.
  call cpu_time(time1end)

  write(*,*) "a=a+1 time = ", time1end-time1start


  a=0.
   call cpu_time(time2start)

  do i=1,maxsize
     do j=1,maxsize
        a(i,j) = a(i,j) + 1
    enddo
  enddo
  call cpu_time(time2end)

  write(*,*) "a(i,j)=a(i,j)+1 time = ", time2end-time2start

 a=0.
   call cpu_time(time3start)

  do i=1,maxsize
     do j=1,maxsize
        a(j,i) = a(j,i) + 1
    enddo
  enddo
  call cpu_time(time3end)

  write(*,*) "a(j,i)=a(j,i)+1 time = ", time3end-time3start



  end program workingarrays 
