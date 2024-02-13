!# from Gene Wagenbreth to test the associate clause





  program t2
    implicit none
    integer:: i12 = 12
    integer:: i = 1
    integer:: j
 
    print *,'start t2'
 
    associate (i=>i12,j=>i12)
      print *, 'in associate'
      print *,'i=',i
      print *,'j=',j
    end associate

    print *, ''
    print *, ''
    print *, ''
    print *, ''
    print  *, 'original variables'
    print *, 'i12 =', i12
    print *, 'i =', i
 
    print *,'end t2'
  end

!ftn -g -O0 t2.F90 -o t2
!./t2
! start t2
! i= 12
! j= 12
! end t2


!# use a debugger to scope the 
