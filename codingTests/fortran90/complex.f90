
  PROGRAM complexcheck
    IMPLICIT NONE

    COMPLEX  :: a(2,2)


    a(1,1) = (1,6)
    a(1,2) = (2,7)
    a(2,1) = (3,8)
    a(2,2) = (4,9)

    write(*,*) real(a)
    write(*,*)
    write(*,*) aimag(a)
  
  END PROGRAM complexcheck
