        PROGRAM fortest

        ! simple program which creates 2 vectors and adds them in a 
        ! cuda function

        IMPLICIT NONE

        integer*4 :: i
        integer*4, parameter :: N=8
        real*4, Dimension(N) :: a, b

        DO i=1,N
          a(i)=i*1.0
          b(i)=2.0
        END DO

         print *, 'a = ', (a(i), i=1,N)

          CALL kernel_wrapper(a, b, N)

         print *, 'a + 2 = ', (a(i), i=1,N)

        END PROGRAM 

