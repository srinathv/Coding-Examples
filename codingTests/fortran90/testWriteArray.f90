

PROGRAM writeArray
IMPLICIT NONE

  !trying to find out how to write arrays with valgrind not shittin a brick

  TYPE :: arraycontainer
    REAL , DIMENSION (3,4) :: tarray
  END TYPE

  TYPE:: bigCont
    TYPE(arraycontainer) :: wrap
  END TYPE

  TYPE(bigCont) :: L
  INTEGER                :: i,j

  L%wrap%tarray=1.

  WRITE(*,*) L%wrap%tarray
!  WRITE(*,*) L%tarray(:,:)
!  
!  DO i=1,3
!    DO j=1,4
!    WRITE(*,*) L%tarray(i,j)
!    ENDDO
!  ENDDO


END PROGRAM
