

PROGRAM indexArray

  REAL,DIMENSION(:),ALLOCATABLE    :: a
  INTEGER :: i


  ALLOCATE(a(0:10))
  DO i=0,10
    a(i)=i
  ENDDO

  WRITE(*,*) a(:)
  WRITE(*,*) a(0:10)

  DEALLOCATE(a)
END PROGRAM indexArray
