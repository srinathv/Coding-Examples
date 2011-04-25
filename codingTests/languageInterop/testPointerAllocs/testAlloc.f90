 MODULE myTypes

   INTEGER, PARAMETER :: osize=2

  TYPE          :: foo
     INTEGER    :: int1
     INTEGER    :: int2 
     REAL       :: real1     
  END TYPE      

  TYPE :: fooContainer
    TYPE(foo), POINTER  :: pointy => NULL()
  END TYPE

  CONTAINS

  SUBROUTINE initFoo(foobar)
     TYPE(foo), INTENT(INOUT) :: foobar

     foobar%int1=0
     foobar%int2=4
     foobar%real1=7.3

 END SUBROUTINE initFoo


  SUBROUTINE makeHandle(handle)
    INTEGER, INTENT(OUT) :: handle(osize)
    !INTEGER, INTENT(OUT) :: handle(*) ! this is not allowed because
    TYPE(fooContainer) :: container

    handle=0
    ALLOCATE(container%pointy)
    CALL initFoo(container%pointy)

    handle=TRANSFER(container,handle)

    !DEALLOCATE(container%pointy)! don't do this



  END SUBROUTINE makeHandle

 END MODULE myTypes

!----------------- 

PROGRAM testAlloc
 USE myTypes
 IMPLICIT NONE

 TYPE(fooContainer) :: container
 INTEGER            :: handle(osize)


 CALL makeHandle(handle)

 !write(*,*) "handle is = ", handle

 container=TRANSFER(handle,container)

 !write(*,*) " foo type writen", container%pointy

 deallocate(container%pointy)

END PROGRAM testAlloc


