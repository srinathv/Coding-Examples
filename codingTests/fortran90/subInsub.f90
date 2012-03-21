    SUBROUTINE foo(a,b,c,d)
    INTEGER ,INTENT(IN) :: a,b
    INTEGER , INTENT(OUT) :: c,d


    call bar(a,b,c)
    d=c+2
    
    contains
    SUBROUTINE bar(a,b,c)
    INTEGER,INTENT(IN) ::  a,b
    INTEGER,INTENT(OUT)  :: c 

    c=a+b

    END SUBROUTINE bar
    END SUBROUTINE foo


    PROGRAM subInSub
    INTEGER :: a,b,c,d

    a=1
    b=2
    
    call foo(a,b,c,d)
  
    write(*,*) "c=",c  
    write(*,*) "d=",d  

  
    END PROGRAM subInSub



