

    PROGRAM testBools


    LOGICAL :: a, b, c, d


    a=.true.
    
    b= .true.

    d = a .and. b
    c = (a .and. b)

    IF(c) write(*,*) "C is true"
    IF(d) write(*,*) "D is true"


    END PROGRAM
