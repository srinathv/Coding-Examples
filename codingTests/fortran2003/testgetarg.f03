          PROGRAM test_get_command_argument
            INTEGER :: i
            CHARACTER(len=32) :: arg
          
            i = 0
            DO
              CALL get_command_argument(i, arg)
              IF (LEN_TRIM(arg) == 0) EXIT
          
              WRITE (*,*) TRIM(arg)
              i = i+1
            END DO
          END PROGRAM
     
