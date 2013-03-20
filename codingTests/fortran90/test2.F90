
      PROGRAM test2

        real :: a= 10.

#if defined(HAVE_A) || defined(HAVE_B)
  write(*,*) "in the if block"
#endif

      WRITE(6,*) "HELLO WORLD"
      WRITE(6,*) "log of 10 is ", LOG(a)

      END PROGRAM test2
