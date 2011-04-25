C***********************************************************************
C                                                                      *
C               SIMPSON'S COMPOSITE ALGORITHM 4.1                      *
C                                                                      *
C***********************************************************************
C
C
C
C     TO APPROXIMATE I = INTEGRAL(( F(X) DX)) FROM A TO B:
C
C     INPUT:  ENDPOINTS A, B; EVEN POSITIVE INTEGER N.
C
C     OUTPUT:  APPROXIMATION XI TO I.
C
      REAL A, B, XI0, XI1, XI2, H, XI, X
      INTEGER N, I, NN
      CHARACTER*1 AA
      LOGICAL OK
C     CHANGE FUNCTION F FOR A NEW PROBLEM
      F(XZ) =SIN(XZ)
      OPEN(UNIT=5,FILE='CON',ACCESS='SEQUENTIAL')
      OPEN(UNIT=6,FILE='CON',ACCESS='SEQUENTIAL')
      WRITE(6,*) 'This is Simpsons Method.'
      WRITE(6,*) 'Has the function F been created in the program? '
      WRITE(6,*) 'Enter Y or N '
      WRITE(6,*) ' '
      READ(5,*)  AA
      IF(( AA .EQ. 'Y' ) .OR. ( AA .EQ. 'y' )) THEN
         OK = .FALSE.
10       IF (OK) GOTO 11
            WRITE(6,*) 'Input lower limit of integration and'
            WRITE(6,*) 'upper limit of integration separated'
            WRITE(6,*) 'by a blank.'
            WRITE(6,*) ' '
            READ(5,*) A, B
            IF (A.GE.B) THEN
               WRITE(6,*) 'lower limit must be less than upper limit'
               WRITE(6,*) ' '
            ELSE
               OK = .TRUE.
            ENDIF
         GOTO 10
11       OK = .FALSE.
14       IF (OK) GOTO 15
            WRITE(6,*) 'Input an even positive integer N.'
            WRITE(6,*) ' '
            READ(5,*) N
            IF((N.GT.0).AND.((N/2)*2.EQ.N)) THEN
              OK=.TRUE.
            ELSE
              WRITE(6,*) 'Input must be an even positive integer '
              WRITE(6,*) ' '
            ENDIF
         GOTO 14
15       CONTINUE
      ELSE
         WRITE(6,*) 'The program will end so that the function F '
         WRITE(6,*) 'can be created '
         OK = .FALSE.
      ENDIF
      IF (.NOT.OK) GOTO 040
C     STEP 1
      H = (B-A)/N
C     STEP 2
      XI0 = F(A) + F(B)
C     SUMMATION OF F(X(2*I-1))
      XI1 = 0.0
C     SUMMATION OF F(X(2*I))
      XI2 = 0.0
C     STEP 3
      MM=N-1
      DO 20 I=1,MM
C          STEP 4
           X = A+I*H
C          STEP 5
           IF (I.EQ.2*(I/2)) THEN
                XI2 = XI2+F(X)
           ELSE
                XI1 = XI1+F(X)
           END IF
20    CONTINUE
C     STEP 6
      XI = XI0+2*XI2+4*XI1
      XI = XI*H/3
C     STEP 7
C     OUTPUT
      WRITE(6,2) A,B,XI
040   CLOSE(UNIT=5)
      CLOSE(UNIT=6)
      STOP
2     FORMAT('1','INTEGRAL OF F FROM',3X,E15.8,3X,'TO',3X,E15.8,3X,'IS'
     *,/,3X,E15.8)
      END
