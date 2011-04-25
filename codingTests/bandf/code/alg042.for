C***********************************************************************
C                                                                      *
C                       ROMBERG ALGORITHM 4.2                          *
C                                                                      *
C***********************************************************************
C
C
C
C     TO APPROXIMATE I = INTEGRAL((F(X) DX)) FROM A TO B:
C
C     INPUT:  ENDPOINTS A, B; INTEGER N.
C
C     OUTPUT:  AN ARRAY R. ( R(2,N) IS THE APPROXIMATION TO I.)
C
C     R IS COMPUTED BY ROWS; ONLY 2 ROWS SAVED IN STORAGE
C
C     DEFINE STORAGE FOR TWO ROWS OF THE TABLE
      DIMENSION R(2,15)
      CHARACTER*1 AA
      LOGICAL OK
C     CHANGE FUNCTION F FOR A NEW PROBLEM
      F(XZ) =SIN(XZ)
      OPEN(UNIT=5,FILE='CON',ACCESS='SEQUENTIAL')
      OPEN(UNIT=6,FILE='CON',ACCESS='SEQUENTIAL')
      WRITE(6,*) 'This is Romberg Integration.'
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
            WRITE(6,*) 'Input the number of rows - no decimal point.'
            WRITE(6,*) ' '
            READ(5,*) N
            IF(N.GT.0) THEN
              OK=.TRUE.
            ELSE
              WRITE(6,*) 'Must be positive integer '
              WRITE(6,*) ' '
            ENDIF
         GOTO 14
15       CONTINUE
      ELSE
         WRITE(6,*) 'The program will end so that the function F '
         WRITE(6,*) 'can be created '
         OK = .FALSE.
      ENDIF
      IF (.NOT.OK) GOTO 400
C     STEP 1
      H = B-A
      R(1,1) = (F(A)+F(B))/2*H
C     STEP 2
      WRITE(6,2) R(1,1)
C     STEP 3
      DO 20 I=2,N
C          STEP 4
C          APPROXIMATION FROM TRAPEZOIDAL METHOD
           SUM = 0.0
           M = 2**(I-2)
           DO 30 K=1,M
30         SUM = SUM+F(A+(K-.5)*H)
           R(2,1) = (R(1,1)+H*SUM)/2
C          STEP 5
C          EXTRAPOLATION
           DO 40 J=2,I
                L = 2**(2*(J-1))
40         R(2,J) = R(2,J-1)+(R(2,J-1)-R(1,J-1))/(L-1)
C          STEP 6
C          OUTPUT
           WRITE(6,2) (R(2,K),K=1,I)
C          STEP 7
           H = H/2
C          STEP 8
C          SINCE ONLY TWO ROWS ARE KEPT IN STORAGE, THIS STEP
C          IS TO PREPARE FOR THE NEXT ROW.
C          UPDATE ROW 1 OF R
           DO 20 J=1,I
20    R(1,J) = R(2,J)
C     STEP 9
400   CLOSE(UNIT=5)
      CLOSE(UNIT=6)
      STOP
2     FORMAT(1X,(6(3X,E15.8)))
      END
