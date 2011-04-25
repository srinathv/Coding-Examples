C***********************************************************************
C                                                                      *
C                ADAPTIVE QUADRATURE ALGORITHM 4.3                     *
C                                                                      *
C***********************************************************************
C
C
C
C     TO APPROXIMATE THE I = INTEGRAL ((F(X) DX)) FORM A TO B TO WITHIN
C     A GIVEN TOLERANCE TOL:
C
C     INPUT:   ENDPOINTS A,B;TOLERANCE TOL
C              LIMIT N TO NUMBER OF LEVELS
C
C     OUTPUT:  APPROXIMATION APP OR A MESSAGE THAT N IS EXCEEDED.
C
      DIMENSION TOL(20),A(20),H(20),FA(20),FC(20),FB(20),S(20),L(20)
      DIMENSION V(8)
      CHARACTER*1 A1
      LOGICAL OK
C     CHANGE F FOR A NEW PROBLEM
      F(XZ)=  100/XZ**2*SIN(10/XZ)
      OPEN(UNIT=5,FILE='CON',ACCESS='SEQUENTIAL')
      OPEN(UNIT=6,FILE='CON',ACCESS='SEQUENTIAL')
      WRITE(6,*) 'This is Adaptive Quadrature with Simpsons Method.'
      WRITE(6,*) 'Has the function F been created in the program? '
      WRITE(6,*) 'Enter Y or N '
      WRITE(6,*) ' '
      READ(5,*)  A1
      IF(( A1 .EQ. 'Y' ) .OR. ( A1 .EQ. 'y' )) THEN
         OK = .FALSE.
10       IF (OK) GOTO 11
            WRITE(6,*) 'Input lower limit of integration and'
            WRITE(6,*) 'upper limit of integration separated'
            WRITE(6,*) 'by a blank.'
            WRITE(6,*) ' '
            READ(5,*) AA, BB
            IF (AA.GE.BB) THEN
               WRITE(6,*) 'lower limit must be less than upper limit'
               WRITE(6,*) ' '
            ELSE
               OK = .TRUE.
            ENDIF
         GOTO 10
11       OK = .FALSE.
12       IF(OK) GOTO 13
            WRITE(6,*) 'Input tolerance'
            WRITE(6,*) ' '
            READ(5,*) EPS
            IF(EPS.LE.0.0) THEN
               WRITE(6,*) 'Tolerance must be positive.'
               WRITE(6,*) ' '
            ELSE
               OK=.TRUE.
            ENDIF
         GOTO 12
13       OK=.FALSE.
14       IF (OK) GOTO 15
            WRITE(6,*) 'Input the maximum number of levels.'
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
      IF (.NOT.OK) GOTO 040
C     STEP 1
      APP = 0
      I = 1
      TOL(I) = 10*EPS
      A(I) = AA
      H(I) = (BB-AA)/2
      FA(I) = F(AA)
      FC(I) = F(AA+H(I))
      FB(I) = F(BB)
C     APPROXIMATION FROM SIMPSON'S METHOD FOR ENTIRE INTERVAL
      S(I) = H(I)*(FA(I)+4*FC(I)+FB(I))/3
      L(I) = 1
C     STEP 2
22    IF (I .LE. 0) GOTO 21
C           STEP 3
            FD = F(A(I)+H(I)/2)
            FE = F(A(I)+3*H(I)/2)
C           APPROXIMATIONS FROM SIMPSON'S METHOD FOR HALVES OF INTERVALS
            S1 = H(I)*(FA(I)+4*FD+FC(I))/6
            S2 = H(I)*(FC(I)+4*FE+FB(I))/6
C           SAVE DATA AT THIS LEVEL
            V(1)=A(I)
            V(2)=FA(I)
            V(3)=FC(I)
            V(4)=FB(I)
            V(5)=H(I)
            V(6)=TOL(I)
            V(7)=S(I)
            V(8)=L(I)
C           STEP 4
C           DELETE THE LEVEL
            I=I-1
C           STEP 5
            IF( ABS(S1+S2-V(7)) .LT. V(6)) THEN
                APP = APP+(S1+S2)
            ELSE
                IF( V(8) .GE. N ) THEN
C                   PROCEDURE FAILS
                    WRITE(6,2)
                    GOTO 040
                ELSE
C                   ADD ONE LEVEL
C                   DATA FOR RIGHT HALF SUBINTERVAL
                    I = I+1
                    A(I) = V(1) + V(5)
                    FA(I) = V(3)
                    FC(I) = FE
                    FB(I) = V(4)
                    H(I) = V(5)/2
                    TOL(I) = V(6)/2
                    S(I) = S2
                    L(I) = V(8) + 1
C                   DATA FOR LEFT HALF SUBINTERVAL
                    I = I+1
                    A(I) = V(1)
                    FA(I) = V(2)
                    FC(I) = FD
                    FB(I) = V(3)
                    H(I) = H(I-1)
                    TOL(I) = TOL(I-1)
                    S(I) = S1
                    L(I) = L(I-1)
                END IF
            END IF
      GOTO 22
C     STEP 6
C     OUTPUT
C     APP APPROXIMATES I TO WITHIN EPS
21    CONTINUE
      WRITE(6,4) AA,BB
      WRITE(6,3) APP,EPS
040   CLOSE(UNIT=5)
      CLOSE(UNIT=6)
      STOP
2     FORMAT(1X,'LEVEL EXCEEDED')
3     FORMAT(3X,E15.8,' TO WITHIN ',E15.8)
4     FORMAT(1X,'THE INTEGRAL FROM ',E15.8,' TO ',E15.8,' IS')
      END
