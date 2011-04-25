C***********************************************************************
C                                                                      *
C                  DOUBLE INTEGRAL ALGORITHM 4.4                       *
C                                                                      *
C***********************************************************************
C
C
C
C     TO APPROXIMATE I = DOUBLE INTEGRAL (( F(X,Y) DY DX )) WITH LIMITS
C         OF INTEGRATION FROM A TO B FOR X AND FROM C(X) TO D(X) FOR Y:
C
C     INPUT:   ENDPOINTS A,B; POSITIVE INTEGERS M, N.
C
C     OUTPUT:  APPROXIMATION J TO I.
C
      CHARACTER*1 AA
      LOGICAL OK
C     CHANGE FUNCTIONS F, C, D FOR A NEW PROBLEM
C     LIMITS OF INTEGRATION
C     C IS THE LOWER LIMIT OF Y
      C(XZ)=XZ**3
C     D IS THE UPPER LIMIT OF Y
      D(XZ)=XZ**2
C     DEFINE INTEGRAND FUNCTION F(X,Y)
      F(XZ,YZ)=EXP(YZ/XZ)
      OPEN(UNIT=5,FILE='CON',ACCESS='SEQUENTIAL')
      OPEN(UNIT=6,FILE='CON',ACCESS='SEQUENTIAL')
      WRITE(6,*) 'This is Simpsons Method for double integrals.'
      WRITE(6,*) 'Have the functions F, C, and D been created?'
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
            WRITE(6,*) 'Input two positive integers M, N.'
            WRITE(6,*) 'There will be 2M subintervals for the outer'
            WRITE(6,*) 'integral and 2N subintervals for the inner'
            WRITE(6,*) 'integral - separate with blank.'
            WRITE(6,*) ' '
            READ(5,*) N,M
            IF((N.GT.0).AND.(M.GT.0)) THEN
              OK=.TRUE.
            ELSE
              WRITE(6,*) 'Must be positive integers '
              WRITE(6,*) ' '
            ENDIF
         GOTO 14
15       CONTINUE
      ELSE
         WRITE(6,*) 'The program will end so that the functions'
         WRITE(6,*) 'F, C and D can be created '
         OK = .FALSE.
      ENDIF
      IF (.NOT.OK) GOTO 400
      NN=2*N+1
      MM=2*M-1
C     STEP 1
      H=(B-A)/(2*N)
C     USE AN, AE, AO FOR J(1), J(2), J(3) RESP.
C
C     END TERMS
      AN=0
C     EVEN TERMS
      AE=0
C     ODD TERMS
      AO=0
C     STEP 2
C     TO AVOID A ZERO SUBSCRIPT THE INDEX HAS BEEN SHIFTED BY ONE
      DO 20 I=1,NN
C        STEP 3
C        COMPOSITE SIMPSON'S METHOD FOR X
         X=A+(I-1)*H
         YA = C(X)
         YB = D(X)
         HX=(YB-YA)/(2*M)
C        USE BN, BE, BO FOR K(1), K(2), K(3)
C
C        END TERMS
         BN=F(X,YA)+F(X,YB)
C        EVEN TERMS
         BE=0
C        ODD TERMS
         BO=0
C        STEP 4
         DO 30 J=1,MM
C           STEP 5
            Y=YA+J*HX
            Z=F(X,Y)
C           STEP 6
            IF(J.EQ.2*(J/2)) THEN
               BE=BE+Z
            ELSE
               BO=BO+Z
            END IF
30       CONTINUE
C     STEP 7
C     USE A1 FOR L, WHICH IS THE INTEGRAL OF F(X(I),Y) FROM C(X(I))
C        TO D(X(I)) BY COMPOSITE SIMPSON'S METHOD
      A1=(BN+2*BE+4*BO)*HX/3
C     STEP 8
      IF( I.EQ.1 .OR. I.EQ.NN ) THEN
         AN=AN+A1
      ELSE
         IF(I.EQ.2*(I/2)) THEN
             AO=AO+A1
         ELSE
             AE=AE+A1
         END IF
      END IF
20    CONTINUE
C     STEP 9
C     USE AC FOR J
      AC=(AN+2*AE+4*AO)*H/3
C     STEP 10
C     OUTPUT
      WRITE(6,1) A,B
      WRITE(6,2) AC
      WRITE(6,3) N,M
400   CLOSE(UNIT=5)
      CLOSE(UNIT=6)
      STOP
1     FORMAT(1X,'The integral of F from ',E15.8,' to ',E15.8,' is')
2     FORMAT(1X,E15.8)
3     FORMAT(1X,'obtained with N = ',I3,' and M = ',I3)
      END
