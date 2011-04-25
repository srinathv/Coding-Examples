C***********************************************************************
C                                                                      *
C                  DOUBLE INTEGRAL ALGORITHM 4.5                       *
C                                                                      *
C***********************************************************************
C
C
C
C     TO APPROXIMATE I = DOUBLE INTEGRAL (( F(X,Y) DY DX )) WITH LIMITS
C         OF INTEGRATION FROM A TO B FOR X AND FROM C(X) TO D(X) FOR Y:
C
C     INPUT:   ENDPOINTS A,B; POSITIVE INTEGERS M, N.  (ASSUME THAT THE
C              ROOTS R(I,J) AND COEFFICIENTS C(I,J) ARE AVAILABLE FOR
C              I EQUALS M AND N AND FOR 1<=J<=I.)
C
C     OUTPUT:  APPROXIMATION J TO I.
C
      REAL JX,K1,K2
      DIMENSION R(5,5),CO(5,5)
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
      WRITE(6,*) 'This is Gaussian Quadrature for double integrals.'
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
            WRITE(6,*) 'Both must be less than or equal to 5'
            WRITE(6,*) 'for this implementation.'
            WRITE(6,*) 'Gaussian Quadrature uses M for the outer'
            WRITE(6,*) 'integral and N for the inner'
            WRITE(6,*) 'integral - separate with blank.'
            WRITE(6,*) ' '
            READ(5,*) M,N
            IF(M.LT.6.AND.N.LT.6.AND.N.GT.0.AND.M.GT.0) THEN
              OK=.TRUE.
            ELSE
              WRITE(6,*) 'Both must be positive integers less than'
              WRITE(6,*) 'or equal to 5 '
              WRITE(6,*)
            ENDIF
         GOTO 14
15       CONTINUE
      ELSE
         WRITE(6,*) 'The program will end so that the functions'
         WRITE(6,*) 'F, C and D can be created '
         OK = .FALSE.
      ENDIF
      IF (.NOT.OK) GOTO 400
      R(2,1) = 0.5773502692
      R(2,2) = -R(2,1)
      CO(2,1) = 1.0
      CO(2,2) = 1.0
      R(3,1) = 0.7745966692
      R(3,2) = 0.0
      R(3,3) = -R(3,1)
      CO(3,1) = 0.5555555556
      CO(3,2) = 0.8888888889
      CO(3,3) = CO(3,1)
      R(4,1) = 0.8611363116
      R(4,2) = 0.3399810436
      R(4,3) = -R(4,2)
      R(4,4) = -R(4,1)
      CO(4,1) = 0.3478548451
      CO(4,2) = 0.6521451549
      CO(4,3) = CO(4,2)
      CO(4,4) = CO(4,1)
      R(5,1) = 0.9061798459
      R(5,2) = 0.5384693101
      R(5,3) = 0.0
      R(5,4) = -R(5,2)
      R(5,5) = -R(5,1)
      CO(5,1) = 0.2369268850
      CO(5,2) = 0.4786286705
      CO(5,3) = 0.5688888889
      CO(5,4) = CO(5,2)
      CO(5,5) = CO(5,1)
C        STEP 1
      H1 = ( B - A ) / 2.0
      H2 = ( B + A ) / 2.0
C     USE AJ INSTEAD OF J
      AJ = 0.0
C       STEP 2
         DO 20 I = 1,M
C              STEP 3
               X = H1 * R(M,I) + H2
               JX = 0.0
               C1 = C( X )
               D1 = D( X )
               K1 = ( D1 - C1 ) / 2.0
               K2 = ( D1 + C1 ) / 2.0
C              STEP 4
               DO 30 J = 1,N
                     Y = K1 * R(N,J) + K2
                     Q = F( X, Y )
                     JX = JX + CO(N,J) * Q
30             CONTINUE
C              STEP 5
               AJ = AJ + CO(M,I) * K1 * JX
20       CONTINUE
C        STEP 6
         AJ = AJ * H1
C        STEP 7
C     OUTPUT
      WRITE(6,1) A,B
      WRITE(6,2) AJ
      WRITE(6,3) N,M
400   CLOSE(UNIT=5)
      CLOSE(UNIT=6)
      STOP
1     FORMAT(1X,'The integral of F from ',E15.8,' to ',E15.8,' is')
2     FORMAT(1X,E15.8)
3     FORMAT(1X,'obtained with N = ',I3,' and M = ',I3)
      END
