C     GAUSSIAN TRIPLE INTEGRAL ALGORITHM 4.6
C
C     To approximate I = triple integral ( ( f(x,y,z) dz dy dx ) ) with
C     limits of integration from a to b for x, from c(x) to d(x) for y,
C     and from alpha(x,y) to beta(x,y) for z.
C
C     INPUT:   endpoints a, b; positive integers m, n, p.  (Assume that the
C              roots r(i,j) and coefficients c(i,j) are available for i
C              equals m, n, and p and for 1 <= j <= i.)
C
C     OUTPUT:  approximation J TO I.
C
      INTEGER P
      REAL JX,K1,K2,JY,L1,L2
      DIMENSION R(5,5),CO(5,5)
      CHARACTER*1 AA
      LOGICAL OK
C     Change functions F,C,D,ALPHA,BETA for a new problem
C     F is the integrand
      F(XZ,YZ,ZZ) = SQRT( XZ * XZ + YZ * YZ )
C     C is the lower limit for Y
      C(XZ) = 0.0
C     D is the upper limit for Y
      D(XZ) = SQRT( 4 - XZ * XZ )
C     ALPHA is the lower limit for Z
      ALPHA(XZ,YZ) = SQRT( XZ * XZ + YZ * YZ )
C     BETA is the upper limit for Z
      BETA(XZ,YZ) = 2
      OPEN(UNIT=5,FILE='CON',ACCESS='SEQUENTIAL')
      OPEN(UNIT=6,FILE='CON',ACCESS='SEQUENTIAL')
      WRITE(6,*) 'This is Gaussian Quadrature for triple integrals.'
      WRITE(6,*) 'Have the functions F, C, D, ALPHA and'
      WRITE(6,*) 'BETA been created?'
      WRITE(6,*) 'Enter Y or N '
      WRITE(6,*) ' '
      READ(5,*)  AA
      IF(( AA .EQ. 'Y' ) .OR. ( AA .EQ. 'y' )) THEN
         OK = .FALSE.
10       IF (OK) GOTO 11
            WRITE(6,*) 'Input lower and upper limits of integration '
            WRITE(6,*) 'of the outer integral, separated'
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
            WRITE(6,*) 'Input three positive integers M, N, P.'
            WRITE(6,*) 'They all must be less than or equal to 5'
            WRITE(6,*) 'for this implementation.'
            WRITE(6,*) 'M will be used for the outer integral,'
            WRITE(6,*) 'N for the center integral and P for the inner'
            WRITE(6,*) 'most integral - separate with blank.'
            WRITE(6,*) ' '
            READ(5,*) M,N,P
            IF(N.GT.0.AND.M.GT.0.AND.P.GT.0.AND.N.LT.6.AND.M.LT.6
     *         .AND.P.LT.6) THEN
              OK=.TRUE.
            ELSE
              WRITE(6,*) 'All three must be positive integers and'
              WRITE(6,*) 'less than or equal t 5. '
              WRITE(6,*)
            ENDIF
         GOTO 14
15       CONTINUE
      ELSE
         WRITE(6,*) 'The program will end so that the functions '
         WRITE(6,*) 'F, C, D, ALPHA and BETA can be created '
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
C     STEP 1
      H1 = ( B - A ) / 2.0
      H2 = ( B + A ) / 2.0
      AJ = 0.0
C     Use AJ instead of J.
C     STEP 2
      DO 20 I = 1, M
C        STEP 3
         X = H1 * R(M,I) + H2
         JX = 0.0
         C1 = C( X )
         D1 = D( X )
         K1 = ( D1 - C1 ) / 2.0
         K2 = ( D1 + C1 ) / 2.0
C        STEP 4
         DO 30 J = 1, N
C           STEP 5
            Y = K1 * R(N,J) + K2
            JY = 0.0
C           Use Z1 for Beta and Z2 for alpha.
            Z1 = BETA(X,Y)
            Z2 = ALPHA(X,Y)
            L1 = (Z1-Z2)/2.0
            L2 = (Z1+Z2)/2.0
C           STEP 6
            DO 40 K = 1, P
               Z = L1 * R(P,K) + L2
               Q = F( X, Y, Z )
               JY = JY + CO(P,K) * Q
40          CONTINUE
C           STEP 7
            JX = JX + CO(N,J) * L1 * JY
30       CONTINUE
C        STEP 8
         AJ = AJ + CO(M,I) * K1 * JX
20    CONTINUE
C     STEP 9
      AJ = AJ * H1
C     STEP 10
      WRITE(6,1) A,B
      WRITE(6,2) AJ
      WRITE(6,3) M,N,P
400   CLOSE(UNIT=5)
      CLOSE(UNIT=6)
      STOP
1     FORMAT(1X,'The integral of F from ',E15.8,' to ',E15.8,' is')
2     FORMAT(1X,E15.8)
3     FORMAT(1X,'obtained with M = ',I3,', N = ',I3,' and P = 'I3)
      END
