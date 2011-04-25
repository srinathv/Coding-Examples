C***********************************************************************
C                                                                      *
C                     HORNER'S ALGORITHM 2.7                           *
C                                                                      *
C***********************************************************************
C
C
C
C     TO EVALUATE THE POLYNOMIAL
C     P(X) = A(N)*X**N + A(N-1)*X**(N-1) + ... + A(1)*X + A(0)
C     AND ITS DERIVATIVE P'(X) AT X = X0;
C
C     INPUT:  DEGREE N; COEFFICIENTS AA(1),AA(2),...,AA(N+1);
C             VALUE OF X0.
C
C     OUTPUT:  Y = P(X0), Z = P'(X0).
C
C     DEFINE ARRAY AA
      DIMENSION AA(51)
      REAL X0,Y,Z
      INTEGER N,MM,I,J
      LOGICAL OK
      OPEN(UNIT=5,FILE='CON',ACCESS='SEQUENTIAL')
      OPEN(UNIT=6,FILE='CON',ACCESS='SEQUENTIAL')
      WRITE(6,*) 'This is Horners Method.'
      OK = .FALSE.
14    IF (OK) GOTO 15
         WRITE(6,*) 'Input degree of polynomial '
            WRITE(6,*) '- no decimal point '
            WRITE(6,*) ' '
            READ(5,*) N
            IF ( N .LT. 0 ) THEN
              WRITE(6,*) 'Input must be nonnegative integer.'
              WRITE(6,*) ' '
            ELSE
              OK = .TRUE.
            ENDIF
         GOTO 14
15    CONTINUE
      M=N+1
      WRITE(6,*) 'INPUT COEFFICIENTS OF P(X) IN ASCENDING ORDER'
      DO 16 I = 1, M
         J=I-1
         WRITE(6,*) 'Input coefficient of X** ',J
         WRITE(6,*) ' '
         READ(5,*) AA(I)
16    CONTINUE
      WRITE(6,*) 'Input argument X0 at which to evaluate P(X)'
      WRITE(6,*) ' '
      READ(5,*) X0
C     STEP 1
C     COMPUTE B(N) FOR P(X)
      Y=AA(N+1)
C     COMPUTE B(N-1) FOR Q(X)=P'(X)
      IF (N.EQ.0) THEN
         Z = 0
      ELSE
         Z=AA(N+1)
      ENDIF
      MM=N-1
C     STEP 2
      IF (MM.GE.1) THEN
         DO 10 I=1,MM
              J=N+1-I
C             COMPUTE B(J) FOR P(X)
              Y=Y*X0+AA(J)
C             COMPUTE B(J-1) FOR Q(X)
10       Z=Z*X0+Y
      ENDIF
C     STEP 3
C     COMPUTE B(0) FOR P(X)
      IF (N.NE.0) Y=Y*X0+AA(1)
C     STEP 4
      WRITE(6,4)(AA(I),I=1,M)
      WRITE(6,5) X0,Y,X0,Z
040   CLOSE(UNIT=5)
      CLOSE(UNIT=6)
      STOP
4     FORMAT(1X,' COEFFICIENTS OF P ARE',/,6(E15.8,1X))
5     FORMAT(1X,' P(',E15.8,') = ',E15.8,', P''(',E15.8,') = ',E15.8)
      END
