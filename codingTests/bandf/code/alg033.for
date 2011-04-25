C***********************************************************************
C                                                                      *
C                 HERMITE INTERPOLATION ALGORITHM 3.3                  *
C                                                                      *
C***********************************************************************
C
C
C
C     TO OBTAIN THE COEFFICIENTS OF THE HERMITE INTERPOLATING
C     POLYNOMIAL H ON THE (N+1) DISTINCT NUMBERS X(0),...,X(N)
C     FOR THE FUNCTION F:
C
C     INPUT NUMBERS X(0), X(1),..., X(N); VALUES F(X(0)), F(X(1)),...,
C           F(X(N)) AND F'(X(0)), F'(X(1)),..., F'(X(N)).
C
C     OUTPUT NUMBERS Q(0,0), Q(1,1),..., Q(2N+1,2N+1) WHERE
C
C            H(X) = Q(0,0)+Q(1,1)*(X-X(0))+Q(2,2)*(X-X(0))**2+
C                   Q(3,3)*(X-X(0))**2*(X-X(1))+Q(4,4)*(X-X(0))**2*
C                   (X-X(1))**2+...+Q(2N+1,2N+1)*(X-X(0))**2*
C                   (X-X(1))**2*...*(X-X(N-1))**2*(X-X(N)).
C
C     DEFINE STORAGE FOR X, Z, Q
      DIMENSION X(12),Z(25),Q(25,25)
      INTEGER I,J,K,N,INP,OUP,FLAG
      CHARACTER NAME*30,NAME1*30,AA*1
      LOGICAL OK
      F(AX)=1/AX
      FP(AX)=-1/(AX*AX)
      OPEN(UNIT=5,FILE='CON',ACCESS='SEQUENTIAL')
      OPEN(UNIT=6,FILE='CON',ACCESS='SEQUENTIAL')
      WRITE(6,*) 'This is Hermite Interpolation.'
      OK = .FALSE.
10    IF ( .NOT. OK) THEN
         WRITE(6,*) 'Choice of input method: '
         WRITE(6,*) '1. Input entry by entry from keyboard '
         WRITE(6,*) '2. Input data from a text file '
         WRITE(6,*) '3. Generate data using a function F '
         WRITE(6,*) 'Choose 1, 2, or 3 please '
         WRITE(6,*) ' '
         READ(5,*)  FLAG
         IF( ( FLAG .GE. 1 ) .AND. ( FLAG .LE. 3 )) OK = .TRUE.
         GOTO 10
      ENDIF
      IF (FLAG .EQ. 1) THEN
         OK = .FALSE.
20       IF (.NOT. OK ) THEN
            WRITE(6,*) 'Input number N '
            WRITE(6,*) ' '
            READ(5,*) N
            IF (N .GT. 0 ) THEN
               OK = .TRUE.
               N=N+1
               DO 30 I = 1, N
                  J=I-1
                  WRITE(6,*) 'Input X(',J,'),F(X(',J,')), and '
                  WRITE(6,*) 'F''(X(',J,')) separated by spaces'
                  WRITE(6,*) ' '
                  J1=2*I-1
                  J2=2*I
                  READ(5,*) X(I),Q(J1,1),Q(J2,2)
30             CONTINUE
            ELSE
               WRITE(6,*) 'Number must be a positive integer. '
            ENDIF
            GOTO 20
         ENDIF
      ENDIF
      IF (FLAG .EQ. 2) THEN
         WRITE(6,*) 'Has a text file been created with data in '
         WRITE(6,*) 'three columns? '
         WRITE(6,*) 'Enter Y or N - letter within quotes '
         WRITE(6,*) ' '
         READ(5,*)  AA
         IF (( AA .EQ. 'Y' ) .OR.( AA .EQ. 'y' )) THEN
            WRITE(6,*) 'Input the file name in the form - '
            WRITE(6,*) 'drive:name.ext  contained in quotes'
            WRITE(6,*) 'as example:   ''A:DATA.DTA'' '
            WRITE(6,*) ' '
            READ(5,*)  NAME
            INP = 4
            OPEN(UNIT=INP,FILE=NAME,ACCESS='SEQUENTIAL')
            OK = .FALSE.
40          IF (.NOT. OK) THEN
               WRITE(6,*) 'Input number N '
               WRITE(6,*) ' '
               READ(5,*) N
               IF ( N .GT. 0) THEN
                  OK = .TRUE.
                  N=N+1
                  DO 50 I = 1, N
                     J1=2*I-1
                     J2=2*I
                     READ(4,*) X(I),Q(J1,1),Q(J2,2)
50                CONTINUE
               CLOSE(UNIT=4)
               ELSE
                  WRITE(6,*) 'Number must be a positive integer. '
               ENDIF
               GOTO 40
            ENDIF
         ELSE
            WRITE(6,*) 'Please create the input file in three column'
            WRITE(6,*) 'form with the X values, F(X) values and'
            WRITE(6,*) 'F''(X) values in the corresponding columns.'
            WRITE(6,*) 'The program will end so the input file can '
            WRITE(6,*) 'be created. '
            OK = .FALSE.
         ENDIF
      ENDIF
      IF (FLAG .EQ. 3) THEN
         WRITE(6,*) 'Have the functions F and F'' been created?'
         WRITE(6,*) 'Enter Y or N - letter within quotes'
         WRITE(6,*) ' '
         READ(5,*)  AA
         IF (( AA .EQ. 'Y' ) .OR. ( AA .EQ. 'y' )) THEN
            OK = .FALSE.
60          IF (.NOT. OK) THEN
               WRITE(6,*) 'Input number N '
               WRITE(6,*) ' '
               READ(5,*) N
               IF ( N .GT. 0 ) THEN
                  OK = .TRUE.
                  N=N+1
                  DO 70 I = 1, N
                     J=I-1
                     WRITE(6,*) 'Input X(',J,') '
                     WRITE(6,*) ' '
                     READ(5,*)  X(I)
                     Q(2*I-1,1) = F( X(I) )
                     Q(2*I,2) = FP( X(I) )
70                CONTINUE
               ELSE
                  WRITE(6,*) 'Number must be a positive integer. '
               ENDIF
               GOTO 60
            ENDIF
         ELSE
            WRITE(6,*) 'The program will end so that the functions '
            WRITE(6,*) 'F and F''can be created '
            OK = .FALSE.
         ENDIF
      ENDIF
      IF(.NOT.OK) GOTO 400
      WRITE(6,*) 'Select output destination: '
      WRITE(6,*) '1. Screen '
      WRITE(6,*) '2. Text file '
      WRITE(6,*) 'Enter 1 or 2 '
      WRITE(6,*) ' '
      READ(5,*) FLAG
      IF ( FLAG .EQ. 2 ) THEN
         WRITE(6,*) 'Input the file name in the form - '
         WRITE(6,*) 'drive:name.ext'
         WRITE(6,*) 'with the name contained within quotes'
         WRITE(6,*) 'as example:   ''A:OUTPUT.DTA'' '
         WRITE(6,*) ' '
         READ(5,*) NAME1
         OUP = 3
         OPEN(UNIT=OUP,FILE=NAME1,STATUS='NEW')
      ELSE
         OUP = 6
      ENDIF
      WRITE(OUP,*) 'HERMITE INTERPOLATING POLYNOMIAL'
C     STEP 1
      DO 21 I=1,N
C        STEP 2
         Z(2*I-1)=X(I)
         Z(2*I)=X(I)
         Q(2*I,1)=Q(2*I-1,1)
C        STEP 3
         IF(I.NE.1) THEN
            Q(2*I-1,2)=(Q(2*I-1,1)-Q(2*I-2,1))/(Z(2*I-1)-Z(2*I-2))
         ENDIF
21       CONTINUE
C     STEP 4
      K=2*N
      DO 31 I=3,K
         DO 31 J=3,I
31       Q(I,J)=(Q(I,J-1)-Q(I-1,J-1))/(Z(I)-Z(I-J+1))
C     STEP 5
      WRITE(OUP,*) 'The input data follows'
      WRITE(OUP,2)
2     FORMAT(2X,'I',16X,'X',13X,'F(X)',12X,'F''(X)')
      DO 41 I=1,N
         J=I-1
         J1=2*I-1
         J2=2*I
         WRITE(OUP,1) J,X(I),Q(J1,1),Q(J2,2)
1        FORMAT(1X,I2,2X,E15.8,2X,E15.8,2X,E15.8)
41    CONTINUE
      WRITE(OUP,*) 'The coefficients of the Hermite Interpolation'
      WRITE(OUP,*) 'Polynomial in order of increasing exponent:'
      DO 51 I=1,K
         WRITE(OUP,4) Q(I,I)
4        FORMAT(1X,E15.8)
51    CONTINUE
400   CLOSE(UNIT=5)
      CLOSE(UNIT=OUP)
      IF(OUP.NE.6) CLOSE(UNIT=6)
      STOP
      END
