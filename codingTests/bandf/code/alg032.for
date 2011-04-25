C***********************************************************************
C                                                                      *
C   NEWTON'S INTERPOLATORY DIVIDED-DIFFERENCE FORMULA ALGORITHM 3.2    *
C                                                                      *
C***********************************************************************
C
C
C
C     TO OBTAIN THE DIVIDED-DIFFERENCE COEFFICIENTS OF THE INTERPOLATORY
C     POLYNOMIAL P ON THE (N+1) DISTINCT NUMBERS X(0), X(1),..., X(N)
C     FOR THE FUNCTION F:
C
C     INPUT NUMBERS X(0),X(1),...,X(N); VALUES F(X(0)),F(X(1)),...,
C           F(X(N)) AS THE FIRST COLUMN Q(0,0),Q(1,0),...,Q(N,0) OF Q.
C
C     OUTPUT THE NUMBERS Q(0,0),Q(1,1),...,Q(N,N) WHERE
C            P(X) = Q(0,0)+Q(1,1)*(X-X(0))+Q(2,2)*(X-X(0))*(X-X(1))+
C                   ...+Q(N,N)*(X-X(0))*(X-X(1))*...*(X-X(N-1)).
C
      DIMENSION X(25),Q(25,25)
      CHARACTER NAME*30,NAME1*30,AA*1
      INTEGER INP,OUP,FLAG
      LOGICAL OK
      F(Z) = 1/Z
      OPEN(UNIT=5,FILE='CON',ACCESS='SEQUENTIAL')
      OPEN(UNIT=6,FILE='CON',ACCESS='SEQUENTIAL')
      WRITE(6,*) 'Newtons form of the Interpolation Polynomial'
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
                  WRITE(6,*) 'Input X(',J,') and F(X(',J,')) '
                  WRITE(6,*) 'separated by space  '
                  WRITE(6,*) ' '
                  READ(5,*) X(I), Q(I,1)
30             CONTINUE
            ELSE
               WRITE(6,*) 'Number must be a positive integer '
            ENDIF
            GOTO 20
         ENDIF
      ENDIF
      IF (FLAG .EQ. 2) THEN
         WRITE(6,*) 'Has a text file been created with data in two '
         WRITE(6,*) 'columns? '
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
                     READ(4,*) X(I) , Q(I,1)
50                CONTINUE
               CLOSE(UNIT=4)
               ELSE
                  WRITE(6,*) 'Number must be a positive integer '
               ENDIF
               GOTO 40
            ENDIF
         ELSE
            WRITE(6,*) 'Please create the input file in two column '
            WRITE(6,*) 'form with the '
            WRITE(6,*) 'X values and F(X) values in the '
            WRITE(6,*) 'corresponding columns '
            WRITE(6,*) 'The program will end so the input file can '
            WRITE(6,*) 'be created. '
            OK = .FALSE.
         ENDIF
      ENDIF
      IF (FLAG .EQ. 3) THEN
         WRITE(6,*) 'Has the function F been created in the program '
         WRITE(6,*) 'immediately proceding the INPUT procedure? '
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
                     Q(I,1) = F( X(I) )
70                CONTINUE
               ELSE
                  WRITE(6,*) 'Number must be a positive integer '
               ENDIF
               GOTO 60
            ENDIF
         ELSE
            WRITE(6,*) 'The program will end so that the function F '
            WRITE(6,*) 'can be created '
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
      WRITE(OUP,3)
      WRITE(OUP,6)
      WRITE(OUP,4) (X(I),Q(I,1),I=1,N)
C     STEP 1
      DO 11 I=2,N
         DO 21 J=2,I
21       Q(I,J)=(Q(I,J-1)-Q(I-1,J-1))/(X(I)-X(I-J+1))
11    CONTINUE
C     STEP 2
      WRITE(OUP,5) (Q(I,I),I=1,N)
400   CLOSE(UNIT=5)
      CLOSE(UNIT=OUP)
      IF(OUP.NE.6) CLOSE(UNIT=6)
      STOP
3     FORMAT(1X,'NEWTONS FORM OF THE INTERPOLATION POLYNOMIAL')
6     FORMAT(1X,'INPUT DATA FOLLOWS')
4     FORMAT(1X,E15.8,1X,E15.8)
5     FORMAT(1X,'Q(0,0),...,Q(N,N)'/4(1X,E15.8))
      END
