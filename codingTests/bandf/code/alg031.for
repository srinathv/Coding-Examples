C***********************************************************************
C                                                                      *
C       NEVILLE'S INTERATED INTERPOLATION ALGORITHM 3.1                *
C                                                                      *
C***********************************************************************
C
C
C
C
C     TO EVALUATE THE INTERPOLATING POLYNOMIAL P ON THE
C     (N+1) DISTINCT NUMBERS X(0) ,..., X(N) AT THE NUMBER X
C     FOR THE FUNCTION F:
C
C     INPUT:  NUMBERS X(0) ,..., X(N) AS XX(1) ,..., XX(N+1);
C             NUMBER X; VALUES OF F AS THE FIRST COLUMN OF Q
C             OR MAY BE COMPUTED IF FUNCTION F IS SUPPLIED.
C
C     OUTPUT:  THE TABLE Q WITH P(X) = Q(N+1,N+1).
C
C     DEFINE STORAGE FOR XX AND Q
      DIMENSION XX(10), Q(10,10), D(10)
      REAL X
      INTEGER I,J,N,FLAG,INP,OUP
      CHARACTER NAME*30,NAME1*30,AA*1
      LOGICAL OK
      F(Z) = 1/Z
      OPEN(UNIT=5,FILE='CON',ACCESS='SEQUENTIAL')
      OPEN(UNIT=6,FILE='CON',ACCESS='SEQUENTIAL')
      OK = .FALSE.
      WRITE(6,*) 'This is Nevilles Method.'
10    IF ( .NOT. OK) THEN
         WRITE(6,*) 'Choice of input method: '
         WRITE(6,*) '1. Input entry by entry from keyboard '
         WRITE(6,*) '2. Input data from a text file '
         WRITE(6,*) '3. Generate data using a function F with nodes'
         WRITE(6,*) 'entered from keyboard '
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
                  WRITE(6,*) 'separated by space '
                  WRITE(6,*) ' '
                  READ(5,*) XX(I), Q(I,1)
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
                     READ(4,*) XX(I) , Q(I,1)
50                CONTINUE
               CLOSE(UNIT=4)
               ELSE
                  WRITE(6,*) 'Number must be a positive integer. '
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
                     READ(5,*)  XX(I)
                     Q(I,1) = F( XX(I) )
70                CONTINUE
               ELSE
                  WRITE(6,*) 'Number must be a positive integer. '
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
      WRITE(6,*) 'Input the point at which the polynomial is'
      WRITE(6,*) 'to be evaluated.'
      WRITE(6,*) ' '
      READ(5,*) X
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
      WRITE(OUP,*) 'THIS IS THE NEVILLE METHOD'
C     STEP 1
      D(1) = X-XX(1)
      DO 11 I=2,N
           D(I) = X-XX(I)
           DO 11 J=2,I
11    Q(I,J) = (D(I)*Q(I-1,J-1)-D(I-J+1)*Q(I,J-1))/(D(I)-D(I-J+1))
C     STEP 2
C     OUTPUT
      WRITE(OUP,3) X
      M=N-1
      WRITE(OUP,4) M
      DO 21 I=1,N
21    WRITE(OUP,5) XX(I),(Q(I,J),J=1,I)
400   CLOSE(UNIT=5)
      CLOSE(UNIT=OUP)
      IF(OUP.NE.6) CLOSE(UNIT=6)
      STOP
3     FORMAT(1X,'TABLE FOR P EVALUATED AT X = ',E15.8,' FOLLOWS')
4     FORMAT(1X,'ENTRIES ARE XX(1),Q(I,1) ,..., Q(I,I) FOR EACH I
     *= 1 ,..., N+1',/,'WHERE N = ',I2)
5     FORMAT((1X,5(1X,E15.8)))
      END
