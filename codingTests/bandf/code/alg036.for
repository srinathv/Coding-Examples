C***********************************************************************
C                                                                      *
C        BEZIER CURVE ALGORITHM 3.6                                    *
C                                                                      *
C***********************************************************************
C
C
C     To construct the cubic Bezier curves C0, ..., Cn-1 in
C     parameter form, where Ci is represented by
C
C     (xi(t),yi(t)) = ( a0(i) + a1(i)*t + a2(i)*t^2 + a3(i)*t^3,
C                     b0(i) + b1(i)*t + b2(i)*t^2 + b3(i)*t^3)
C
C     for 0 <= t <= 1 as determined by the left endpoint (x(i),y(i)),
C     left guidepoint (x+(i),y+(i)), right endpoint (x(i+1),y(i+1)) and
C     right guidepoint (x-(i+1),y-(i+1)) for each i = 0, 1, ... , n-1;
C
C     INPUT  n, ( (x(i),y(i)), i = 0,...,n ),
C             ( (x+(i),y+(i)), i = 0,...,n-1 ),
C             ( (x-(i),y-(i)), i = 1,...,n ).
C
C     OUTPUT coefficients ( a0(i), a1(i), a2(i), a3(i),
C                       b0(i), b1(i), b2(i), b3(i), i = 0, ... , n-1 ).
C
      DIMENSION A0(25),A1(25),A2(25),A3(25),B0(25),B1(25),B2(25),B3(25)
      DIMENSION X(26),Y(26),XPL(25),YPL(25),XMI(25),YMI(25)
      CHARACTER NAME*30,NAME1*30,AA*1
      INTEGER INP,OUP,FLAG
      LOGICAL OK
      OPEN(UNIT=5,FILE='CON',ACCESS='SEQUENTIAL')
      OPEN(UNIT=6,FILE='CON',ACCESS='SEQUENTIAL')
      WRITE(6,*) 'This is the Bezier Curve Algorithm.'
      OK = .FALSE.
10    IF ( .NOT. OK) THEN
         WRITE(6,*) 'Choice of input method: '
         WRITE(6,*) '1. Input entry by entry from keyboard '
         WRITE(6,*) '2. Input data from a text file '
         WRITE(6,*) 'Choose 1 or 2 please '
         WRITE(6,*) ' '
         READ(5,*)  FLAG
         IF( ( FLAG .GE. 1 ) .AND. ( FLAG .LE. 2 )) OK = .TRUE.
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
               WRITE(6,*) 'Input X(0),Y(0),X+(0),Y+(0)'
               WRITE(6,*) 'separated by space'
               READ(5,*) X(1),Y(1),XPL(1),YPL(1)
               DO 11 I = 2, N
                  J=I-1
                  WRITE(6,*) 'Input X(',J,'),Y(',J,') '
                  WRITE(6,*) 'separated by space '
                  READ(5,*) X(I), Y(I)
                  WRITE(6,*) 'Input X-(',J,'),Y-(',J,') '
                  WRITE(6,*) 'separated by space '
                  READ(5,*) XMI(I), YMI(I)
                  WRITE(6,*) 'Input X+(',J,'),Y+(',J,') '
                  WRITE(6,*) 'separated by space '
                  READ(5,*) XPL(I), YPL(I)
11             CONTINUE
               WRITE(6,*) 'Input X(n),Y(n),X-(n),Y-(n)'
               WRITE(6,*) 'separated by a space'
               READ(5,*) X(N+1),Y(N+1),XMI(N+1),YMI(N+1)
            ELSE
               WRITE(6,*) 'Number must be a positive integer. '
            ENDIF
            GOTO 20
         ENDIF
      ENDIF
      IF (FLAG .EQ. 2) THEN
         WRITE(6,*) 'Has a text file been created with the data as '
         WRITE(6,*) 'follows ? '
         WRITE(6,*) ' '
         WRITE(6,*) 'X[0]    Y[0]    X+[0]    Y+[0]'
         WRITE(6,*) 'X[1]    Y[1]    X-[1]    Y-[1]    X+[1]    Y+[1]'
         WRITE(6,*) '...'
         WRITE(6,*) 'X[n-1]  Y[n-1]  X-[n-1]  Y-[n-1]  X+[n-1]  Y+[n-1]'
         WRITE(6,*) 'X[n]    Y[n]    X-[n]    Y-[n]'
         WRITE(6,*) ' '
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
                  READ(4,*) X(1),Y(1),XPL(1),YPL(1)
                  DO 31 I = 2, N
                    READ(4,*) X(I),Y(I),XMI(I),YMI(I),XPL(I),YPL(I)
31                CONTINUE
                  READ(4,*) X(N+1),Y(N+1),XMI(N+1),YMI(N+1)
               CLOSE(UNIT=4)
               ELSE
                  WRITE(6,*) 'Number must be a positive integer. '
               ENDIF
               GOTO 40
            ENDIF
         ELSE
            WRITE(6,*) 'Please create the input file as indicated. '
            WRITE(6,*) 'The program will end so the input file can '
            WRITE(6,*) 'be created. '
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
      WRITE(OUP,*) 'BEZIER CURVE ALGORITHM'
C     STEP 1
      DO 22 I = 1, N
C        STEP 2
         A0(I) = X(I)
         B0(I) = Y(I)
         A1(I) = 3*(XPL(I) - X(I))
         B1(I) = 3*(YPL(I) - Y(I))
         A2(I) = 3*(X(I)+XMI(I+1)-2*XPL(I))
         B2(I) = 3*(Y(I)+YMI(I+1)-2*YPL(I))
         A3(I) = X(I+1)-X(I)+3*XPL(I)-3*XMI(I+1)
         B3(I) = Y(I+1)-Y(I)+3*YPL(I)-3*YMI(I+1)
C        STEP 3
         J=I-1
         WRITE(OUP,*) 'I = ',J
         WRITE(OUP,*) 'A0 ','A1 ','A2 ','A3 '
         WRITE(OUP,*) A0(I),A1(I),A2(I),A3(I)
         WRITE(OUP,*) 'B0 ','B1 ','B2 ','B3 '
         WRITE(OUP,*) B0(I),B1(I),B2(I),B3(I)
22    CONTINUE
C     STEP 4
400   CLOSE(UNIT=5)
      CLOSE(UNIT=OUP)
      IF(OUP.NE.6) CLOSE(UNIT=6)
      STOP
      END
