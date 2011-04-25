C***********************************************************************
C                                                                      *
C                 RUNGE-KUTTA (ORDER 4) ALGORITHM 5.2                  *
C                                                                      *
C***********************************************************************
C
C
C
C     TO APPROXIMATE THE SOLUTION TO THE INITIAL VALUE PROBLEM
C             Y' = F(T,Y), A <= T <= B, Y(A) = ALPHA,
C     AT (N+1) EQUALLY SPACED NUMBERS IN THE INTERVAL [A,B].
C
C     INPUT:   ENDPOINTS A,B; INITIAL CONDITION ALPHA; INTEGER N.
C
C     OUTPUT:  APPROXIMATION W TO Y AT THE (N+1) VALUES OF T.
C
      CHARACTER NAME1*30,AA*1
      INTEGER OUP,FLAG
      LOGICAL OK
C     CHANGE FUNCTION F FOR A NEW PROBLEM
      F(TZ,WZ)=WZ-TZ*TZ+1
      OPEN(UNIT=5,FILE='CON',ACCESS='SEQUENTIAL')
      OPEN(UNIT=6,FILE='CON',ACCESS='SEQUENTIAL')
      WRITE(6,*) 'This is the Runge-Kutta Order Four Method.'
      WRITE(6,*) 'Has the function F been created in the program? '
      WRITE(6,*) 'Enter Y or N '
      WRITE(6,*) ' '
      READ(5,*)  AA
      IF(( AA .EQ. 'Y' ) .OR. ( AA .EQ. 'y' )) THEN
         OK = .FALSE.
10       IF (OK) GOTO 11
            WRITE(6,*) 'Input left and right endpoints separated by'
            WRITE(6,*) 'blank '
            WRITE(6,*) ' '
            READ(5,*) A, B
            IF (A.GE.B) THEN
               WRITE(6,*) 'Left endpoint must be less'
               WRITE(6,*) 'than right endpoint'
            ELSE
               OK = .TRUE.
            ENDIF
         GOTO 10
11       OK = .FALSE.
         WRITE(6,*) 'Input the initial condition.'
         WRITE(6,*) ' '
         READ(5,*) ALPHA
12       IF (OK) GOTO 13
            WRITE(6,*) 'Input a positive integer for the number'
            WRITE(6,*) 'of subinvervals '
            WRITE(6,*) ' '
            READ(5,*) N
            IF ( N .LE. 0 ) THEN
              WRITE(6,*) 'Must be positive integer '
            ELSE
              OK = .TRUE.
            ENDIF
         GOTO 12
13       CONTINUE
      ELSE
         WRITE(6,*) 'The program will end so that the function F '
         WRITE(6,*) 'can be created '
         OK = .FALSE.
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
      WRITE(OUP,*) 'RUNGE-KUTTA FOURTH ORDER METHOD'
C     STEP 1
      H = (B-A)/N
      T=A
      W=ALPHA
      WRITE(OUP,2)
      I=0
      WRITE(OUP,3) I,T,W
C     STEP 2
      DO 110 I=1,N
C          STEP 3
C          USE XK1,XK2,XK3,XK4 FOR K(1),K(2),K(3),K(4) RESP.
           XK1 = H*F(T,W)
           XK2 = H*F(T+H/2,W+XK1/2)
           XK3 = H*F(T+H/2,W+XK2/2)
           XK4 = H*F(T+H,W+XK3)
C          STEP 4
C          COMPUTE W(I)
           W = W + (XK1+2*(XK2+XK3)+XK4)/6
C          COMPUTE T(I)
           T=A+I*H
C          STEP 5
110        WRITE(OUP,3) I, T, W
C     STEP 6
400   CLOSE(UNIT=5)
      CLOSE(UNIT=OUP)
      IF(OUP.NE.6) CLOSE(UNIT=6)
      STOP
2     FORMAT(1X,7X,'I',12X,'T',19X,'W')
3     FORMAT(1X,5X,I3,5X,E15.8,5X,E15.8)
      END
