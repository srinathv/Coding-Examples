C***********************************************************************
C                                                                      *
C   RUNGE-KUTTA FOR SYSTEMS OF DIFFERENTIAL EQUATIONS ALGORITHM 5.7    *
C                                                                      *
C***********************************************************************
C
C
C
C     TO APPROXIMATE THE SOLUTION OF THE MTH-ORDER SYSTEM OF FIRST-
C     ORDER INITIAL-VALUE PROBLEMS
C              UJ' = FJ(T,U1,U2,...,UM), J=1,2,...,M
C           A <= T <= B, UJ(A)=ALPHAJ, J=1,2,...,M
C     AT (N+1) EQUALLY SPACED NUMBERS IN THE INTERVAL [A,B].
C
C     INPUT ENDPOINTS A,B; NUMBER OF EQUATIONS M; INTIAL
C           CONDITIONS ALPHA1,...,ALPHAM; INTEGER N.
C
C     OUTPUT APPROXIMATIONS WJ TO UJ(T) AT THE (N+1) VALUES OF T.
C
      CHARACTER NAME1*30,AA*1
      INTEGER OUP,FLAG
      LOGICAL OK
C     CHANGE FUNCTIONF F1 AND F2 FOR A NEW PROBLEM
      F1(T,X1,X2) = -4*X1+3*X2+6
      F2(T,X1,X2) = -2.4*X1+1.6*X2+3.6
      OPEN(UNIT=5,FILE='CON',ACCESS='SEQUENTIAL')
      OPEN(UNIT=6,FILE='CON',ACCESS='SEQUENTIAL')
C     DEFINE FUNCTIONS F1,...,FM
      WRITE(6,*) 'This is the Runge-Kutta Method for systems with m=2.'
      WRITE(6,*) 'Have the functions F1 and F2 been defined?'
      WRITE(6,*) 'Enter Y or N '
      WRITE(6,*) ' '
      READ(5,*)  AA
      IF(( AA .EQ. 'Y' ) .OR. ( AA .EQ. 'y' )) THEN
         OK = .FALSE.
10       IF (OK) GOTO 11
            WRITE(6,*) 'Input left and right endpoints separated by'
            WRITE(6,*) 'blank'
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
         WRITE(6,*) 'Input the two initial conditions.'
         WRITE(6,*) ' '
         READ(5,*) ALPHA1, ALPHA2
12       IF (OK) GOTO 13
            WRITE(6,*) 'Input a positive integer for the number'
            WRITE(6,*) 'of subintervals '
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
         WRITE(6,*) 'The program will end so that the functions'
         WRITE(6,*) 'F1 and F2 can be created '
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
      WRITE(OUP,*) 'RUNGE-KUTTA METHOD FOR SYSTEMS'
      WRITE(OUP,6)
6     FORMAT(12X,'t(i)',11X,'w1(i)',11X,'w2(i)')
C     STEP 1
      H=(B-A)/N
      T=A
C     STEP 2
      W1=ALPHA1
      W2=ALPHA2
C     STEP 3
      WRITE(OUP,1) T,W1,W2
C     STEP 4
      DO 110 I=1,N
C        STEP 5
         X11=H*F1(T,W1,W2)
         X12=H*F2(T,W1,W2)
C        STEP 6
         X21=H*F1(T+H/2,W1+X11/2,W2+X12/2)
         X22=H*F2(T+H/2,W1+X11/2,W2+X12/2)
C        STEP 7
         X31=H*F1(T+H/2,W1+X21/2,W2+X22/2)
         X32=H*F2(T+H/2,W1+X21/2,W2+X22/2)
C        STEP 8
         X41=H*F1(T+H,W1+X31,W2+X32)
         X42=H*F2(T+H,W1+X31,W2+X32)
C        STEP 9
         W1=W1+(X11+2*X21+2*X31+X41)/6
         W2=W2+(X12+2*X22+2*X32+X42)/6
C        STEP 10
         T=A+I*H
C        STEP 11
         WRITE(OUP,1) T,W1,W2
110      CONTINUE
C     STEP 12
400   CLOSE(UNIT=5)
      CLOSE(UNIT=OUP)
      IF(OUP.NE.6) CLOSE(UNIT=6)
      STOP
1     FORMAT(3(1X,E15.8))
      END
