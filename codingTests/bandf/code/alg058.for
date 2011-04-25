C***********************************************************************
C                                                                      *
C               TRAPEZOIDAL WITH NEWTON ITERATION ALGORITHM 5.8        *
C                                                                      *
C***********************************************************************
C
C
C
C     TO APPROXIMATE THE SOLUTION OF THE INITIAL-VALUE PROBLEM
C             Y' = F(T,Y), A <= T <= B, Y(A)=ALPHA
C     AT (N+1) EQUALLY SPACED NUMBERS IN THE INTERVAL [A,B]:
C
C     INPUT ENDPOINTS A,B; INITIAL CONDITION ALPHA; INTEGER N;
C           TOLERANCE TOL; MAXIMUM NUMBER OF ITERATIONS M AT ANY ONE STEP.
C
C     OUTPUT APPROXIMATION W TO Y AT THE (N+1) VALUES OF T OR A MESSAGE
C            OF FAILURE.
C
C
      CHARACTER NAME1*30,AA*1
      INTEGER OUP,FLAG
      LOGICAL OK
C     CHANGE FUNCTIONS F AND FYP FOR A NEW PROBLEM
      F(TZ,WZ) = 5*EXP(5*TZ)*(WZ-TZ)**2+1.0
C     FUNCTION FYP IS THE PARTIAL DERIVATIVE OF F WITH RESPECT TO Y
      FYP(TZ,WZ) = 10*EXP(5*TZ)*(WZ-TZ)
      OPEN(UNIT=5,FILE='CON',ACCESS='SEQUENTIAL')
      OPEN(UNIT=6,FILE='CON',ACCESS='SEQUENTIAL')
      WRITE(6,*) 'This is the Implicit Trapezoidal Method.'
      WRITE(6,*) 'Has the functions F and FYP been'
      WRITE(6,*) 'created in the program?'
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
13       OK = .FALSE.
14       IF (OK) GOTO 15
            WRITE(6,*) 'Input tolerance '
            WRITE(6,*) ' '
            READ(5,*) TOL
            IF (TOL.LE.0.0) THEN
               WRITE(6,*) 'Tolerance must be positive '
            ELSE
               OK = .TRUE.
            ENDIF
         GOTO 14
15       OK = .FALSE.
16       IF (OK) GOTO 17
            WRITE(6,*) 'Input maximum number of iterations.'
            WRITE(6,*) ' '
            READ(5,*) M
            IF (M.LE.0) THEN
               WRITE(6,*) 'Number of iterations must be positive '
            ELSE
               OK = .TRUE.
            ENDIF
         GOTO 16
17       CONTINUE
      ELSE
         WRITE(6,*) 'The program will end so that the functions'
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
      WRITE(OUP,*) 'IMPLICIT TRAPEZOIDAL METHOD'
      WRITE(OUP,6)
6     FORMAT(12X,'t(i)',12X,'w(i)')
C     STEP 1
      W=ALPHA
      T=A
      H=(B-A)/N
      WRITE(OUP,1) T,W
C     STEP 2
      DO 110 I=1,N
C        STEP 3
         XK1=W+H/2*F(T,W)
         W0=XK1
         J=1
         IFLAG=0
C        STEP 4
100      IF ( IFLAG.NE.0) GOTO 200
C             STEP 5
              W=W0-(W0-XK1-H/2*F(T+H,W0))/(1-H/2*FYP(T+H,W0))
C             STEP 6
              IF(ABS(W-W0).LT.TOL) THEN
                 IFLAG=1
              ELSE
                 J=J+1
                 W0=W
                 IF(J.GT.M) THEN
                    WRITE(OUP,2)
                    GOTO 400
                 ENDIF
              ENDIF
         GOTO 100
C        STEP 7
200      T=A+I*H
         WRITE(OUP,1) T,W
110   CONTINUE
C     STEP 8
400   CLOSE(UNIT=5)
      CLOSE(UNIT=OUP)
      IF(OUP.NE.6) CLOSE(UNIT=6)
      STOP
1     FORMAT(1X,2(E15.8,1X))
2     FORMAT(1X,'MAXIMUM NUMBER OF ITERATIONS EXCEEDED')
      END
