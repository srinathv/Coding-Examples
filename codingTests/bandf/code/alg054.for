C***********************************************************************
C                                                                      *
C       ADAMS-FOURTH ORDER PREDICTOR-CORRECTOR ALGORITHM 5.4           *
C                                                                      *
C***********************************************************************
C
C
C
C     TO APPROXIMATE THE SOLUTION OF THE INITIAL VALUE PROLEM:
C               Y'=F(T,Y), A<=T<=B, Y(A)=ALPHA,
C     AT (N+1) EQUALLY SPACED NUMBERS IN THE INTERVAL [A,B].
C
C     INPUT:   ENDPOINTS A,B; INITIAL CONDITION ALPHA; INTEGER N.
C
C     OUTPUT:  APPROXIMATION W TO Y AT THE (N+1) VALUES OF T.
C
C     T(1),...,T(4) AND W(1),...,W(4) ARE THE 4 MOST RECENT VALUES OF
C     T(I) AND W(I) RESP.
C
      DIMENSION T(4),W(4)
      CHARACTER NAME1*30,AA*1
      INTEGER OUP,FLAG
      LOGICAL OK
C     CHANGE FUNCTION F FOR A NEW PROBLEM
      F(TZ,WZ) = WZ-TZ*TZ+1
      OPEN(UNIT=5,FILE='CON',ACCESS='SEQUENTIAL')
      OPEN(UNIT=6,FILE='CON',ACCESS='SEQUENTIAL')
      WRITE(6,*) 'This is the Adams-Bashforth '
      WRITE(6,*) 'Predictor-Corrector Method.'
      WRITE(6,*) 'Has the function F been created in the program? '
      WRITE(6,*) 'NOTE:  The function is defined in two places.'
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
      WRITE(OUP,*) 'ADAMS-BASHFORTH PREDICTOR-CORRECTOR METHOD'
C     STEP 1
C     THE SUBSCRIPTS ARE SHIFTED TO AVOID ZERO SUBSCRIPTS
      T(1) = A
      W(1) = ALPHA
      H = (B-A)/N
      I = 0
      WRITE(OUP,2)
      WRITE(OUP,3) I,T(1),W(1)
C     STEP 2
      DO 110 I=1,3
C          STEPS 3 AND 4
C          COMPUTE STARTING VALUES USING RUNGE-KUTTA METHOD GIVEN IN A
C          SUBROUTINE--NOTE:  FUNCTION F IS NEEDED IN THE SUBROUTINE
           CALL RK4(H,T(I),W(I),T(I+1),W(I+1))
C          STEP 5
110   WRITE(OUP,3) I,T(I+1),W(I+1)
C     STEP 6
      DO 20 I=4,N
C          STEP 7
C          TO, WO WILL BE USED IN PLACE OF T, W RESP.
           TO=A+I*H
C          PREDICT W(I)
           WO = W(4)+H*(55*F(T(4),W(4))-59*F(T(3),W(3))+37*F(T(2),W(2)
     *     )-9*F(T(1),W(1)))/24
C          CORRECT W(I)
           WO = W(4)+H*(9*F(TO,WO)+19*F(T(4),W(4))-5*F(T(3),W(3))+
     *     F(T(2),W(2)))/24
C          STEP 8
           WRITE(OUP,3) I,TO,WO
C          STEP 9
C          PREPARE FOR NEXT ITERATION
           DO 30 J=1,3
                T(J) = T(J+1)
30              W(J) = W(J+1)
C          STEP 10
           T(4) = TO
           W(4) = WO
20    CONTINUE
C     STEP 11
400   CLOSE(UNIT=5)
      CLOSE(UNIT=OUP)
      IF(OUP.NE.6) CLOSE(UNIT=6)
      STOP
2     FORMAT(1X,7X,'I',12X,'T',19X,'W')
3     FORMAT(1X,5X,I3,5X,E15.8,5X,E15.8)
      END
           SUBROUTINE RK4(H,TO,WO,TI,WI)
           F(T,W) = W-T*T+1
           TI = TO+H
           XK1 = H*F(TO,WO)
           XK2 = H*F(TO+H/2,WO+XK1/2)
           XK3 = H*F(TO+H/2,WO+XK2/2)
           XK4 = H*F(TI,WO+XK3)
           WI = WO+(XK1+2*(XK2+XK3)+XK4)/6
           RETURN
           END
