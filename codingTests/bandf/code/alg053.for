C***********************************************************************
C                                                                      *
C               RUNGE-KUTTA-FEHLBERG ALGORITHM 5.3                     *
C                                                                      *
C***********************************************************************
C
C
C
C     TO APPROXIMATE THE SOLUTION OF THE INITIAL VALUE PROBLEM
C                 Y'=F(T,Y), A<=T<=B, Y(A)=ALPHA,
C     WITH LOCAL TRUNCATION ERROR WITHIN A GIVEN TOLERANCE.
C
C     INPUT:   ENDPOINTS A,B; INITIAL CONDITION ALPHA; TOLERANCE
C              TOL; MAXIMUM STEPSIZE HMAX; MINIMUM STEPSIZE HMIN.
C
C     OUTPUT:  T,W,H WHERE W APPROXIMATES Y(T) AND STEPSIZE H
C              WAS USED OR A MESSAGE THAT MINIMUM STEPSIZE WAS EXCEEDED.
C
C     INITIALIZATION AND INPUT
      CHARACTER NAME1*30,AA*1
      INTEGER OUP,FLAG
      LOGICAL OK
C     CHANGE FUNCTION F FOR A NEW PROBLEM
      F(TZ,WZ)=WZ-TZ*TZ+1
      OPEN(UNIT=5,FILE='CON',ACCESS='SEQUENTIAL')
      OPEN(UNIT=6,FILE='CON',ACCESS='SEQUENTIAL')
      WRITE(6,*) 'This is the Runge-Kutta-Fehlberg Method.'
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
13       WRITE(6,*) 'Input the initial condition.'
         WRITE(6,*) ' '
         READ(5,*) ALPHA
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
            WRITE(6,*) 'Input minimum and maximum mesh spacing'
            WRITE(6,*) 'separated by blank.'
            WRITE(6,*) ' '
            READ(5,*) HMIN, HMAX
            IF ((HMIN.GE.HMAX) .OR. (HMIN.LE.0.0)) THEN
               WRITE(6,*) 'Minimum mesh spacing must be positive '
               WRITE(6,*) 'and less than maximum mesh spacing.'
            ELSE
               OK = .TRUE.
            ENDIF
         GOTO 16
17       CONTINUE
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
      WRITE(OUP,*) 'RUNGE-KUTTA-FEHLBERG METHOD'
      WRITE(OUP,2)
C     STEP 1
      H=HMAX
      W=ALPHA
      T=A
      WRITE(OUP,3) T,W
      IFLAG = 1
      IFLAG1=0
C     STEP 2
100   IF ((T.GE.B) .OR. (IFLAG.NE.1)) GOTO 200
C          STEP 3
           XK1=H*F(T,W)
           XK2=H*F(T+H/4,W+XK1/4)
           XK3=H*F(T+3*H/8,W+(3*XK1+9*XK2)/32)
           XK4=H*F(T+12*H/13,W+(1932*XK1-7200*XK2+7296*XK3)/2197)
           XK5=H*F(T+H,W+439*XK1/216-8*XK2+3680*XK3/513-845*XK4/4104)
           XK6=H*F(T+H/2,W-8*XK1/27+2*XK2-3544*XK3/2565+1859*XK4/4104
     *     -11*XK5/40)
C          STEP 4
           R=ABS(XK1/360-128*XK3/4275-2197*XK4/75240.0+XK5/50+2*XK6/55)
     *       /H
C          STEP 5
           IF(R.LE.TOL) THEN
C          STEP 6
C          APPROXIMATION ACCEPTED
                T = T + H
                W=W+25*XK1/216+1408*XK3/2565+2197*XK4/4104-XK5/5
C          STEP 7
                WRITE(OUP,3) T,W,H,R
                IF(IFLAG1.EQ.1) IFLAG=0
           END IF
C          STEP 8
C          TO AVOID UNDERFLOW
           IF(R.GT.1.0E-20) THEN
                DELTA=.84*(TOL/R)**.25
           ELSE
                DELTA=10.0
           END IF
C          STEP 9
C          CALCULATE NEW H
           IF(DELTA.LE..1) THEN
                H = .1*H
           ELSE
                IF(DELTA.GE.4.) THEN
                     H = 4*H
                ELSE
                      H = DELTA*H
                END IF
           END IF
C          STEP 10
           IF(H.GT.HMAX) H=HMAX
C          STEP 11
           IF(H.LT.HMIN) THEN
                IFLAG=0
                WRITE(OUP,4)
           ELSE
                IF (T+H.GT.B) THEN
                     IF (ABS(B-T).LT.TOL) THEN
                          T=B
                     ELSE
                          H=B-T
                     ENDIF
                     IFLAG1=1
                ENDIF
           END IF
      GOTO 100
C     STEP 12
C     THE PROCESS IS COMPLETE.
200   CONTINUE
400   CLOSE(UNIT=5)
      CLOSE(UNIT=OUP)
      IF(OUP.NE.6) CLOSE(UNIT=6)
      STOP
2     FORMAT(1X,'ORDER OF OUTPUT IS: T,W(I),H,R',/)
3     FORMAT(1X,4(E15.8,1X))
4     FORMAT(1X,'MINIMAL H EXCEEDED')
      END
