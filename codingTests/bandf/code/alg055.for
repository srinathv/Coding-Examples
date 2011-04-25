C***********************************************************************
C                                                                      *
C     ADAMS VARIABLE STEP-SIZE PREDICTOR-CORRECTOR ALGORITHM 5.5       *
C                                                                      *
C***********************************************************************
C
C
C
C     TO APPROXIMATE THE SOLUTION OF THE INITIAL VALUE PROBLEM
C                Y'=F(T,Y), A <= T <= B , Y(A)=ALPHA,
C     WITH LOCAL TRUNCATION ERROR WITHIN A GIVEN TOLERANCE:
C
C     INPUT:   ENDPOINTS A,B; INITIAL CONDITION ALPHA; TOLERANCE
C              TOL; MAXIMUM STEPSIZE HMAX; MINIMUM STEPSIZE HMIN.
C
C     OUTPUT:  I,T(I),W(I),H WHERE AT THE ITH STEP W(I) APPROXIMATES
C              Y(T(I)) AND STEPSIZE H WAS USED OR A MESSAGE THAT THE
C              MINIMUM STEP SIZE WAS EXCEEDED.
C
C     STEP 1 SETS UP THE SUBALGORITHM FOR RK4
C     THE FUNCTION F MUST ALSO BE DEFINED IN THE SUBROUTINE
C
      DIMENSION T(100),W(100)
      CHARACTER NAME1*30,AA*1
      INTEGER OUP,FLAG
      LOGICAL OK
C     CHANGE FUNCTION F FOR A NEW PROBLEM
      F(TZ,WZ)= WZ-TZ*TZ+1
      OPEN(UNIT=5,FILE='CON',ACCESS='SEQUENTIAL')
      OPEN(UNIT=6,FILE='CON',ACCESS='SEQUENTIAL')
      WRITE(6,*) 'This is the Adams Variable Step-size'
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
13       WRITE(6,*) 'Input the initial condition.'
         WRITE(6,*) ' '
         READ(5,*) ALPHA
         OK = .FALSE.
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
      WRITE(OUP,*) 'ADAMS VARIABLE PREDICTOR-CORRECTOR METHOD'
      WRITE(OUP,*) 'The fifth column is sigma.  Output only if'
      WRITE(OUP,*) 'approximation is from Adams formulas.'
      WRITE(OUP,6)
6     FORMAT(3X,'I',12X,'t(i)',12X,'w(i)',15x,'h')
C     STEP 2
C     SUBSCRIPTS ARE SHIFTED TO AVOID ZERO SUBSCRIPTS
      T(1)= A
      W(1)= ALPHA
      H= HMAX
C     FLAG WILL BE USED TO EXIT THE LOOP IN STEP 4
      FLAG=1
C     LAST WILL INDICATE WHEN THE LAST VALUE IS CALCULATED.
      LAST=0
      WRITE(OUP,1) T(1),W(1)
C     STEP 3
      KK=1
      CALL RK4(KK,H,W,T)
C     NFLAG INDICATES COMPUTATION FROM RK4
      NFLAG=1
      I=5
C     USE TT IN PLACE OF T
      TT=T(4)+H
C     STEP 4
100   IF (FLAG .NE. 1 ) GOTO 200
C         STEP 5
C         PREDICT W(I)
          WP=W(I-1)+H*(55*F(T(I-1),W(I-1))-59*F(T(I-2),W(I-2))+37*F(T(I-
     1    3),W(I-3))-9*F(T(I-4),W(I-4)))/24
C         CORRECT W(I)
          WC=W(I-1)+H*(9*F(TT,WP)+19*F(T(I-1),W(I-1))-5*F(T(I-2),W(I-2))
     1    +F(T(I-3),W(I-3)))/24
          SIG=19*(ABS(WC-WP))/(270*H)
C         STEP 6
          IF(SIG.LE.TOL) THEN
C             STEP 7
C             RESULT ACCEPTED
              W(I)=WC
              T(I)=TT
C             STEP 8
              IF(NFLAG.EQ.1) THEN
                   K=I-3
                   KK=I-1
C                  PREVIOUS RESULTS ARE ALSO TO BE ACCEPTED
                   DO 20 J=K,KK
                   JJ=J-1
20                 WRITE(OUP,2) JJ,T(J),W(J),H
                   J=I-1
                   WRITE(OUP,3) J,T(I),W(I),H,SIG
              ELSE
                   J=I-1
C                  PREVIOUS RESULTS WERE ALREADY ACCEPTED
                   WRITE(OUP,3) J,T(I),W(I),H,SIG
              END IF
C             STEP 9
              IF (LAST.EQ.1) THEN
                  FLAG=0
              ELSE
C                  STEP 10
                   I=I+1
                   NFLAG=0
C                  STEP 11
                   IF((SIG.LE.0.1*TOL).OR.(T(I-1)+H.GT.B)) THEN
C                  INCREASE H IF IT MORE ACCURATE THAN REQUIRED OR
C                  DECREASE H TO INCLUDE B AS A MESH POINT.
C                  STEP 12
C                  TO AVOID UNDERFLOW
                        IF (SIG .LE. 1.0E-20) THEN
                             Q=4
                        ELSE
                             Q=(TOL/(2*SIG))**.25
                        END IF
C                       STEP 13
                        IF(Q.GT.4) THEN
                             H=4*H
                        ELSE
                             H=Q*H
                        END IF
C                       STEP 14
                        IF(H.GT.HMAX) H=HMAX
C                       STEP 15
C                  AVOID TERMINATING WITH CHANGE IN STEPSIZE
                        IF(T(I-1)+4*H.GT.B) THEN
                            H=(B-T(I-1))/4
                            LAST=1
                        ENDIF
C                       STEP 16
                        NFLAG=1
                        KK=I-1
                        CALL RK4(KK,H,W,T)
                        I=I+3
                   END IF
              END IF
          ELSE
C             FALSE BRANCH FOR STEP 6
C             RESULT REJECTED
C             STEP 17
              Q=(TOL/(2*SIG))**.25
C             STEP 18
              IF(Q.LT.0.1) THEN
                   H=0.1*H
              ELSE
                   H=Q*H
              END IF
C             STEP 19
              IF(H.LT.HMIN) THEN
C                  PROCEDURE FAILS
                   WRITE(OUP,4)
                   FLAG=0
              ELSE
C             PREVIOUS RESULTS ALSO REJECTED
                   IF(T(I-1)+4*H.GT.B) H=0.25*(B-T(I-1))
                   IF(NFLAG.EQ.1) I=I-3
                   KK=I-1
                   CALL RK4(KK,H,W,T)
                   I=I+3
                   NFLAG=1
              ENDIF
          END IF
C     STEP 20
      TT=T(I-1)+H
      GOTO 100
C     STEP 21
200   CONTINUE
400   CLOSE(UNIT=5)
      CLOSE(UNIT=OUP)
      IF(OUP.NE.6) CLOSE(UNIT=6)
      STOP
1     FORMAT(5X,E15.8,1X,E15.8)
2     FORMAT(1X,I3,1X,E15.8,1X,E15.8,1X,E15.8)
3     FORMAT(1X,I3,1X,E15.8,1X,E15.8,1X,E15.8,1X,E15.8)
4     FORMAT(1X,'HMIN EXCEEDED - PROCEDURE FAILS')
      END
      SUBROUTINE RK4(K,H,V,X)
      DIMENSION V(100),X(100)
      F(XZ,VZ)=VZ-XZ*XZ+1
      DO 20 I=1,3
      J=K+I-1
      XK1=H*F(X(J),V(J))
      XK2=H*F(X(J)+H/2,V(J)+XK1/2)
      XK3=H*F(X(J)+H/2,V(J)+XK2/2)
      XK4=H*F(X(J)+H,V(J)+XK3)
      V(J+1)=V(J)+(XK1+2*(XK2+XK3)+XK4)/6
20    X(J+1)=X(J)+H
      RETURN
      END
