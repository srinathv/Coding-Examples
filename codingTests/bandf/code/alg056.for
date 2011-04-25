C***********************************************************************
C                                                                      *
C                     EXTRAPOLATION ALGORITHM 5.6                      *
C                                                                      *
C***********************************************************************
C
C
C
C     TO APPROXIMTE THE SOLUTION OF THE INITIAL VALUE PROBLEM:
C                  Y'=F(T,Y), A<=T<=B, Y(A)=ALPHA,
C     WITH LOCAL TRUNCATION ERROR WITHIN A GIVEN TOLERANCE:
C
C     INPUT:   ENDPOINTS A,B; INITIAL CONDITION ALPHA;
C              TOLERANCE TOL; MAXIMUM STEPSIZE HMAX;
C              MINIMUM STEPSIZE HIMIN.
C
C     OUTPUT:  T,W,H WHERE W APPROXIMATES Y(T) AND STEPSIZE H WAS
C              USED OR A MESSAGE THAT MINIMUM STEPSIZE EXCEEDED.
C
      DIMENSION NK(8),Y(8),Q(7,7)
      CHARACTER NAME1*30,AA*1
      INTEGER OUP,FLAG
      LOGICAL OK
C     CHANGE FUNCTION F FOR A NEW PROBLEM
      F(TZ,WZ)= WZ-TZ*TZ+1
C     STEP 1
      DATA NK/2,4,6,8,12,16,24,32/
      OPEN(UNIT=5,FILE='CON',ACCESS='SEQUENTIAL')
      OPEN(UNIT=6,FILE='CON',ACCESS='SEQUENTIAL')
      WRITE(6,*) 'This is the Gragg Extrapolation Method.'
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
      IF(.NOT.OK) GOTO 700
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
      WRITE(OUP,*) 'GRAGG EXTRAPOLATION METHOD'
      WRITE(OUP,6)
6     FORMAT(12X,'t(i)',12X,'w(i)',15X,'h',2X,'k')
C     STEP 1 - SEE DATA STATEMENTS ARE BEGINNING OF PROGRAM
C     STEP 2
      TO=A
      WO=ALPHA
      H=HMAX
C     IFLAG IS USED TO EXIT THE LOOP IN STEP 4
      IFLAG=1
C     STEP 3
      DO 110 I=1,7
      DO 110 J=1,I
C     ( Q(I,J) = H(J)**2/H(I+1)**2 )
110   Q(I,J)=(FLOAT(NK(I+1))/FLOAT(NK(J)))**2
C     STEP 4
100   IF (IFLAG.NE.1) GOTO 200
C          STEP 5
           K=1
C          WHEN DESIRED ACCURACY ACHIEVED, NFLAG IS SET TO 1
           NFLAG=0
C          STEP 6
300        IF (K.GT.8 .OR. NFLAG.NE.0) GOTO 400
C               STEP 7
                HK=H/NK(K)
                T=TO
                W2=WO
C               EULER FIRST STEP
                W3=W2+HK*F(T,W2)
                T=TO+HK
C               STEP 8
                M=NK(K)-1
                DO 20 J=1,M
                     W1=W2
                     W2=W3
C                    MIDPOINT METHOD
                     W3=W1+2*HK*F(T,W2)
20              T=TO+(J+1)*HK
C               STEP 9
C               ENDPOINT CORRECTION TO COMPUTE Y(K,1)
                Y(K)=(W3+W2+HK*F(T,W3))/2
C               STEP 10
C     NOTE: Y(K-1)=Y(K-1,1), Y(K-2)=Y(K-1,2),..., Y(1)=Y(K-1,K-1) SINCE
C        ONLY PREVIOUS ROW OF THE TABLE IS SAVED
                IF(K.GE.2) THEN
C                    STEP 11
                     J=K
C                    SAVE Y(K-1,K-1)
                     V=Y(1)
C                    STEP 12
500                  IF (J.LT.2) GOTO 600
C                    EXTRAPOLATION TO COMPUTE Y(J-1)=Y(K,K-J+2)
                          Y(J-1)=Y(J)+(Y(J)-Y(J-1))/(Q(K-1,J-1)-1)
                          J=J-1
                     GOTO 500
C                    STEP 13
600                  IF(ABS(Y(1)-V).LE.TOL) NFLAG=1
C                    Y(1) ACCEPTED AS NEW W
                END IF
C               STEP 14
                K=K+1
           GOTO 300
C          STEP 15
400         K=K-1
C          STEP 16
           IF(NFLAG.EQ.0) THEN
C               STEP 17
C               NEW VALUE FOR W REJECTED, DECREASE H
                H=H/2
C               STEP 18
                IF(H.LT.HMIN) THEN
                     WRITE(OUP,1)
                     IFLAG=0
                END IF
           ELSE
C               STEP 19
C               NEW VALUE FOR W ACCEPTED
                WO=Y(1)
                TO=TO+H
C               STEP 20
C               INCREASE H IF POSSIBLE
                WRITE(OUP,2) TO,WO,H,K
                IF(TO.GE.B)THEN
                    IFLAG=0
                ELSE
                    IF(TO+H.GT.B)THEN
                        H=B-TO
                    ELSE
                        IF(K.LE.3.AND.H.LT.HMAX/2) THEN
                            H=2*H
                        ENDIF
                    ENDIF
                ENDIF
           END IF
      GOTO 100
C     STEP 21
200   CONTINUE
700   CLOSE(UNIT=5)
      CLOSE(UNIT=OUP)
      IF(OUP.NE.6) CLOSE(UNIT=6)
      STOP
1     FORMAT(1X,'FAILURE')
2     FORMAT(1X,E15.8,1X,E15.8,1X,E15.8,1X,I3)
      END
