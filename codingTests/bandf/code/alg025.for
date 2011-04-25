C***********************************************************************
C                                                                      *
C                METHOD OF FALSE POSITION ALGORITHM 2.5                *
C                                                                      *
C***********************************************************************
C
C
C
C     TO FIND A SOLUTION TO THE EQUATION F(X)=0
C     GIVEN THE CONTINUOUS FUNCTION F ON THE INTERVAL [P0,P1]
C     WHERE F(P0) AND F(P1) HAVE OPPOSITE SIGNS:
C
C     INPUT:   ENDPOINTS P0,P1; TOLERANCE TOL;
C              MAXIMUM NUMBER OF ITERATIONS N0.
C
C     OUTPUT:  APPROXIMATE SOLUTION P OR MESSAGE THAT THE
C              ALGORITHM FAILS.
C
      REAL Q,P0,Q0,P1,Q1,P,X,TOL
      INTEGER I,N0,FLAG,OUP
      LOGICAL OK
      CHARACTER NAME1*30,AA*1
C     DEFINE FUNCTION F
      F(XX)=COS(XX)-XX
      OPEN(UNIT=5,FILE='CON',ACCESS='SEQUENTIAL')
      OPEN(UNIT=6,FILE='CON',ACCESS='SEQUENTIAL')
      WRITE(6,*) 'This is the Method of False Position.'
      WRITE(6,*) 'Has the function F been created in the program? '
      WRITE(6,*) 'Enter Y or N '
      WRITE(6,*) ' '
      READ(5,*)  AA
      IF(( AA .EQ. 'Y' ) .OR. ( AA .EQ. 'y' )) THEN
         OK = .FALSE.
10       IF (OK) GOTO 11
            WRITE(6,*) 'Input endpoints P0 < P1  separated by blank '
            WRITE(6,*) ' '
            READ(5,*) P0, P1
            IF (P0.GT.P1) THEN
               X = P0
               P0 = P1
               P1 = X
            ENDIF
            IF (P0.EQ.P1) THEN
               WRITE(6,*) 'P0 cannot equal P1 '
               WRITE(6,*) ' '
            ELSE
               Q0 = F( P0 )
               Q1 = F( P1 )
               IF ( Q0 * Q1 .GT. 0.0 ) THEN
                  WRITE(6,*) 'F(P0) and F(P1) have same sign '
                  WRITE(6,*) ' '
               ELSE
                  OK = .TRUE.
               ENDIF
            ENDIF
         GOTO 10
11       OK = .FALSE.
12       IF (OK) GOTO 13
            WRITE(6,*) 'Input tolerance '
            WRITE(6,*) ' '
            READ(5,*) TOL
            IF (TOL.LE.0.0) THEN
               WRITE(6,*) 'Tolerance must be positive '
               WRITE(6,*) ' '
            ELSE
               OK = .TRUE.
            ENDIF
         GOTO 12
13       OK = .FALSE.
14       IF (OK) GOTO 15
            WRITE(6,*) 'Input maximum number of iterations '
            WRITE(6,*) '- no decimal point '
            WRITE(6,*) ' '
            READ(5,*) N0
            IF ( N0 .LE. 0 ) THEN
              WRITE(6,*) 'Must be positive integer '
              WRITE(6,*) ' '
            ELSE
              OK = .TRUE.
            ENDIF
         GOTO 14
15       CONTINUE
      ELSE
         WRITE(6,*) 'The program will end so that the function F '
         WRITE(6,*) 'can be created '
         OK = .FALSE.
      ENDIF
      IF (.NOT.OK) GOTO 040
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
      WRITE(6,*) 'Select amount of output '
      WRITE(6,*) '1. Answer only '
      WRITE(6,*) '2. All intermediate approximations '
      WRITE(6,*) 'Enter 1 or 2 '
      WRITE(6,*) ' '
      READ(5,*) FLAG
      WRITE(OUP,*) 'METHOD OF FALSE POSITION'
      IF (FLAG.EQ.2) THEN
         WRITE(OUP,4)
4        FORMAT(3X,'I',16X,'P',13X,'F(P)')
      ENDIF
C     STEP 1
      I=2
      Q0=F(P0)
      Q1=F(P1)
C     STEP 2
016   IF ( I .GT. N0 ) GOTO 020
C          STEP 3
C          COMPUTE P(I)
           P=P1-Q1*(P1-P0)/(Q1-Q0)
           Q=F(P)
           IF (FLAG.EQ.2) THEN
              WRITE(OUP,5) I,P,Q
5             FORMAT(1X,I3,2X,E15.8,2X,E15.8)
           ENDIF
C          STEP 4
           IF( ABS(P-P1) .LT. TOL ) THEN
                WRITE(OUP,2) P,I,TOL
                GOTO 040
           END IF
C          STEP 5
           I=I+1
C          STEP 6
C          UPDATE P0, Q0, P1, Q1
           IF (Q * Q1 .LT. 0.0) THEN
              P0=P1
              Q0=Q1
           ENDIF
C     STEP 7
           P1=P
           Q1=Q
      GOTO 016
020   CONTINUE
C     STEP 8
C     PROCEDURE COMPLETED UNSUCCESSFULLY
      IF(OUP.NE.6) WRITE(6,3) N0,P,TOL
      WRITE(OUP,3) N0,P,TOL
040   CLOSE(UNIT=5)
      CLOSE(UNIT=OUP)
      IF(OUP.NE.6) CLOSE(UNIT=6)
      STOP
2     FORMAT(1X,'THE APPROXIMATE SOLUTION IS',1X,E15.8,1X,
     *'AFTER',1X,I2,1X,'ITERATIONS',/,' WITH TOLERANCE',1X,E15.8)
3     FORMAT(1X,'ITERATION NUMBER',1X,I3,1X,'GAVE APPROXIMATION',
     */,E15.8,1X,'NOT WITHIN TOLERANCE',1X,E15.8)
      END
