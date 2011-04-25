C***********************************************************************
C                                                                      *
C                  STEFFENSEN'S ALGORITHM 2.6                          *
C                                                                      *
C***********************************************************************
C
C
C
C     TO FIND A SOLUTION TO G(X) = X
C     GIVEN AN INITIAL APPROXIMATION P0:
C
C     INPUT:   INITIAL APPROXIMATION P0, TOLERANCE TOL,
C              MAXIMUM NUMBER OF ITERATIONS N0.
C
C     OUTPUT:  APPROXIMATE SOLUTION P OR MESSAGE THAT
C              THE METHOD FAILS.
C
      REAL TOL,P0,P1,P2,P,D
      INTEGER I,N0,FLAG,OUP
      LOGICAL OK
      CHARACTER NAME1*30,AA*1
C     DEFINE FUNCTION G
      G(XX)=SQRT(10/(XX+4))
      OPEN(UNIT=5,FILE='CON',ACCESS='SEQUENTIAL')
      OPEN(UNIT=6,FILE='CON',ACCESS='SEQUENTIAL')
      ZERO = 1.0E-07
      WRITE(6,*) 'This is the Steffensen Method.'
      WRITE(6,*) 'Has the function F been created in the program? '
      WRITE(6,*) 'Enter Y or N '
      WRITE(6,*) ' '
      READ(5,*)  AA
      IF(( AA .EQ. 'Y' ) .OR. ( AA .EQ. 'y' )) THEN
         WRITE(6,*) 'Input initial approximation '
         WRITE(6,*) ' '
         READ(5,*) P0
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
            IF (TOL.LE.ZERO) THEN
               WRITE(6,*) 'WARNING: THIS ACCURACY MAY NOT BE POSSIBLE'
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
      WRITE(OUP,*) 'STEFFENSEN METHOD'
      IF (FLAG.EQ.2) THEN
         WRITE(OUP,4)
4        FORMAT(3X,'I',16X,'P')
      ENDIF
C     STEP 1
      I = 1
C     STEP 2
010   IF ( I .GT . N0 ) GOTO 020
C          STEP 3
C          COMPUTE P(1) WITH SUPERSCRIPT (I-1)
           P1 = G(P0)
C          COMPUTE P(2) WITH SUPERSCRIPT (I-1)
           P2 = G(P1)
           IF (ABS(P2-2*P1+P0).LT.ZERO) THEN
              WRITE(OUP,6)
              WRITE(OUP,7) I-1,P2
6             FORMAT(1X,'DENOMINATOR = 0, METHOD FAILS')
7             FORMAT(1X,'BEST POSSIBLE IS P2(',I3,')= ',E15.8)
              GOTO 040
           ELSE
              D = (P1-P0)*(P1-P0)/(P2-2*P1+P0)
           ENDIF
C          COMPUTE P(0) WITH SUPERSCRIPT (I)
           P = P0-D
           IF (FLAG.EQ.2) THEN
              WRITE(OUP,5) I,P
5             FORMAT(1X,I3,2X,E15.8)
           ENDIF
C          STEP 4
           IF( ABS(D) .LT. TOL ) THEN
C          PROCEDURE COMPLETED SUCCESSFULLY
                WRITE(OUP,2) P,I,TOL
                GOTO 040
           END IF
C          STEP 5
           I = I+1
C          STEP 6
C          UPDATE P0
           P0=P
      GOTO 010
C     STEP 7
C     PROCEDURE COMPLETED UNSUCCESSFULLY
020   IF(OUP.NE.6) WRITE(6,3) N0,P,TOL
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