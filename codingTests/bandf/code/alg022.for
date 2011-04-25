C***********************************************************************
C                                                                      *
C               FIXED-POINT ALGORITHM 2.2                              *
C                                                                      *
C***********************************************************************
C
C
C
C     TO FIND A SOLUTION TO P=G(P) GIVEN AN
C     INITIAL APPROXIMATION PO:
C
C     INPUT:   INITIAL APPROXIMATION PO; TOLERANCE TOL;
C              MAXIMUM NUMBER OF ITERATIONS N0.
C
C     OUTPUT:  APPROXIMATE SOLUTION P OR MESSAGE THAT
C              THE METHOD FAILS.
C
      REAL TOL,P0,P
      INTEGER I,N0,FLAG
      CHARACTER NAME1*30,AA*1
      INTEGER OUP
      LOGICAL OK
C     DEFINE FUNCTION G
      G(X)=SQRT(10/(4+X))
      OPEN(UNIT=5,FILE='CON',ACCESS='SEQUENTIAL')
      OPEN(UNIT=6,FILE='CON',ACCESS='SEQUENTIAL')
      WRITE(6,*) 'This is the Fixed-Point Method.'
      WRITE(6,*) 'Has the function G been created in the program? '
      WRITE(6,*) 'Enter Y or N '
      WRITE(6,*) ' '
      READ(5,*)  AA
      IF(( AA .EQ. 'Y' ) .OR. ( AA .EQ. 'y' )) THEN
         WRITE(6,*) 'Input initial approximation '
         WRITE(6,*) ' '
         READ(5,*) P0
         OK = .FALSE.
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
      WRITE(OUP,*) 'FIXED-POINT METHOD'
      IF (FLAG.EQ.2) THEN
         WRITE(OUP,004)
004      FORMAT(3X,'I',16X,'P')
      ENDIF
C     STEP 1
      I=1
C     STEP 2
010   IF ( I .GT. N0 ) GOTO 020
C          STEP 3
C          COMPUTE P(I)
           P=G(P0)
           IF (FLAG.EQ.2) THEN
              WRITE(OUP,006) I,P
006           FORMAT(1X,I3,2X,E15.8)
           ENDIF
C          STEP 4
           IF( ABS(P-P0) .LT. TOL ) THEN
C          PROCEDURE COMPLETED SUCCESSFULLY
               WRITE(OUP,2) P, I, TOL
               GOTO 040
           END IF
C          STEP 5
           I=I+1
C          STEP 6
C          UPDATE P0
           P0=P
      GOTO 010
020   CONTINUE
C     STEP 7
C     PROCEDURE COMPLETED UNSUCCESSFULLY
      IF(OUP.NE.6) WRITE(6,3) N0,P,TOL
      WRITE(OUP,3) N0,P,TOL
040   CLOSE(UNIT=5)
      CLOSE(UNIT=OUP)
      IF(OUP.NE.6) CLOSE(UNIT=6)
      STOP
2     FORMAT(1X,'APPROXIMATE SOLUTION IS',1X,E15.8,1X
     *,'AFTER',1X,I2,1X,'ITERATIONS,',/,' WITH TOLERANCE',1X,E15.8)
3     FORMAT(1X,'ITERATION NUMBER',1X,I3,1X,'GAVE APPROXIMATION',
     */,E15.8,1X,'NOT WITHIN TOLERANCE',1X,E15.8)
      END
