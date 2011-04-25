C***********************************************************************
C                                                                      *
C                 NEWTON-RAPHSON ALGORITHM 2.3                         *
C                                                                      *
C***********************************************************************
C
C
C
C     TO FIND A SOLUTION TO F(X)=0 GIVEN AN
C     INITIAL APPROXIMATION PO:
C
C     INPUT:   INITIAL APPROXIMATION PO;  TOLERANCE TOL;
C              MAXIMUM NUMBER OF ITERATIONS N0.
C
C     OUTPUT:  APPROXIMATE SOLUTION P OR A MESSAGE
C              THAT THE ALGORITHM FAILS.
C
      REAL TOL,P0,D,F0,FP0
      INTEGER I,N0,FLAG,OUP
      CHARACTER NAME1*30,AA*1
      LOGICAL OK
C     DEFINE FUNCTIONS F AND F' (DENOTED FP)
      F(X)=COS(X)-X
      FP(X)=-SIN(X)-1
      OPEN(UNIT=5,FILE='CON',ACCESS='SEQUENTIAL')
      OPEN(UNIT=6,FILE='CON',ACCESS='SEQUENTIAL')
      WRITE(6,*) 'This is Newtons Method.'
      WRITE(6,*) 'Have the functions F and F'' been'
      WRITE(6,*) 'created in the program? '
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
         WRITE(6,*) 'The program will end so that the functions F '
         WRITE(6,*) 'and F'' can be created '
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
      WRITE(OUP,*) 'NEWTONS METHOD'
      IF (FLAG.EQ.2) THEN
         WRITE(OUP,004)
004      FORMAT(3X,'I',16X,'P',13X,'F(P)')
      ENDIF
C     STEP 1
      I=1
      F0=F(P0)
C     STEP 2
010   IF ( I .GT. N0 ) GOTO 020
C          STEP 3
C          COMPUTE P(I)
           FP0=FP(P0)
           D=F0/FP0
C          STEP 6
           P0=P0-D
           F0=F(P0)
           IF (FLAG.EQ.2) THEN
              WRITE(OUP,005) I,P0,F0
005           FORMAT(1X,I3,2X,E15.8,2X,E15.8)
           ENDIF
C          STEP 4
           IF( ABS(D) .LT. TOL ) THEN
C          PROCEDURE COMPLETED SUCCESSFULLY
                WRITE(6,2) P0,I,TOL
                GOTO 040
           END IF
C          STEP 5
           I=I+1
      GOTO 010
020   CONTINUE
C     STEP 7
C     PROCEDURE COMPLETED UNSUCCESSFULLY
      IF(OUP.NE.6) WRITE(6,3) N0,P0,TOL
      WRITE(OUP,3) N0,P0,TOL
040   CLOSE(UNIT=5)
      CLOSE(UNIT=OUP)
      IF(OUP.NE.6) CLOSE(UNIT=6)
      STOP
2     FORMAT(1X,'APPROXIMATE SOLUTION IS',1X,E15.8,1X,'AFTER',
     *1X,I2,1X,'ITERATIONS,',/,' WITH TOLERANCE',1X,E15.8)
3     FORMAT(1X,'ITERATION NUMBER',1X,I3,1X,'GAVE APPROXIMATION',
     */,E15.8,1X,'NOT WITHIN TOLERANCE',1X,E15.8)
      END
