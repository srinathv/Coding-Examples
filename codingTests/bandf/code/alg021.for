C***********************************************************************
C                                                                      *
C                        BISECTION ALGORITHM 2.1                       *
C                                                                      *
C***********************************************************************
C
C
C     TO FIND A SOLUTION TO F(X)=0 GIVEN THE CONTINOUS FUNCTION
C     F ON THE INTERVAL <A,B>, WHERE F(A) AND F(B) HAVE
C     OPPOSITE SIGNS:
C
C     INPUT:   ENDPOINTS A,B; TOLERANCE TOL;
C              MAXIMUM INTERATIONS N0.
C
C     OUTPUT:  APPROXIMATE SOLUTION P OR A
C              MESSAGE THAT THE ALGORITHM FAILS.
C
      CHARACTER NAME1*14,AA*1
      INTEGER OUP,FLAG
      LOGICAL OK
      REAL A,B,FA,FB,X,TOL
      INTEGER N0
C     DEFINE F
      F(X)=(X+4)*X*X-10
      OPEN(UNIT=5,FILE='CON',ACCESS='SEQUENTIAL')
      OPEN(UNIT=6,FILE='CON',ACCESS='SEQUENTIAL')
      WRITE(6,*) 'This is the Bisection Method.'
      WRITE(6,*) 'Has the function F been created in the program? '
      WRITE(6,*) 'Enter Y or N '
      WRITE(6,*) ' '
      READ(5,*)  AA
      IF(( AA .EQ. 'Y' ) .OR. ( AA .EQ. 'y' )) THEN
         OK = .FALSE.
10       IF (OK) GOTO 11
            WRITE(6,*) 'Input endpoints A < B  separated by blank '
            WRITE(6,*) ' '
            READ(5,*) A, B
            IF (A.GT.B) THEN
               X = A
               A = B
               B = X
            ENDIF
            IF (A.EQ.B) THEN
               WRITE(6,*) 'A cannot equal B '
               WRITE(6,*) ' '
            ELSE
               FA = F( A )
               FB = F( B )
               IF ( FA * FB .GT. 0.0 ) THEN
                  WRITE(6,*) 'F(A) and F(B) have same sign '
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
      IF (.NOT.OK) GOTO 40
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
      WRITE(OUP,*) 'BISECTION METHOD'
      IF (FLAG.EQ.2) THEN
         WRITE(OUP,004)
004      FORMAT(3X,'I',15X,'P',12X,'F(P)')
      ENDIF
C     STEP 1
      I=1
C     STEP 2
016   IF (I.GT.N0) GOTO 020
C          STEP 3
C          COMPUTE P(I)
           P=A+(B-A)/2
           FP=F(P)
           IF (FLAG.EQ.2) THEN
              WRITE(OUP,005) I,P,FP
005           FORMAT(1X,I3,2X,E15.8,2X,E15.8)
           ENDIF
C          STEP 4
           IF( ABS(FP).LE.1.0E-20 .OR. (B-A)/2 .LT. TOL) THEN
C          PROCEDURE COMPLETED SUCCESSFULLY
                WRITE(OUP,002) P, I, TOL
                GOTO 040
           ENDIF
C          STEP 5
           I=I+1
C          STEP 6
C          COMPUTE A(I) AND B(I)
           IF( FA*FP .GT. 0) THEN
                A=P
                FA=FP
           ELSE
                B=P
                FB=FP
           ENDIF
      GOTO 016
020   CONTINUE
C     STEP 7
C     PROCEDURE COMPLETED UNSUCCESSFULLY
      IF(OUP.NE.6) WRITE(6,3) N0,P,TOL
      WRITE(OUP,3) N0,P,TOL
040   CLOSE(UNIT=5)
      CLOSE(UNIT=OUP)
      IF (OUP.NE.6) CLOSE(UNIT=6)
      STOP
002     FORMAT(1X,'THE APPROXIMATE SOLUTION IS',/,1X
     *,E15.8,1X,'AFTER',1X,I2,1X,'ITERATIONS, WITH TOLERANCE'
     * ,1X,E15.8)
003   FORMAT(1X,'ITERATION NUMBER',1X,I3,1X,'GAVE APPROXIMATION',
     */,E15.8,1X,'NOT WITHIN TOLERANCE',1X,E15.8)
      END
