C***********************************************************************
C                                                                      *
C   GAUSSIAN ELIMINATION WITH SCALED PARTIAL PIVOTING ALGORITHM 6.3    *
C                                                                      *
C***********************************************************************
C
C
C
C     TO SOLVE AN N BY N LINEAR SYSTEM
C
C
C   E1:  A(1,1) X(1) + A(1,2) X(2) + ... + A(1,N) X(N) = A(1,N+1)
C   E2:  A(2,1) X(1) + A(2,2) X(2) + ... + A(2,N) X(N) = A(2,N+1)
C   :
C   .
C   EN:  A(N,1) X(1) + A(N,2) X(2) + ... + A(N,N) X(N) = A(N,N+1)
C
C     INPUT:   NUMBER OF UNKNOWNS AND EQUATIONS N; AUGMENTED MATRIX
C              A=(A(I,J)) WHERE 1<=I<=N AND 1<=J<=N+1.
C
C     OUTPUT:  SOLUTION X(1),X(2),...X(N) OR A MESSAGE THAT LINEAR
C              SYSTEM HAS NO UNIQUE SOLUTION.
C
C     INITIALIZATION
      DIMENSION A(10,11),NROW(10),S(10),X(10)
      CHARACTER NAME*30,NAME1*30,AA*1
      INTEGER INP,OUP,FLAG
      LOGICAL OK
      OPEN(UNIT=5,FILE='CON',ACCESS='SEQUENTIAL')
      OPEN(UNIT=6,FILE='CON',ACCESS='SEQUENTIAL')
      WRITE(6,*) 'This is Gaussian Elimination with Scaled'
      WRITE(6,*) 'Partial Pivoting'
      WRITE(6,*) 'The array will be input from a text file in the'
      WRITE(6,*) ' order: A(1,1), A(1,2), ..., A(1,N+1), A(2,1),'
      WRITE(6,*) ' A(2,2), ..., A(2,N+1)..., A(N,1), A(N,2),'
      WRITE(6,*) ' ..., A(N,N+1) '
      WRITE(6,*) 'Place as many entries as desired on each line,'
      WRITE(6,*) ' but separate entries with at least one blank.'
      OK = .FALSE.
      WRITE(6,*) 'Has the input file been created?'
      WRITE(6,*) 'Enter Y or N - letter within quotes '
      WRITE(6,*) ' '
      READ(5,*)  AA
      IF (( AA .EQ. 'Y' ) .OR.( AA .EQ. 'y' )) THEN
         WRITE(6,*) 'Input the file name in the form - '
         WRITE(6,*) 'drive:name.ext  contained in quotes'
         WRITE(6,*) 'as example:   ''A:DATA.DTA'' '
         WRITE(6,*) ' '
         READ(5,*)  NAME
         INP = 4
         OPEN(UNIT=INP,FILE=NAME,ACCESS='SEQUENTIAL')
         OK = .FALSE.
19       IF (OK) GOTO 11
         WRITE(6,*) 'Input the number of equations - an integer '
         WRITE(6,*)
         READ(5,*) N
         IF (N .GT. 0) THEN
            M = N+1
            READ(INP,*) ((A(I,J), J=1,M),I=1,N)
            OK = .TRUE.
            CLOSE(UNIT=INP)
         ELSE
            WRITE(6,*) 'The number must be a positive integer'
         ENDIF
         GOTO 19
      ELSE
         WRITE(6,*) 'The program will end so the input file can '
         WRITE(6,*) 'be created. '
         OK = .FALSE.
      ENDIF
11    IF(.NOT. OK) GOTO 400
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
      WRITE(OUP,*) 'GAUSSIAN ELIMINATION WITH SCALED PARTIAL PIVOTING'
C     ICHG COUNTS NUMBER OF INTERCHANGES
      ICHG=0
      WRITE(OUP,3)
      WRITE(OUP,4) ((A(I,J),J=1,M),I=1,N)
C     STEP 1
      DO 10 I=1,N
C     INITIALIZE ROW POINTER
10    NROW(I)=I
      DO 20 I=1,N
           S(I)=ABS(A(I,1))
           DO 30 J=1,N
                IF(ABS(A(I,J)).GT.S(I)) S(I)=ABS(A(I,J))
30         CONTINUE
           IF(S(I).LT.1.0E-20) THEN
C               SYSTEM HAS NO UNIQUE SOLUTION
                WRITE(OUP,5)
                GOTO 400
           END IF
20    CONTINUE
C     STEP 2
C     ELIMINATION PROCESS
      NN = N-1
      DO 90 I=1,NN
C          STEP 3
           IMAX=NROW(I)
           AMAX=ABS(A(IMAX,I))/S(IMAX)
           IMAX=I
           JJ=I+1
           DO 40 IP=JJ,N
                JP=NROW(IP)
                TEMP=ABS(A(JP,I)/S(JP))
                IF(TEMP.GT.AMAX) THEN
                     AMAX=TEMP
                     IMAX=IP
                END IF
40         CONTINUE
C          STEP 4
           IF(AMAX.LT.1.0E-20) THEN
C               SYSTEM HAS NO UNIQUE SOLUTION
                WRITE(OUP,5)
                GOTO 400
           END IF
C          STEP 5
C          SIMULATE ROW INTERCHANGE
           IF(NROW(I).NE.NROW(IMAX)) THEN
                ICHG = ICHG+1
                NCOPY=NROW(I)
                NROW(I)=NROW(IMAX)
                NROW(IMAX)=NCOPY
           END IF
C          STEP 6
           I1=NROW(I)
           DO 50 J=JJ,N
                J1=NROW(J)
C               STEP 7
                XM=A(J1,I)/A(I1,I)
C               STEP 8
                DO 60 K=JJ,M
60              A(J1,K)=A(J1,K)-XM*A(I1,K)
C               MULTIPLIER XM COULD BE SAVED IN A(J1,I)
50         A(J1,I)=0.0
90    CONTINUE
C     STEP 9
      N1=NROW(N)
      IF(ABS(A(N1,N)).LT.1.0E-20) THEN
C          SYSTEM HAS NO UNIQUE SOLUTION
           WRITE(OUP,5)
           GOTO 400
      END IF
C     STEP 10
C     START BACKWARD SUBSTITUTION
C     STORE SOLUTION IN (N+1)ST COLUMN OF A
      X(N)=A(N1,N+1)/A(N1,N)
C     STEP 11
      DO 70 K=1,NN
           I=NN-K+1
           JJ=I+1
           N2=NROW(I)
           SUM=0.0
           DO 80 KK=JJ,N
80         SUM=SUM-A(N2,KK)*X(KK)
70    X(I)=(A(N2,N+1)+SUM)/A(N2,I)
C     STEP 12
C     PROCEDURE COMPLETED SUCCESSFULLY
      WRITE(OUP,6)
      WRITE(OUP,4) ((A(I,J),J=1,N),I=1,M)
      WRITE(OUP,7) (X(I),I=1,N)
      WRITE(OUP,8) ICHG
      WRITE(OUP,9) (NROW(I),I=1,N)
400   CLOSE(UNIT=5)
      CLOSE(UNIT=OUP)
      IF(OUP.NE.6) CLOSE(UNIT=6)
      STOP
5     FORMAT(1X,'THE PRECEDING SYSTEM HAS NO UNIQUE SOLUTION')
4     FORMAT((5(1X,E14.8)))
6     FORMAT(1X,'THE REDUCED SYSTEM: ')
7     FORMAT(1X,'HAS SOLUTION VECTOR',/,4(1X,E14.8))
8     FORMAT(1X,'NUMBER OF INTERCHANGES = ',3X,I2)
3     FORMAT(1X,'ORIGINAL SYSTEM:',/)
9     FORMAT(1X,4I5)
      END
