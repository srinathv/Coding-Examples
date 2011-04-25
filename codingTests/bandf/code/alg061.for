C***********************************************************************
C                                                                      *
C  GAUSSIAN ELIMINATION WITH BACKWARD SUBSTITUTION ALGORITHM 6.1  *
C                                                                      *
C***********************************************************************
C
C
C
C     TO SOLVE THE N BY N LINEAR SYSTEM
C
C   E1:  A(1,1) X(1) + A(1,2) X(2) + ... + A(1,N) X(N) = A(1,N+1)
C   E2:  A(2,1) X(1) + A(2,2) X(2) + ... + A(2,N) X(N) = A(2,N+1)
C   :
C   .
C   EN:  A(N,1) X(1) + A(N,2) X(2) + ... + A(N,N) X(N) = A(N,N+1)
C
C     INPUT:   NUMBER OF UNKNOWNS AND EQUATIONS N; AUGMENTED
C              MATRIX A = (A(I,J)) WHERE 1<=I<=N AND 1<=J<=N+1.
C
C     OUTPUT:  SOLUTION X(1),X(2),...,X(N) OR A MESSAGE THAT THE LINEAR
C              SYSTEM HAS NO UNIQUE SOLUTION.
C
C     INITIALZATION
      DIMENSION A(10,11), X(10)
      CHARACTER NAME*30,NAME1*30,AA*1
      INTEGER INP,OUP,FLAG
      LOGICAL OK
      OPEN(UNIT=5,FILE='CON',ACCESS='SEQUENTIAL')
      OPEN(UNIT=6,FILE='CON',ACCESS='SEQUENTIAL')
      WRITE(6,*) 'This is Gaussian Elimination.'
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
9        IF (OK) GOTO 11
         WRITE(6,*) 'Input the number of equations - an integer '
         WRITE(6,*) ' '
         READ(5,*) N
         IF (N .GT. 0) THEN
            M = N+1
            READ(INP,*) ((A(I,J), J=1,M),I=1,N)
            OK = .TRUE.
            CLOSE(UNIT=INP)
         ELSE
            WRITE(6,*) 'The number must be a positive integer'
         ENDIF
         GOTO 9
      ELSE
         WRITE(6,*) 'The program will end so the input file can '
         WRITE(6,*) 'be created. '
         OK = .FALSE.
      ENDIF
11    IF( .NOT. OK) GOTO 400
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
      WRITE(OUP,*) 'GAUSSIAN ELIMINATION'
C     ICHG COUNTS NUMBER OF INTERCHANGES
      ICHG = 0
      WRITE(OUP,3)
      WRITE(OUP,4) ((A(I,J),J=1,M),I=1,N)
C     STEP 1
C     ELIMINATION PROCESS
      NN = N-1
      DO 10 I=1,NN
C          STEP 2
C          USE IP IN PLACE OF P
           IP = I
100        IF (ABS(A(IP,I)).GE.1.0E-20 .OR. IP.GT.N) GOTO 200
                IP = IP+1
           GOTO 100
200        IF(IP.EQ.N+1)THEN
C               SYSTEM DOES NOT HAVE UNIQUE SOLUTION
                WRITE(OUP,5)
                GOTO 400
           END IF
C          STEP 3
           IF(IP.NE.I) THEN
                DO 20 JJ=1,M
                     C = A(I,JJ)
                     A(I,JJ) = A(IP,JJ)
20              A(IP,JJ) = C
                ICHG = ICHG+1
           END IF
C          STEP 4
           JJ = I+1
           DO 30 J=JJ,N
C               STEP 5
C               USE XM IN PLACE OF M(J,I)
                XM = A(J,I)/A(I,I)
C               STEP 6
                DO 40 K=JJ,M
40              A(J,K) = A(J,K)-XM*A(I,K)
C               MULTIPLIER XM COULD BE SAVED IN A(J,I)
30         A(J,I) = 0
10    CONTINUE
C     STEP 7
      IF(ABS(A(N,N)).LT.1.0E-20) THEN
C          SYSTEM DOES NOT HAVE UNIQUE SOLUTION
           WRITE(OUP,5)
           GOTO 400
      END IF
C     STEP 8
C     START BACKWARD SUBSTITUTION
      X(N) = A(N,N+1)/A(N,N)
C     STEP 9
      L = N-1
      DO 15 K=1,L
           I = L-K+1
           JJ = I+1
           SUM = 0.0
           DO 16 KK=JJ,N
16         SUM = SUM-A(I,KK)*X(KK)
15    X(I) = (A(I,N+1)+SUM)/A(I,I)
      WRITE(OUP,6)((A(I,J),J=1,M),I=1,N)
C     STEP 10
C     PROCEDURE COMPLETED SUCCESSFULLY
      WRITE(OUP,7)(X(I),I=1,N)
      WRITE(OUP,8) ICHG
400   CLOSE(UNIT=5)
      CLOSE(UNIT=OUP)
      IF(OUP.NE.6) CLOSE(UNIT=6)
      STOP
5     FORMAT(1X,'THE PRECEDING SYSTEM HAS NO UNIQUE SOLUTION')
4     FORMAT(5(1X,E15.8))
6     FORMAT(1X,'THE REDUCED SYSTEM:',/,(5(1X,E14.8)))
7     FORMAT(1X,'HAS SOLUTION VECTOR',/,4(1X,E14.8))
8     FORMAT(1X,'NUMBER OF INTERCHANGES = ',3X,I2)
3     FORMAT(1X,'ORIGINAL SYSTEM',/)
      END
