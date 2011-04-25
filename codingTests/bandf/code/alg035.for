C***********************************************************************
C                                                                      *
C            CLAMPED CUBIC SPLINE ALGORITHM 3.5                        *
C                                                                      *
C***********************************************************************
C
C
C
C
C     TO CONSTRUCT THE CUBIC SPLINE INTERPOLANT S FOR
C     THE FUNCTION F, DEFINED AT THE NUMBERS
C     X(0) < X(1) < ... < X(N), SATISFYING
C     S'(X(0)) = F'(X(0)) AND S'(X(N)) = F'(X(N)):
C
C     INPUT:   N; X(0),X(1),...,X(N); EITHER GENERATE
C              A(I) = F(X(I)) FOR I = 0,1,...,N OR INPUT
C              A(I) FOR I = 0,1,...,N; FPO = F'(X(0)); FPN = F'(X(N)).
C
C     OUTPUT:  A(J),B(J),C(J),D(J) FOR J=0,1,...,N-1.
C
C     NOTE: S(X) = A(J) + B(J)*(X-X(J)) + C(J)*(X-X(J))**2 +
C                 D(J)*(X-X(J))**3 FOR X(J) < X < X(J+1)
C
      CHARACTER NAME*30,NAME1*30,AA*1
      INTEGER INP,OUP,FLAG
      LOGICAL OK
      DIMENSION X(25),A(25),B(25),C(25),D(25),H(25),XA(25),XL(25),
     *XU(25),XZ(25)
C     CHANGE FUNCTION F FOR A NEW PROBLEM
      F(XX) = 1/XX
      OPEN(UNIT=5,FILE='CON',ACCESS='SEQUENTIAL')
      OPEN(UNIT=6,FILE='CON',ACCESS='SEQUENTIAL')
      OK = .FALSE.
10    IF ( .NOT. OK) THEN
         WRITE(6,*) 'Choice of input method: '
         WRITE(6,*) '1. Input entry by entry from keyboard '
         WRITE(6,*) '2. Input data from a text file '
         WRITE(6,*) '3. Generate data using a function F with nodes'
         WRITE(6,*) 'entered from keyboard '
         WRITE(6,*) '4. Generate data using a function F with nodes'
         WRITE(6,*) 'from a text file '
         WRITE(6,*) 'Choose 1, 2, 3, or 4 please '
         WRITE(6,*) ' '
         READ(5,*)  FLAG
         IF( ( FLAG .GE. 1 ) .AND. ( FLAG .LE. 4 )) OK = .TRUE.
         GOTO 10
      ENDIF
      IF (FLAG .EQ. 1) THEN
         OK = .FALSE.
20       IF (.NOT. OK ) THEN
            WRITE(6,*) 'Input number N '
            WRITE(6,*) ' '
            READ(5,*) N
            IF (N .GT. 0 ) THEN
               OK = .TRUE.
               N=N+1
               DO 30 I = 1, N
                  J=I-1
                  WRITE(6,*) 'Input X(',J,') and F(X(',J,')) '
                  WRITE(6,*) 'separated by space '
                  WRITE(6,*) ' '
                  READ(5,*)  X(I) , A(I)
30             CONTINUE
            ELSE
               WRITE(6,*) 'Number must be a positive integer '
            ENDIF
            GOTO 20
         ENDIF
      ENDIF
      IF (FLAG .EQ. 2) THEN
         WRITE(6,*) 'Has a text file been created with data in two '
         WRITE(6,*) 'columns? '
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
40          IF (.NOT. OK) THEN
               WRITE(6,*) 'Input number N '
               WRITE(6,*) ' '
               READ(5,*) N
               IF ( N .GT. 0) THEN
                  OK = .TRUE.
                  N=N+1
                  DO 50 I = 1, N
                     READ(4,*) X(I) , A(I)
50                CONTINUE
               CLOSE(UNIT=4)
               ELSE
                  WRITE(6,*) 'Number must be a positive integer '
               ENDIF
               GOTO 40
            ENDIF
         ELSE
            WRITE(6,*) 'Please create the input file in two column '
            WRITE(6,*) 'form with the '
            WRITE(6,*) 'X values and F(X) values in the '
            WRITE(6,*) 'corresponding columns '
            WRITE(6,*) 'The program will end so the input file can '
            WRITE(6,*) 'be created. '
            OK = .FALSE.
         ENDIF
      ENDIF
      IF (FLAG .EQ. 3) THEN
         WRITE(6,*) 'Has the function F been created in the program?'
         WRITE(6,*) 'Enter Y or N - letter within quotes'
         WRITE(6,*) ' '
         READ(5,*)  AA
         IF (( AA .EQ. 'Y' ) .OR. ( AA .EQ. 'y' )) THEN
            OK = .FALSE.
60          IF (.NOT. OK) THEN
               WRITE(6,*) 'Input number N '
               WRITE(6,*) ' '
               READ(5,*) N
               IF ( N .GT. 0 ) THEN
                  OK = .TRUE.
                  N=N+1
                  DO 70 I = 1, N
                     J=I-1
                     WRITE(6,*) 'Input X(',J,') '
                     WRITE(6,*) ' '
                     READ(5,*)  X(I)
                     A(I) = F( X(I) )
70                CONTINUE
               ELSE
                  WRITE(6,*) 'Number must be a positive integer '
               ENDIF
               GOTO 60
            ENDIF
         ELSE
            WRITE(6,*) 'The program will end so that the function F '
            WRITE(6,*) 'can be created '
            OK = .FALSE.
         ENDIF
      ENDIF
      IF ( FLAG .EQ. 4) THEN
         WRITE(6,*) 'Has the text file with X-values been created and '
         WRITE(6,*) 'has the function F been created in the program?'
         WRITE(6,*) 'Enter Y or N - letter within quotes'
         WRITE(6,*) ' '
         READ(5,*) AA
         IF (( AA .EQ. 'Y' ) .OR. ( AA .EQ. 'y')) THEN
            WRITE(6,*) 'Input the file name in the form - '
            WRITE(6,*) 'drive:name.ext  contained within quotes'
            WRITE(6,*) 'as example:   ''A:DATA.DTA'' '
            WRITE(6,*) ' '
            READ(5,*)  NAME
            INP = 4
            OPEN(UNIT=4,FILE=NAME,ACCESS='SEQUENTIAL')
            OK = .FALSE.
80          IF (.NOT. OK ) THEN
               WRITE(6,*) 'Input number N '
               WRITE(6,*) ' '
               READ(5,*)  N
               IF ( N .GT. 0 ) THEN
                  OK = .TRUE.
                  N=N+1
                  DO 90 I = 1, N
                     READ(4,*) X(I)
                     A(I) = F(X(I))
90                CONTINUE
                  CLOSE(UNIT=4)
               ELSE
                  WRITE(6,*) 'Number must be a positive integer '
               ENDIF
               GOTO 80
            ENDIF
         ELSE
            WRITE(6,*) 'Please create the input file with one entry '
            WRITE(6,*) 'per row and create the function F. '
            WRITE(6,*) 'The program will end so the input file and '
            WRITE(6,*) 'F can be created. '
            OK = .FALSE.
         ENDIF
      ENDIF
      IF(.NOT.OK) GOTO 400
      IF ( OK ) THEN
         WRITE(6,*) 'Enter F''(X(0)) and F''(X(N)) separated by blank'
         WRITE(6,*) ' '
         READ(5,*) FPO, FPN
      ENDIF
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
      M = N-1
C     GENERATE H(I)=X(I+1)-X(I)
      DO 101 I=1,M
      H(I) = X(I+1)-X(I)
101   CONTINUE
C     STEP 2
C     USE XA INSTEAD OF ALPHA
      XA(1) = 3*(A(2)-A(1))/H(1)-3*FPO
      XA(N) = 3*FPN-3*(A(N)-A(N-1))/H(N-1)
C     STEP 3
      DO 21 I=2,M
      XA(I) = 3*(A(I+1)*H(I-1)-A(I)*(X(I+1)-X(I-1))+A(I-1)*H(I))/
     *(H(I)*H(I-1))
21    CONTINUE
C     STEP 4
C     STEPS 4,5,6 AND PART OF 7 SOLVE A TRIDIAGONAL LINEAR SYSTEM
C     USING ALGORITHM 6.7.
C
C     USE XL, XU, XZ IN PLACE OF L, MU, Z RESP.
      XL(1) = 2*H(1)
      XU(1) = 0.5
      XZ(1) = XA(1)/XL(1)
C     STEP 5
      DO 31 I=2,M
         XL(I) = 2*(X(I+1)-X(I-1))-H(I-1)*XU(I-1)
         XU(I) = H(I)/XL(I)
         XZ(I) = (XA(I)-H(I-1)*XZ(I-1))/XL(I)
31    CONTINUE
C     STEP 6
      XL(N) = H(N-1)*(2-XU(N-1))
      XZ(N) = (XA(N)-H(N-1)*XZ(N-1))/XL(N)
      C(N) = XZ(N)
C     STEP 7
      DO 41 I=1,M
            J = N-I
            C(J) = XZ(J)-XU(J)*C(J+1)
            B(J) = (A(J+1)-A(J))/H(J)-H(J)*(C(J+1)+2*C(J))/3
      D(J) = (C(J+1)-C(J))/(3*H(J))
41    CONTINUE
C     STEP 8
      WRITE(OUP,*) 'The numbers X(0), ..., X(N) are: '
      DO 100 I = 1, N
         WRITE(OUP,*) X(I)
100   WRITE(OUP,*) ' '
      WRITE(OUP,*) 'The coefficients of the spline are'
      WRITE(OUP,*) 'A(I), B(I), C(I), D(I) for I = 0, ..., N-1 '
      DO 110 I = 1, M
         WRITE(OUP,*) I,A(I),B(I),C(I),D(I)
110   WRITE(OUP,*) ' '
400   CLOSE(UNIT=5)
      CLOSE(UNIT=OUP)
      IF(OUP.NE.6) CLOSE(UNIT=6)
      STOP
4     FORMAT(1X,I2,4(1X,E15.8))
      END
