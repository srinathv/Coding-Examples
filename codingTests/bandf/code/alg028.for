C***********************************************************************
C                                                                      *
C                       MULLER'S ALGORITHM 2.8                         *
C                                                                      *
C***********************************************************************
C
C
C
C     TO FIND A SOLUTION TO F(X) = 0 GIVEN THREE APPROXIMATIONS X0, X1
C     AND X2:
C
C     INPUT X0,X1,X2; TOLERANCE TOL; MAXIMUM NUMBER OF ITERATIONS N0.
C
C     OUTPUT APPROXIMATE SOLUTION P OR MESSAGE OF FAILURE.
C
C     THIS IMPLEMENTATION ALLOWS FOR A SWITCH TO COMPLEX ARITHMETIC
C     THE COEFFICIENTS ARE STORED IN THE VECTOR A, SO THE DIMENSION
C     OF A MAY HAVE TO BE CHANGED
      COMPLEX Z(4),G(4),CH(3),CDEL1(2),CDEL,CB,CD,CE
      DIMENSION H(3),F(4),X(4),DEL1(2),A(50)
      LOGICAL OK
      INTEGER INP,OUP,FLAG
      CHARACTER NAME*30,NAME1*30,AA*1
      OPEN(UNIT=5,FILE='CON',ACCESS='SEQUENTIAL')
      OPEN(UNIT=6,FILE='CON',ACCESS='SEQUENTIAL')
      WRITE(6,*) 'This is the Muller Method.'
      OK = .FALSE.
10    IF ( .NOT. OK) THEN
         WRITE(6,*) 'Choice of input method: '
         WRITE(6,*) '1. Input entry by entry from keyboard '
         WRITE(6,*) '2. Input data from a text file '
         WRITE(6,*) 'Choose 1 or 2 please '
         WRITE(6,*) ' '
         READ(5,*)  FLAG
         IF( ( FLAG .GE. 1 ) .AND. ( FLAG .LE. 2 )) OK = .TRUE.
         GOTO 10
      ENDIF
      IF (FLAG .EQ. 1) THEN
         OK = .FALSE.
20       IF (.NOT. OK ) THEN
            WRITE(6,*) 'Input the degree n of the polynomial'
            WRITE(6,*) ' '
            READ(5,*) N
            IF (N .GT. 0 ) THEN
               OK = .TRUE.
               N=N+1
               WRITE(6,*) 'Input the coefficients of the'
               WRITE(6,*) 'polynomial in ascending order '
               WRITE(6,*) 'of exponent at the prompt '
               WRITE(6,*) ' '
               DO 30 I = 1, N
                  J=I-1
                  WRITE(6,*) 'Input A( ',J,' )'
                  WRITE(6,*) ' '
                  READ(5,*) A(I)
30             CONTINUE
            ELSE
               WRITE(6,*) 'Number must be a positive integer '
            ENDIF
            GOTO 20
         ENDIF
      ENDIF
      IF (FLAG .EQ. 2) THEN
         WRITE(6,*) 'Has a text file been created containing the '
         WRITE(6,*) 'coefficients in ascending order of exponent. '
         WRITE(6,*) 'Place each coefficient on a separate line.'
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
               WRITE(6,*) 'Input degree N '
               WRITE(6,*) ' '
               READ(5,*) N
               IF ( N .GT. 0) THEN
                  OK = .TRUE.
                  N=N+1
                  DO 50 I = 1, N
                     READ(4,*) A(I)
50                CONTINUE
               CLOSE(UNIT=4)
               ELSE
                  WRITE(6,*) 'Number must be a positive integer '
               ENDIF
               GOTO 40
            ENDIF
         ELSE
            WRITE(6,*) 'Please create the input file.'
            WRITE(6,*) 'The program will end so the input file can '
            WRITE(6,*) 'be created. '
            OK = .FALSE.
         ENDIF
      ENDIF
      IF (A(N).EQ.0) THEN
         WRITE(6,*) 'Leading coefficient is 0 - error in input'
         OK = .FALSE.
      ENDIF
      IF (N.EQ.2.AND.OK) THEN
         P = -A(1)/A(2)
         WRITE(6,*) 'Polynomial is linear:  root is ',P
         OK = .FALSE.
      ENDIF
      IF(.NOT.OK) GOTO 400
11    OK = .FALSE.
12    IF (OK) GOTO 13
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
13    OK = .FALSE.
14    IF (OK) GOTO 15
         WRITE(6,*) 'Input maximum number of iterations '
         WRITE(6,*) '- no decimal point '
         WRITE(6,*) ' '
         READ(5,*) M
         IF ( M .LE. 0 ) THEN
           WRITE(6,*) 'Must be positive integer '
           WRITE(6,*) ' '
         ELSE
           OK = .TRUE.
         ENDIF
      GOTO 14
15    CONTINUE
      WRITE(6,*) 'Input the first of three starting values.'
      WRITE(6,*) ' '
      READ(5,*) X(1)
      WRITE(6,*) 'Input the second of three starting values.'
      WRITE(6,*) ' '
      READ(5,*) X(2)
      WRITE(6,*) 'Input the third of three starting values.'
      WRITE(6,*) ' '
      READ(5,*) X(3)
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
      WRITE(OUP,*) 'MULLER METHOD'
      WRITE(OUP,*) 'THE OUTPUT IS I, APPROXIMATION X(I), F(X(I))'
      WRITE(OUP,*) ' '
      WRITE(OUP,*) 'THREE COLUMNS MEANS THE RESULTS ARE REAL NUMBERS,'
      WRITE(OUP,*) 'FIVE COLUMNS MEANS THE RESULTS ARE COMPLEX '
      WRITE(OUP,*) 'NUMBERS WITH REAL AND IMAGINARY PARTS OF X(I)'
      WRITE(OUP,*) 'FOLLOWED BY REAL AND IMAGINARY PARTS OF F(X(I)).'
C     EVALUATE F USING HORNER'S METHOD AND STORE IN VECTOR F
      DO 60 I=1,3
         F(I)=A(N)
         DO 60 J=2,N
            K=N-J+1
60          F(I)=F(I)*X(I)+A(K)
      WRITE(OUP,5) (I-1,X(I),F(I),I=1,3)
C     VARIABLE ISW USED TO NOTE A SWITCH TO COMPLEX ARITHMETIC
C     ISW=0 MEANS REAL ARITHMETIC, AND ISW=1 MEANS COMPLEX ARITHMETIC
      ISW=0
C     STEP 1
      H(1)=X(2)-X(1)
      H(2)=X(3)-X(2)
      DEL1(1)=(F(2)-F(1))/H(1)
      DEL1(2)=(F(3)-F(2))/H(2)
      DEL=(DEL1(2)-DEL1(1))/(H(2)+H(1))
      I=3
C     STEP 2
100   IF (I.GT.M) GOTO 200
C        STEPS 3-7 FOR REAL ARITHMETIC
         IF(ISW.EQ.0) THEN
C           STEP 3
            B=DEL1(2)+H(2)*DEL
            D=B*B-4*F(3)*DEL
C           TEST TO SEE IF NEED COMPLEX ARITHMETIC
            IF(D.GE.0.0) THEN
C             REAL ARITHMETIC/ TEST TO SEE IF STRAIGHT LINE
               IF(ABS(DEL).LE.1.0E-20) THEN
C                 STRAIGHT LINE/ TEST TO SEE IF HORIZONTAL
                  IF(ABS(DEL1(2)).LE.1.0E-20) THEN
C                    HORIZONTAL LINE
                     WRITE(OUP,6)
                     GOTO 400
                  ENDIF
C                 STRAIGHT LINE BUT NOT HORIZONTAL
                  X(4)=(F(3)-DEL1(2)*X(3))/DEL1(2)
                  H(3)=X(4)-X(3)
               ELSE
C                 NOT STRAIGHT LINE
                  D=SQRT(D)
C                 STEP 4
                  E=B+D
                  IF(ABS(B-D).GT.ABS(E)) E=B-D
C                 STEP 5
                  H(3)=-2*F(3)/E
                  X(4)=X(3)+H(3)
               ENDIF
C              EVALUATE F(X(2))=F(4) BY HORNER'S METHOD
               F(4)=A(N)
               DO 70 J=2,N
                  K=N-J+1
70             F(4)=F(4)*X(4)+A(K)
               WRITE(OUP,8) I,X(4),F(4)
C              STEP 6
               IF(ABS(H(3)).LE.TOL) THEN
                  WRITE(OUP,9)
                  GOTO 400
               ENDIF
C              STEP 7
               DO 80 J=1,2
                  H(J)=H(J+1)
                  X(J)=X(J+1)
80                F(J)=F(J+1)
               X(3)=X(4)
               F(3)=F(4)
               DEL1(1)=DEL1(2)
               DEL1(2)=(F(3)-F(2))/H(2)
               DEL=(DEL1(2)-DEL1(1))/(H(2)+H(1))
            ELSE
C           SWITCH TO COMPLEX ARITHMETIC
               ISW=1
               DO 160 J=1,3
                  Z(J)=X(J)
160               G(J)=F(J)
               DO 170 J=1,2
                  CDEL1(J)=DEL1(J)
170               CH(J)=H(J)
               CDEL=DEL
            ENDIF
         ENDIF
C        STEPS 3-7 COMPLEX ARITHMETIC
         IF(ISW.EQ.1) THEN
C          TEST IF STRAIGHT LINE
            IF(CABS(CDEL).LE.1.0E-20) THEN
C             STRAIGHT LINE/ TEST IF HORIZONTAL
               IF(CABS(CDEL1(1)).LE.1.0E-20) THEN
C                 HORIZONTAL LINE
                  WRITE(OUP,6)
                  GOTO 400
               ENDIF
C              STRAIGHT LINE BUT NOT HORIZONTAL
               Z(4)=(G(3)-CDEL1(2)*Z(3))/CDEL1(2)
               CH(3)=Z(4)-Z(3)
            ELSE
C           NOT STRAIGHT LINE
C              STEP 3
               CB=CDEL1(2)+CH(2)*CDEL
               CD=CB*CB-4*G(3)*CDEL
               CD=CSQRT(CD)
C              STEP 4
               CE=CB+CD
               IF(CABS(CB-CD).GT.CABS(CE)) CE=CB-CD
C              STEP 5
               CH(3)=-2*G(3)/CE
               Z(4)=Z(3)+CH(3)
            ENDIF
C           EVALUATE G(X(2))=G(4) BY HORNER'S METHOD
            G(4)=A(N)
            DO 180 J=2,N
               K=N-J+1
180         G(4)=G(4)*Z(4)+A(K)
            WRITE(OUP,111) I,Z(4),G(4)
C           STEP 6
            IF(CABS(CH(3)).LE.TOL) THEN
               WRITE(OUP,9)
               GOTO 400
            ENDIF
C           STEP 7  CONTINUED
            DO 190 J=1,2
               CH(J)=CH(J+1)
               Z(J)=Z(J+1)
190            G(J)=G(J+1)
            Z(3)=Z(4)
            G(3)=G(4)
            CDEL1(1)=CDEL1(2)
            CDEL1(2)=(G(3)-G(2))/CH(2)
            CDEL=(CDEL1(2)-CDEL1(1))/(CH(2)+CH(1))
         ENDIF
C        STEP 7
         I=I+1
      GOTO 100
C     STEP 8
200   CONTINUE
      WRITE(OUP,121)
400   CLOSE(UNIT=5)
      CLOSE(UNIT=OUP)
      IF(OUP.NE.6) CLOSE(UNIT=6)
      STOP
3     FORMAT(8(1X,E15.8))
5     FORMAT(1X,I3,1X,E15.8,1X,E15.8)
6     FORMAT(1X,'HORIZONTAL LINE')
8     FORMAT(1X,1X,I3,1X,E15.8,1X,E15.8)
9     FORMAT(1X,'PROCEDURE COMPLETED SUCCESSFULLY')
111   FORMAT(1X,I3,4(1X,E15.8))
121   FORMAT(1X,'MAXIMUM NUMBER OF ITERATIONS EXCEEDED')
      END
