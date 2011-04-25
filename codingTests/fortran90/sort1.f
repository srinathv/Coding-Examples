c<html>
c<head><title>sort1.f</title></head>
c<body>
c<pre>      
	SUBROUTINE SSORT (X, IY, N, KFLAG)
      IMPLICIT NONE
c
c    Example of a Selection Sort   Using a Fortran 90 Intrinsic Function
c
C***BEGIN PROLOGUE  SSORT
C***PURPOSE  Sort an array and make the same interchanges in
C            an auxiliary array.  The array is sorted in
C            decreasing order.
C***TYPE      SINGLE PRECISION
C***KEYWORDS  SORT, SORTING
C
C   Description of Parameters
C      X - array of values to be sorted   (usually abscissas)
C      IY - array to be carried with X (all swaps of X elements are
C          matched in IY .  After the sort IY(J) contains the original
C          postition of the value X(J) in the unsorted X array.
C      N - number of values in array X to be sorted
C      KFLAG - Not used in this implementation
C
C***REVISION HISTORY  (YYMMDD)
C   950310  DATE WRITTEN
C   John Mahaffy
C***END PROLOGUE  SSORT
C     .. Scalar Arguments ..
      INTEGER KFLAG, N
C     .. Array Arguments ..  -----NOTE the 2 new ways of declaring array size
      REAL X(1:N)
      INTEGER IY(N)
C     .. Local Scalars ..
      REAL TEMP
      INTEGER I, ISWAP(1), ITEMP, ISWAP1
C     .. External Subroutines ..
C     None
C     .. Intrinsic Functions ..
      INTRINSIC MAXLOC
c
c
c    MAXLOC is a FORTRAN 90 function that returns the index value for the
c    maximum element in the array
C***FIRST EXECUTABLE STATEMENT  SSORT
C
      DO 200 I=1,N-1
c<a name=1><font color=FF0000>
         ISWAP=MAXLOC(X(I:N))

c</font>
         ISWAP1=ISWAP(1)+I-1
         IF(ISWAP1.NE.I) THEN
           TEMP=X(I)
            X(I)=X(ISWAP1)
            X(ISWAP1)=TEMP
            ITEMP=IY(I)
            IY(I)=IY(ISWAP1)
            IY(ISWAP1)=ITEMP
         ENDIF
  200 CONTINUE
      RETURN
      END
c</pre>
c</body>
c</html>
