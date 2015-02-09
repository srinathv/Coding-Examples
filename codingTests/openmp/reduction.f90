!******************************************************************************
! FILE: omp_reduction.f
! DES!RIPTION:
!   OpenMP Example - !ombined Parallel Loop Reduction - Fortran Version
!   This example demonstrates a sum reduction within a combined parallel loop
!   construct.  Notice that default data element scoping is assumed - there
!   are no clauses specifying shared or private variables.  OpenMP will
!   automatically make loop index variables private within team threads, and
!   global variables shared.
! AUTHOR: Blaise Barney  5/99
! LAST REVISED:
!******************************************************************************

      PROGRAM REDUCTION

      INTEGER I, N , NT, omp_get_num_threads
      REAL A(100), B(100), SUM

!$OMP PARALLEL 
   NT=omp_get_num_threads()
!$OMP END PARALLEL
   print *, NT

!     Some initializations
      N = 100
      DO I = 1, N
        A(I) = I *1.0
        B(I) = A(I)
      ENDDO
      SUM = 0.0

!$OMP PARALLEL DO REDUCTION(+:SUM)
      DO I = 1, N
        SUM = SUM + (A(I) * B(I))
      ENDDO

      PRINT *, '   Sum = ', SUM
      END
