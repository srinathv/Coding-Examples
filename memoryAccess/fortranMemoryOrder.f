! from http://www.visitusers.org/index.php?title=C_vs_Fortran_memory_order

      program main
      implicit none
      integer NX,NY,NZ
      parameter (NX = 3)
      parameter (NY = 4)
      parameter (NZ = 5)
      integer values(NX,NY,NZ),i,j,k,nvals,index

      index = 0
      do 30 k=1,NZ
          do 20 j=1,NY
              do 10 i=1,NX
                  values(i,j,k) = index
                  index = index + 1
10            continue
20        continue
30    continue

      nvals = NX*NY*NZ
      call print_array_values(values, nvals)

      stop
      end
