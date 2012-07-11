
!  run the transitions example

program run_transitions

use, intrinsic :: iso_fortran_env, only: real64

integer, parameter :: n = 10

integer, parameter :: max_iter = 1000

real, dimension( 6), parameter :: p = [ 0.0, 0.0, 0.0, 0.5, 0.5, 0.5 ]

logical, dimension( n, n, n) :: ising

   integer :: i, j, k

continue

   do k = 1, n

      do j = 1, n

         do i = 1, n

            ising( i, j, k) = bernoulli()

         end do

      end do

   end do

   call transition( n, ising, max_iter, p)

   call print_ising()

stop 'normal exit in run_transitions'

contains

!  display the array

subroutine print_ising()

   integer :: i, j, k

continue

   do k = 1, n

      do j = 1, n

         write( *, *) ( ising( k, j, i), i = 1, n)

      end do

   end do

return

end subroutine print_ising

!  t/f

function bernoulli() result( b)
logical :: b

   real( real64) :: rn

continue

   call random_number( harvest= rn)

   b = rn < 0.5_real64

return

end function bernoulli

end program run_transitions
