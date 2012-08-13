
!  compute ising transitions without loops over array elements

subroutine transition( n, ising, iterations, p)
implicit none

!  size of arrays

   integer, intent( in) :: n

!  number of iterations to compute

   integer, intent( in) :: iterations

!  spins

   logical, dimension( n, n, n), intent( in out) :: ising

!  true if this spin will flip on this iteration

   logical, dimension( n, n, n) :: flips

!  integer representations of ising

   integer, dimension( n, n, n) :: ones, neighbor_counts

!  threshold to flip

   real, dimension( n, n, n) :: threshold

!  probabilities of flipping per number of neighbors up/down

   real, intent( in) :: p( 0: 6)

!  loop through iterations

   integer :: i

!  executable code

continue

!  loop over iterations

   do i = 1, iterations

!  elements of ones are zero where elements of ising are false ( = spin down)

      ones = 0

!  elements of ones are one where elements of ising are true ( = spin up)

      where( ising) ones = 1

!  neighbor_counts the number of neighbors with spin up

      neighbor_counts = cshift( ones, -1, 1) + cshift( ones, 1, 1) &
                      + cshift( ones, -1, 2) + cshift( ones, 1, 2) &
                      + cshift( ones, -1, 3) + cshift( ones, 1, 3)

!  change the sense of the neighbor_counts where the spin is down

      where( .not. ising) neighbor_counts = 6 - neighbor_counts

!  always flip spin

      threshold = 1.0

!  unless given probability says otherwise

      where( neighbor_counts == 4) threshold = p( 4)
      where( neighbor_counts == 5) threshold = p( 5)
      where( neighbor_counts == 6) threshold = p( 6)

!  set flip when rn < threshold

      flips = ranf( n) <= threshold

!  flip ising

      where( flips) ising = .not. ising

!  end loop over iterations

   end do

!  done

return

contains

!  return an array of rn for use in expressions

function ranf( n) result( rngs)

!  size of result

integer, intent( in) :: n

!  result is an array

real, dimension( n, n, n) :: rngs

!  executable code

continue

!  from the intrinsic rng

   call random_number( harvest= rngs)

!  done

return
end function ranf

end subroutine transition
