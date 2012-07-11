
!  write all prime numbers between two limits

program find_primes

!  current limits and this test case

integer :: n, max_n, min_n, i, max_i

!  list of factors

integer, dimension( :), allocatable :: factors

!  executable code

continue

!  get the limits

   read( unit= *, fmt= *) min_n, max_n

!  start at the low limit

   n = min_n

!  loop until the high limit

   check_n: do

!  test the next candidate prime

      n = n + 1

!  only test less than the square root

      max_i = nint( sqrt( real( n)))

!  allocate a right-sized factors array

      allocate( factors( max_i), source= 0)

!  initialize the candidate possible factor

      i = 1

!  test all possible candidate factors

      check_i: do

!  test the next candidate factor

         i = i + 1

!  quit when past square root

         if( i > max_i ) exit check_i

!  mark when an even division occurs

         if( mod( n, i) == 0 )then

!  mark this entry

            factors( i) = i

         end if

      end do check_i

!  primes have no factors

      if( all( factors == 0 ) ) write( unit= *, fmt= *) n

!  remove factor array

      deallocate( factors)

!  quit when past high limit

      if( n > max_n ) exit check_n

   end do check_n

!  done

stop 'normal exit in find_primes'

end program find_primes
