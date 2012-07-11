
!  display fibonacci numbers

program display_fibonacci

implicit none

!  these will change size during execution

   integer, dimension( :), allocatable :: fibs_in, fibs_out

   integer :: n, i

!  executable code

continue

!  get a maximum number to compute

   read( unit= *, fmt= *) n

!  must start with a zero-sized array

   allocate( fibs_in( 0: -1))

!  the zero sized array produces a size-1 array

   allocate( fibs_out( 0: 0))

!  compute until n have been computed

   do i = 0, n

!  compute the next f_n

      fibs_out = fibonacci( fibs_in)

!  write the array so far

      write( unit= *, fmt= *) fibs_out

!  reset the sizes

      deallocate( fibs_in)

      allocate( fibs_in( 0: ubound( fibs_out, dim= 1)))

!  and copy output to input

      fibs_in = fibs_out

      deallocate( fibs_out)

      allocate( fibs_out( 0: size( fibs_in)))

   end do

!  done

stop 'normal exit'

!  internal procedures follow

contains

!  compute the next fibonacci number

function fibonacci( f_in) result( f_out)

!  f_in is assumed to have the zeroth through nth fibonacci numbers already

integer, dimension( 0: ), intent( in) :: f_in

!  f_out will have one more

integer, dimension( 0: size( f_in)) :: f_out

!  executable code

continue

!  set the already computed ones in the output

   if( size( f_in) > 0 )then

      f_out( 0: ubound( f_in, dim= 1)) = f_in

   end if

!  which fibonacci number to compute

   select case( size( f_in))

!  an input array of size zero means compute the fibonacci number of order zero

   case( 0)

!  and store it in the output array at element zero

      f_out( 0) = 0

!  an input array of size one means compute the fibonacci number of order one

   case( 1)

!  and store it in the output array at element one

      f_out( 1) = 1

!  larger input arrays allow computation from the definition

   case( 2: )

!  and store the new result in the last element

      f_out( ubound( f_out, dim= 1)) = f_in( ubound( f_in, dim= 1)) + f_in( ubound( f_in, dim= 1) - 1)

!  any other input size is an error

   case default

!  so complain and quit

      stop 'size error in fibonacci'

   end select

!  done

return

end function fibonacci

end program display_fibonacci
