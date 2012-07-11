! ---------------------------------------------------------------------

module normalized_accumulator_type

use, intrinsic :: iso_fortran_env, only: wk => real64

implicit none

private

! ---------------------------------------------------------------------

integer, parameter, public :: acc_wk = wk

real( wk), parameter :: zero = 0.0_wk

!  the accumulator has a bin for each possible exponent value

type, public :: normalized_accumulator_t

   private

   real( wk), dimension( minexponent( 0.0_wk): maxexponent( 0.0_wk)) :: bins

end type normalized_accumulator_t

!  accumulator + real

interface operator( +)

   module procedure add_real

end interface operator( +)

!  real = accumulator

interface assignment( =)

   module procedure assign_real

end interface assignment( =)

!  the public face of the accumulator_t

public :: initialize
public :: count_magnitude
public :: operator( +)
public :: assignment( =)

! ---------------------------------------------------------------------

!  library

contains

! ---------------------------------------------------------------------

!  set al the bins to zero

pure subroutine initialize( a)

type( normalized_accumulator_t), intent( out) :: a

continue

   a% bins = zero

return

end subroutine initialize

! ---------------------------------------------------------------------

!  count each bin that holds a value greater than its exponent

pure function count_magnitude( a) result( c)
integer :: c

type( normalized_accumulator_t), intent( in) :: a

   integer :: i

continue

   c = 0

   scan_bins: do i = lbound( a% bins, dim= 1), ubound( a% bins, dim= 1)

      check: if( exponent( a% bins( i)) > i )then

         c = c + 1

      end if check

   end do scan_bins

return

end function count_magnitude

! ---------------------------------------------------------------------

!  add to the one bin with the matching exponent

pure function add_real( a, r) result( s)

type( normalized_accumulator_t) :: s
type( normalized_accumulator_t), intent( in) :: a
real( wk), intent( in) :: r

   integer :: i, j

continue

   s = a

   i = exponent( r)

   s% bins( i) = s% bins( i) + r

   normalize: do

      j = exponent( s% bins( i))

      if( j <= i .or. j > ubound( s% bins, dim= 1) ) exit normalize

      s% bins( j) = s% bins( j) + s% bins( i)

      s% bins( i) = zero

      i = j

   end do normalize

return

end function add_real

! ---------------------------------------------------------------------

!  sum the bins array from small to large to calculate the real value

pure subroutine assign_real( r, a)

type( normalized_accumulator_t), intent( in) :: a
real( wk), intent( out) :: r

   integer :: i

continue

   r = zero

   sum_bins: do i = lbound( a% bins, dim= 1), ubound( a% bins, dim= 1)

      r = r + a% bins( i)

   end do sum_bins

return

end subroutine assign_real

! ---------------------------------------------------------------------

end module normalized_accumulator_type
