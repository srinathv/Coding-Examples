
!  supply math functions
!  version: $Id$
!  compiles with
!  gfortran -c -std=f2008 -Wall -fcheck=all Math_funcs.f90
!  use via
!  use :: math_funcs, only: ...

module math_funcs

!  get the kind value for 64-bit reals

use, intrinsic :: iso_fortran_env, only: real64

implicit none

private

!  define a 64-bit third

real( real64), parameter :: third = 1.0_real64 / 3.0_real64

!  define a 64-bit quarter

real( real64), parameter :: quarter = 0.25_real64

!  export cbrt and qurt

public :: cbrt
public :: qurt

!  define the functions

contains

!  define cbrt

function cbrt( x) result( r)

!  the result is a 64-bit real

real( real64) :: r

!  the argument is a 64-bit real

real( real64), intent( in) :: x

!  the executable text

continue

!  x^0.3 = exp of one-third the log

   r = exp( log( x) * third)

!  all done

return

!  end of cbrt

end function cbrt

!  define qurt

function qurt( x) result( r)

!  the result is a 64-bit real

real( real64) :: r

!  the argument is a 64-bit real

real( real64), intent( in) :: x

!  the executable text

continue

!  x^0.25 = exp of one-quarter the log

   r = exp( log( x) * quarter)

!  all done

return

!  end of qurt

end function qurt

!  end of the math functions module

end module math_funcs
