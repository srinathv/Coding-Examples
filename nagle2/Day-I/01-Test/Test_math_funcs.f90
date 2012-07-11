
!  exercise pFUnit with simple tests of math functions
!  version: $Id$
!  compiles with
!  gfortran -c -std=f2008 -Wall -fcheck=all -I$PFUNIT Test_math_funcs.f90
!  use via
!  use :: test_math_funcs, only: ...

module test_math_funcs

implicit none

private

!  export test_cbrt and test_qurt

public :: test_cbrt
public :: test_qurt

!  define the functions

contains

!  define test_cbrt

subroutine test_cbrt()

!  get the kind value for 64-bit reals

use, intrinsic :: iso_fortran_env, only: real64

!  get pfunit definitions

use :: pfunit

!  get cbrt to test

use :: math_funcs, only: cbrt

!  function input and function result

   real( real64) :: a, acb

!  the executable text

continue

!  define the function input

   a = 3.0_real64 ** 3

!  compute the function result

   acb = cbrt( a)

!  check for (reasonable) equality

   call assertequal( acb*acb*acb, a, tolerance= 0.0000001)

!  all done

return

!  end of test_cbrt

end subroutine test_cbrt

!  define test_qurt

subroutine test_qurt()

!  get the kind value for 64-bit reals

use, intrinsic :: iso_fortran_env, only: real64

!  get pfunit definitions

use :: pfunit

!  get qurt to test

use :: math_funcs, only: qurt

!  function input and function result

   real( real64) :: a, aqb

!  the executable text

continue

!  define the function input

   a = 3.0_real64 ** 4

!  compute the function result

   aqb = qurt( a)

!  check for (reasonable) equality

   call assertequal( aqb*aqb*aqb*aqb, a, tolerance= 0.0000001)

!  all done

return

!  end of test_qurt

end subroutine test_qurt

!  end of the test module

end module test_math_funcs
