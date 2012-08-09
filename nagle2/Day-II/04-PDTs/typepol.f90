! bof
! **********************************************************************
! Source Control:

! $Id$

! **********************************************************************
!  copyright 2012 Dan Nagle

! **********************************************************************
! Fortran 90 module type_polar

! **********************************************************************
! a polar representation of type complex

! **********************************************************************

!  =, pol(), cmplx(), real(), int(), aimag(), radius(), angle()
!  +, -, *, /, **, abs()
!  unary +, -, conjg()
!  .eq., .ne.
!  exp(), log(), sqrt()
!  sin(), cos(), tan(), cot(), sec(), csc()
!  asin(), acos(), atan(), atan2(), acot(), acot2(), asec(), acsc()
!  rotate(), scale(), rsheet(), hsheet()

!  bit_size() for type_polar

!  type_polar_version()

module type_polar

! **********************************************************************
! a basic description of the processor, including all kinds, etc.

use, intrinsic :: iso_fortran_env

private

! **********************************************************************

!  RCS identifier

character( len= *), parameter, public :: type_polar_rcs_id = &
   '$Id'

! **********************************************************************

!  definition of the type

type, public :: polar_t( p_k)

   integer, kind :: p_k

   real( kind= p_k) :: r
   real( kind= p_k) :: theta

end type polar_t

! **********************************************************************

!  assignment

interface assignment( =)
   module procedure p_to_c, p_to_r, p_to_i, c_to_p
end interface

! **********************************************************************

!  explicit conversion

! **********************************************************************

!  p = pol( c)

interface pol
   module procedure complex_pol
end interface

! **********************************************************************

!  c = cmplx( p)

intrinsic :: cmplx

interface cmplx
   module procedure polar_cmplx
end interface

! **********************************************************************

!  r = real( p)

intrinsic :: real

interface real
   module procedure polar_real
end interface

! **********************************************************************

!  i = int( p)

intrinsic :: int

interface int
   module procedure polar_int
end interface

! **********************************************************************

!  r = aimag( p)

interface aimag
   module procedure polar_aimag
end interface

! **********************************************************************

!  r = radius( p)

interface radius
   module procedure polar_radius
end interface

! **********************************************************************

!  r = angle( p)

interface angle
   module procedure polar_angle
end interface

! **********************************************************************

!  arithmetic operators:  +, -, *, /, **, abs()

! **********************************************************************

!  p = p + p

interface operator( +)
   module procedure polar_add
end interface

! **********************************************************************

!  p = p - p

interface operator( -)
   module procedure polar_sub
end interface

! **********************************************************************

!  p = p * p

interface operator( *)
   module procedure polar_mul
end interface

! **********************************************************************

!  p = p / p

interface operator( /)
   module procedure polar_div
end interface

! **********************************************************************

!  p = p ** p

interface operator( **)
   module procedure polar_pow
end interface

! **********************************************************************

!  r = abs( p)

intrinsic :: abs

interface abs
   module procedure polar_abs
end interface

! **********************************************************************

!  unary operators:  +, -, conjg()

! **********************************************************************

!  p = +p

interface operator( +)
   module procedure polar_plus
end interface

! **********************************************************************

!  p = -p

interface operator( -)
   module procedure polar_minus
end interface

! **********************************************************************

!  p = conjg( p)

intrinsic :: conjg

interface conjg
   module procedure polar_conjg
end interface

! **********************************************************************

!  p = p .eq. p

interface operator( .eq.)
   module procedure polar_eq
end interface

! **********************************************************************

!  p = p .ne. p

interface operator( .ne.)
   module procedure polar_ne
end interface

! *********************************************************************

!  enforce use of generic name, operator or assignment

   private p_to_c, p_to_r, p_to_i, c_to_p
   private complex_pol, polar_cmplx, polar_real

   private polar_aimag, polar_radius, polar_angle

   private polar_add, polar_sub, polar_mul, polar_div
   private polar_pow, polar_abs

   private polar_plus, polar_minus, polar_conjg

   private polar_eq, polar_ne

! *********************************************************************

!  library

contains

! *********************************************************************

!  assignment

! *********************************************************************

!  p_to_c(): complex = polar

subroutine p_to_c( c, p)

type( polar_t( real64)), intent( in) :: p

complex( kind= real64), intent( out) :: c

!  begin

continue

   c = cmplx( p%r*cos( p%theta), p%r*sin( p%theta) )

return

!  p_to_c()

end subroutine

! *********************************************************************

!  p_to_r(): real = polar

subroutine p_to_r( r, p)

type( polar_t( real64)), intent( in) :: p

real( kind= real64), intent( out) :: r

!  begin

continue

   r = p%r * cos( p%theta)

return

!  p_to_r()

end subroutine

! *********************************************************************

!  p_to_i(): integer = polar

subroutine p_to_i( i, p)

type( polar_t( real64)), intent( in) :: p

integer, intent( out) :: i

!  begin

continue

   i = int( p%r * cos( p%theta) )

return

!  p_to_i()

end subroutine

! *********************************************************************

!  c_to_p(): polar = complex

subroutine c_to_p( p, c)

complex( kind= real64), intent( in) :: c

type( polar_t( real64)), intent( out) :: p

!  begin

continue

   p%r = abs( c)

   p%theta = atan2( aimag( c), real( c, kind= real64) )

return

!  c_to_p()

end subroutine

! *********************************************************************

!  explicit conversion

! *********************************************************************

!  pol(): polar = pol( complex)

type( polar_t( real64)) function complex_pol( c)

complex( kind= real64), intent( in) :: c

!  begin

continue

   complex_pol%r = abs( c)

   complex_pol%theta = atan2( aimag( c), real( c, kind= real64) )

return

!  complex_pol()

end function

! *********************************************************************

!  cmplx(): complex = cmplx( polar)

complex( kind= real64) function polar_cmplx( p)

type( polar_t( real64)), intent( in) :: p

!  begin

continue

   polar_cmplx = cmplx( p%r*cos( p%theta), p%r*sin( p%theta) )

return

!  polar_cmplx()

end function

! *********************************************************************

!  real(): real = real( polar)

real( kind= real64) function polar_real( p)

type( polar_t( real64)), intent( in) :: p

!  begin

continue

   polar_real = p%r*cos( p%theta)

return

!  polar_real()

end function

! *********************************************************************

!  int(): integer = int( polar)

real( kind= real64) function polar_int( p)

type( polar_t( real64)), intent( in) :: p

!  begin

continue

   polar_int = int( p%r*cos( p%theta) )

return

!  polar_int()

end function

! *********************************************************************

!  polar utilities conversion

! *********************************************************************

!  aimag(): polar = aimag( polar)

type( polar_t( real64)) function polar_aimag( p)

type( polar_t( real64)), intent( in) :: p

!  begin

continue

   polar_aimag%theta = -p%theta

return

!  polar_aimag()

end function

! *********************************************************************

!  radius(): real = radius( polar)

real( kind= real64) function polar_radius( p)

type( polar_t( real64)), intent( in) :: p

!  begin

continue

   polar_radius = p%r

return

!  polar_radius()

end function

! *********************************************************************

!  arithmetic operators: +, -, *, /, **, abs()

! *********************************************************************

!  angle(): real = angle( polar)

real( kind= real64) function polar_angle( p)

type( polar_t( real64)), intent( in) :: p

!  begin

continue

   polar_angle = p%theta

return

!  polar_angle()

end function

! *********************************************************************

!  polar_add(): p = p + p

type( polar_t( real64)) function polar_add( p1, p2)

type( polar_t( real64)), intent( in) :: p1, p2

!  begin

continue

   polar_add = pol( cmplx( p1) + cmplx( p2) )

return

!  polar_add()

end function

! *********************************************************************

!  polar_sub(): p = p - p

type( polar_t( real64)) function polar_sub( p1, p2)

type( polar_t( real64)), intent( in) :: p1, p2

!  begin

continue

   polar_sub = pol( cmplx( p1) - cmplx( p2) )

return

!  polar_sub()

end function

! *********************************************************************

!  polar_mul(): p = p * p

type( polar_t( real64)) function polar_mul( p1, p2)

type( polar_t( real64)), intent( in) :: p1, p2

!  begin

continue

   polar_mul = polar_t( p1%r*p2%r, p1%theta+p2%theta )

return

!  polar_mul()

end function

! *********************************************************************

!  polar_div(): p = p + p

type( polar_t( real64)) function polar_div( p1, p2)

type( polar_t( real64)), intent( in) :: p1, p2

!  begin

continue                                        ! here from caller

   polar_div = polar_t( p1%r/p2%r, p1%theta-p2%theta)

return                                          ! back to caller

!  polar_div()

end function

! *********************************************************************

!  polar_pow(): p = p ** p

type( polar_t( real64)) function polar_pow( p1, p2)

type( polar_t( real64)), intent( in) :: p1, p2

!  begin

continue                                        ! here from caller

   polar_pow = polar_t( p1%r**p2%r, p1%theta*p2%theta)

return                                          ! back to caller

!  polar_pow()

end function

! *********************************************************************

!  polar_abs():

real function polar_abs( p)

type( polar_t( real64)), intent( in) :: p

!  begin

continue                                        ! here from caller

   polar_abs = p%r

return                                          ! back to caller

!  polar_abs()

end function

! **********************************************************************

!  unary operators:  +, -, conjg()

! **********************************************************************

!  p = +p

type( polar_t( real64)) function polar_plus( p)

type( polar_t( real64)), intent( in) :: p

!  begin

continue                                        ! here from caller

   polar_plus = p

return                                          ! back to caller

!  polar_plus()

end function

! **********************************************************************

!  p = -p

type( polar_t( real64)) function polar_minus( p)

type( polar_t( real64)), intent( in) :: p

!  begin

continue                                        ! here from caller

   polar_minus%theta = p%theta + 3.1415926_real64

return                                          ! back to caller

!  polar_minus()

end function

! **********************************************************************

!  p = conjg( p)

type( polar_t( real64)) function polar_conjg( p)

type( polar_t( real64)), intent( in) :: p

!  begin

continue                                        ! here from caller

   polar_conjg%theta = -p%theta

return                                          ! back to caller

!  polar_conjg()

end function

! **********************************************************************

!  logical operators:  .eq., .ne.

! **********************************************************************

!  p = p .eq. p

logical function polar_eq( p1, p2)

type( polar_t( real64)), intent( in) :: p1, p2

!  begin

continue                                        ! here from caller

   polar_eq = ( p1%r == p2%r) .and. ( p1%theta == p2%theta)

return                                          ! back to caller

!  polar_eq()

end function

! **********************************************************************

!  p = p .ne. p

logical function polar_ne( p1, p2)

type( polar_t( real64)), intent( in) :: p1, p2

!  begin

continue                                        ! here from caller

   polar_ne = ( p1%r /= p2%r) .or. ( p1%theta /= p2%theta)

return                                          ! back to caller

!  polar_ne()

end function

! *********************************************************************

!  type_polar

! $Id$
! *********************************************************************

end module type_polar
