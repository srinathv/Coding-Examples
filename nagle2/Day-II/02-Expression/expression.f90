! bof
! **********************************************************************
! Fortran 95 module expression

! ----------------------------------------------------------------------
! Source Control Strings

! $Id$

! ----------------------------------------------------------------------
!  Copyright 2012 Dan Nagle

!   This library is free software; you can redistribute it and/or
!   modify it under the terms of the GNU Library General Public
!   License as published by the Free Software Foundation; either
!   version 2 of the License, or (at your option) any later version.

!   This library is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!   Library General Public License for more details.

!   You should have received a copy of the GNU Library General Public
!   License along with this library; if not, write to the Free
!   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

! To report bugs, suggest enhancements, etc. to the Authors,
! Contact:
!    Dan Nagle
!                               send email to dannagle@verizon.net
!                                  or mail to 2820 Lafayette Dr
!                                             Boulder CO 80305 USA

! ----------------------------------------------------------------------
! expression description

! ----------------------------------------------------------------------

!  expression uses

!     iso_fortran_env- describes the processor

!  expression includes

!     <none>

!  expression constants

!  expression types

!  expression data

!  expression library

! **********************************************************************

!  expression

! ----------------------------------------------------------------------

module expression

! ----------------------------------------------------------------------

!  expression uses modules

!  <none>

! ----------------------------------------------------------------------

!  no implicit typing: require explicit declarations

implicit none

! ----------------------------------------------------------------------

!  all names are private: require explicit exports

private

! ----------------------------------------------------------------------

!  expression RCS strings

! ----------------------------------------------------------------------

!  module source filename supplied by RCS

character( len= *), public, parameter :: expression_rcs_id = &
   '$Id$'

! ----------------------------------------------------------------------

!  expression constants

! ----------------------------------------------------------------------

integer, parameter :: no_op = 0
integer, parameter :: add_op = 1
integer, parameter :: sub_op = 2
integer, parameter :: mul_op = 3
integer, parameter :: div_op = 4
integer, parameter :: pow_op = 5
integer, parameter :: mod_op = 6

! ----------------------------------------------------------------------

!  expression types

! ----------------------------------------------------------------------

!  an operator has two operands

type, abstract, public :: expression_t

   integer :: op_code = no_op

end type expression_t

!  an integer expression

type, extends( expression_t), public :: integer_expression_t

   type( integer_expression_t), pointer :: left => null()
   type( integer_expression_t), pointer :: right => null()

   integer :: value

end type integer_expression_t

type, extends( expression_t), public :: real_expression_t

   type( real_expression_t), pointer :: left => null()
   type( real_expression_t), pointer :: right => null()

   real :: value

end type real_expression_t

! ----------------------------------------------------------------------

!  expression data

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------

!  expression library

! ----------------------------------------------------------------------

public :: operator( +)

interface operator( +)

   module procedure exp_add 
   module procedure re_exp_add 

end interface operator( +)

public :: operator( -)

interface operator( -)

   module procedure exp_sub 
   module procedure re_exp_sub 

end interface operator( -)

public :: operator( *)

interface operator( *)

   module procedure exp_mul 
   module procedure re_exp_mul 

end interface operator( *)

public :: operator( /)

interface operator( /)

   module procedure exp_div 
   module procedure re_exp_div 

end interface operator( /)

public :: operator( **)

interface operator( **)

   module procedure exp_pow 
   module procedure re_exp_pow 

end interface operator( **)

public :: operator( .mod.)

interface operator( .mod.)

   module procedure exp_mod 
   module procedure re_exp_mod 

end interface operator( .mod.)

public :: evaluate 

interface evaluate
  module procedure evaluate_int, evaluate_re
end interface evaluate

! ----------------------------------------------------------------------

!  module procedures

! ----------------------------------------------------------------------

contains

! ----------------------------------------------------------------------

!  evaluate an expression

recursive subroutine evaluate_int( expression)

type( integer_expression_t), intent( in out) :: expression

continue

   if( associated( expression% left)) call evaluate( expression% left)

   if( associated( expression% right)) call evaluate( expression% right)

   select case( expression% op_code)

   case( no_op)

      continue

   case( add_op)

      expression% value = expression% left% value + expression% right% value

   case( sub_op)

      expression% value = expression% left% value - expression% right% value

   case( mul_op)

      expression% value = expression% left% value * expression% right% value

   case( div_op)

      expression% value = expression% left% value / expression% right% value

   case( pow_op)

      expression% value = expression% left% value ** expression% right% value

   case( mod_op)

      expression% value = mod( expression% left% value, expression% right% value)

   case default

      stop 'invalid op code'

   end select

return

end subroutine evaluate_int

! ----------------------------------------------------------------------

function exp_add( left, right) result( e)
type( integer_expression_t), pointer :: e

type( integer_expression_t), intent( in), target :: left
type( integer_expression_t), intent( in), target :: right

continue

   allocate( e)

   e% left => left
   e% right => right

   e% op_code = add_op

return

end function exp_add

! ----------------------------------------------------------------------

function exp_sub( left, right) result( e)
type( integer_expression_t), pointer :: e

type( integer_expression_t), intent( in), target :: left
type( integer_expression_t), intent( in), target :: right

continue

   allocate( e)

   e% left => left
   e% right => right

   e% op_code = sub_op

return

end function exp_sub

! ----------------------------------------------------------------------

function exp_mul( left, right) result( e)
type( integer_expression_t), pointer :: e

type( integer_expression_t), intent( in), target :: left
type( integer_expression_t), intent( in), target :: right

continue

   allocate( e)

   e% left => left
   e% right => right

   e% op_code = mul_op

return

end function exp_mul

! ----------------------------------------------------------------------

function exp_div( left, right) result( e)
type( integer_expression_t), pointer :: e

type( integer_expression_t), intent( in), target :: left
type( integer_expression_t), intent( in), target :: right

continue

   allocate( e)

   e% left => left
   e% right => right

   e% op_code = div_op

return

end function exp_div

! ----------------------------------------------------------------------

function exp_pow( left, right) result( e)
type( integer_expression_t), pointer :: e

type( integer_expression_t), intent( in), target :: left
type( integer_expression_t), intent( in), target :: right

continue

   allocate( e)

   e% left => left
   e% right => right

   e% op_code = pow_op

return

end function exp_pow

! ----------------------------------------------------------------------

function exp_mod( left, right) result( e)
type( integer_expression_t), pointer :: e

type( integer_expression_t), intent( in), target :: left
type( integer_expression_t), intent( in), target :: right

continue

   allocate( e)

   e% left => left
   e% right => right

   e% op_code = mod_op

return

end function exp_mod

! ----------------------------------------------------------------------

!  expression_re 


!  evaluate an expression

recursive subroutine evaluate_re( expression)

type( real_expression_t), intent( in out) :: expression

continue

   if( associated( expression% left)) call evaluate( expression% left)

   if( associated( expression% right)) call evaluate( expression% right)

   select case( expression% op_code)

   case( no_op)

      continue

   case( add_op)

      expression% value = expression% left% value + expression% right% value

   case( sub_op)

      expression% value = expression% left% value - expression% right% value

   case( mul_op)

      expression% value = expression% left% value * expression% right% value

   case( div_op)

      expression% value = expression% left% value / expression% right% value

   case( pow_op)

      expression% value = expression% left% value ** expression% right% value

   case( mod_op)

      expression% value = mod( expression% left% value, expression% right% value)

   case default

      stop 'invalid op code'

   end select

return

end subroutine evaluate_re

! ----------------------------------------------------------------------

function re_exp_add( left, right) result( e)
type( real_expression_t), pointer :: e

type( real_expression_t), intent( in), target :: left
type( real_expression_t), intent( in), target :: right

continue

   allocate( e)

   e% left => left
   e% right => right

   e% op_code = add_op

return

end function re_exp_add

! ----------------------------------------------------------------------

function re_exp_sub( left, right) result( e)
type( real_expression_t), pointer :: e

type( real_expression_t), intent( in), target :: left
type( real_expression_t), intent( in), target :: right

continue

   allocate( e)

   e% left => left
   e% right => right

   e% op_code = sub_op

return

end function re_exp_sub

! ----------------------------------------------------------------------

function re_exp_mul( left, right) result( e)
type( real_expression_t), pointer :: e

type( real_expression_t), intent( in), target :: left
type( real_expression_t), intent( in), target :: right

continue

   allocate( e)

   e% left => left
   e% right => right

   e% op_code = mul_op

return

end function re_exp_mul

! ----------------------------------------------------------------------

function re_exp_div( left, right) result( e)
type( real_expression_t), pointer :: e

type( real_expression_t), intent( in), target :: left
type( real_expression_t), intent( in), target :: right

continue

   allocate( e)

   e% left => left
   e% right => right

   e% op_code = div_op

return

end function re_exp_div

! ----------------------------------------------------------------------

function re_exp_pow( left, right) result( e)
type( real_expression_t), pointer :: e

type( real_expression_t), intent( in), target :: left
type( real_expression_t), intent( in), target :: right

continue

   allocate( e)

   e% left => left
   e% right => right

   e% op_code = pow_op

return

end function re_exp_pow

! ----------------------------------------------------------------------

function re_exp_mod( left, right) result( e)
type( real_expression_t), pointer :: e

type( real_expression_t), intent( in), target :: left
type( real_expression_t), intent( in), target :: right

continue

   allocate( e)

   e% left => left
   e% right => right

   e% op_code = mod_op

return

end function re_exp_mod

end module expression


