! bof
! **********************************************************************
! Fortran 95 module type_boolean

! **********************************************************************
! Source Control Strings

! $Id: typebool.f90 1.9 2001/06/10 09:28:08Z Dan Release $

! **********************************************************************
! Copyright 2012 Dan Nagle

! **********************************************************************
! type boolean is a 32-bit typeless type with operations and routines

! **********************************************************************
! Summary of License

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
!                               send email to dnagle@ucar.edu
!                                  or mail to 2820 Lafayette Dr
!                                             Boulder, CO 80305 USA

!  a variable of type boolean consists of (wordsize) distinct bits
!  an assignment to or from a boolean variable is a bit-wise copy
!  there is no meaning assigned to any bit, hence "typeless"

! **********************************************************************

!  type_boolean types

!     boolean_t a typeless type, an ordered set of bits

!  type_boolean constants

!     all_set all bits set
!     all_clear all bits clear

!  type_boolean operators

!     = assignment

!     + unary operators
!     -

!     .set.       boolean = boolean .set. integer
!     .clear.     boolean = boolean .clear. integer
!     .isset.     logical = boolean .isset. integer
!     .isclear.   logical = boolean .isclear. integer

!     .and.       boolean = boolean .and. boolean
!     .or.        boolean = boolean .or. boolean
!     .eor.       boolean = boolean .eor. boolean
!     .not.       boolean = .not. boolean

!     .xor.       boolean = boolean .xor. boolean
!     .eqv.       boolean = boolean .eqv. boolean
!     .neqv.      boolean = boolean .neqv. boolean

!     .eq.        logical = boolean .eq. boolean
!     .ne.        logical = boolean .ne. boolean
!     .gt.        logical = boolean .gt. boolean
!     .ge.        logical = boolean .ge. boolean
!     .le.        logical = boolean .le. boolean
!     .lt.        logical = boolean .lt. boolean

!     +           boolean = boolean + boolean
!     -           boolean = boolean - boolean
!     *           boolean = boolean * boolean
!     /           boolean = boolean / boolean

!     .hamd.      integer = boolean .hamd. boolean
!     .shift.     boolean = boolean .shift. integer
!     .rotate.    boolean = boolean .rotate. integer

!  type_boolean library

!     bool()      boolean = bool( real integer logical character)
!     int()       integer = int( boolean)
!     real()      real = real( boolean)
!     logical()   logical = logical( boolean)
!     char()      character = char( boolean)

!     compl()     boolean = compl( boolean)
!     csmg()      boolean = csmg( boolean, boolean, boolean)

!     leadz()     integer = leadz( boolean)
!     lastz()     integer = lastz( boolean)
!     popcnt()    integer = popcnt( boolean)
!     poppar()    integer = poppar( boolean)
!     hamd()      integer = hamd( boolean, boolean)

!     mask()      boolean = mask( integer)
!     maskl()     boolean = maskl( integer)
!     maskr()     boolean = maskr( integer)

!     ishft()     boolean = ishft( boolean, integer)
!     ishftc()    boolean = ishftc( boolean, integer, integer)
!     dshftl()    boolean = dshftl( boolean, boolean, integer)
!     dshftr()    boolean = dshftr( boolean, boolean, integer)
!     dshftc()    call dshftc( boolean, boolean, integer)

!     ibset()     boolean = ibset( boolean, integer)
!     ibclr()     boolean = ibclr( boolean, integer)
!     btest()     logical = btest( boolean, integer)
!     bztest()    logical = btest( boolean, integer)
!     mvbits()    call mvbits( boolean, integer, integer, boolean, integer)
!     ibits()     boolean = ibits( boolean, integer, integer)
!     not()       boolean = not( boolean)

!     rev_endian()
!     bit_size()  integer = bit_size( boolean)
!     swap()      call swap( boolean, boolean)

! **********************************************************************
!  type boolean- a bit_size( word) number of bits- "typeless"

! **********************************************************************

module type_boolean

! **********************************************************************
! access the fortran environment

use, intrinsic :: iso_fortran_env, only: character_storage_size, numeric_storage_size

! **********************************************************************
! declare all variables

implicit none

! **********************************************************************
! explicit public

private

! **********************************************************************

!  RCS strings

! **********************************************************************

character( len= *), public, parameter :: type_boolean_rcs_id = &
      '$Id: typebool.f90 1.9 2001/06/10 09:28:08Z Dan Release $'

! **********************************************************************

!  define the type

! **********************************************************************

!  type boolean is one word of bits

type, public :: boolean_t
   private
   integer :: bits
end type boolean_t

! **********************************************************************

!  boolean_t constants

! **********************************************************************

!  characters per word

integer, parameter :: csu_per_nsu = numeric_storage_size / character_storage_size

! **********************************************************************

!  all_set has all bits set

type( boolean_t), public, parameter :: all_set = boolean_t( -1)

!  all_clear has all bits clear

type( boolean_t), public, parameter :: all_clear = boolean_t( 0)

! **********************************************************************

!  assignment: to/from integer, real, logical, character

public :: assignment( =)

interface assignment( =)
   module procedure b_to_i
   module procedure i_to_b
   module procedure b_to_r
   module procedure r_to_b
   module procedure b_to_l
   module procedure l_to_b
   module procedure b_to_c
   module procedure c_to_b
end interface

! **********************************************************************

!  conversion: bool(), int(), real(), logical(), char()

public :: bool

interface bool
   module procedure int_bool
   module procedure single_bool
   module procedure logical_bool
   module procedure char_bool
end interface

intrinsic :: int

public :: int

interface int
   module procedure boolean_int
end interface

intrinsic :: real

public :: real

interface real
   module procedure boolean_real
end interface

intrinsic :: logical

public :: logical

interface logical
   module procedure boolean_logical
end interface

intrinsic :: char

public :: char

interface char
   module procedure boolean_char
end interface

! **********************************************************************

!  unary operators

! **********************************************************************

!  boolean unary operator +

public :: operator( +)

interface operator( +)
   module procedure boolean_plus
end interface

!  boolean unary operator -

public :: operator( -)

interface operator( -)
   module procedure boolean_minus
end interface

! **********************************************************************

!  bitwise logical operators & functions

! **********************************************************************

!  boolean binary operator .and.

public :: operator( .and.)

interface operator( .and.)
   module procedure boolean_and
end interface

!  boolean binary operator .or.

public :: operator( .or.)

interface operator( .or.)
   module procedure boolean_or
end interface

!  boolean binary operator .eor.

public :: operator( .eor.)

interface operator( .eor.)
   module procedure boolean_eor
end interface

!  boolean unary operator .not.

public :: operator( .not.)

interface operator( .not.)
   module procedure boolean_not
end interface

!  boolean function compl( i)

public :: compl

interface compl
   module procedure boolean_compl
end interface

!  boolean function csmg( i, j, k)

public :: csmg

interface csmg
   module procedure boolean_csmg
end interface

! **********************************************************************

!  boolean logical operators

! **********************************************************************

!  boolean binary operator .xor.

public :: operator( .xor.)

interface operator( .xor.)
   module procedure boolean_xor
end interface

!  boolean binary operator .eqv.

public :: operator( .eqv.)

interface operator( .eqv.)
   module procedure boolean_eqv
end interface

!  boolean binary operator .neqv.

public :: operator( .neqv.)

interface operator( .neqv.)
   module procedure boolean_neqv
end interface

! **********************************************************************

!  logical boolean operators: .eq., .ne., .ge., .gt., .le., .lt.

! **********************************************************************

!  boolean binary operator ==

public :: operator( .eq.)

interface operator( .eq.)
   module procedure boolean_eq
end interface

!  boolean binary operator /=

public :: operator( .ne.)

interface operator( .ne.)
   module procedure boolean_ne
end interface

!  boolean binary operator >=

public :: operator( .ge.)

interface operator( .ge.)
   module procedure boolean_ge
end interface

!  boolean binary operator >

public :: operator( .gt.)

interface operator( .gt.)
   module procedure boolean_gt
end interface

!  boolean binary operator <=

public :: operator( .le.)

interface operator( .le.)
   module procedure boolean_le
end interface

!  boolean binary operator <

public :: operator( .lt.)

interface operator( .lt.)
   module procedure boolean_lt
end interface

! **********************************************************************

!  integer arithmetic operators

! **********************************************************************

!  boolean binary operator +

interface operator( +)
   module procedure boolean_add
end interface

!  boolean binary operator -

interface operator( -)
   module procedure boolean_sub
end interface

!  boolean binary operator *

public :: operator( *)

interface operator( *)
   module procedure boolean_mul
end interface

!  boolean binary operator /

public :: operator( /)

interface operator( /)
   module procedure boolean_div
end interface

! **********************************************************************

!  bit level functions

! **********************************************************************

!  boolean function leadz

public :: leadz

interface leadz
   module procedure boolean_leadz
end interface

!  boolean function lastz

public :: lastz

interface lastz
   module procedure boolean_lastz
end interface

!  boolean function popcnt

public :: popcnt

interface popcnt
   module procedure boolean_popcnt
end interface

!  boolean function poppar

public :: poppar

interface poppar
   module procedure boolean_poppar
end interface

!  boolean hamming distance

public :: hamd

interface hamd
   module procedure boolean_hamd
end interface

public :: operator( .hamd.)

interface operator( .hamd.)
   module procedure boolean_hamd
end interface

!  boolean hamming distance

public :: operator( .shift.)

interface operator( .shift.)
   module procedure boolean_shift
end interface

public :: operator( .rotate.)

interface operator( .rotate.)
   module procedure boolean_rotate
end interface

! **********************************************************************

!  mask functions

! **********************************************************************

!  boolean mask

public :: mask

interface mask
   module procedure boolean_mask
end interface

!  boolean maskl

public :: maskl

interface maskl
   module procedure boolean_maskl
end interface

!  boolean maskr

public :: maskr

interface maskr
   module procedure boolean_maskr
end interface

! **********************************************************************

!  shift functions

! **********************************************************************

!  boolean ishft

intrinsic :: ishft

public :: ishft

interface ishft
   module procedure boolean_ishft
end interface

!  boolean ishftc

intrinsic :: ishftc

public :: ishftc

interface ishftc
   module procedure boolean_ishftc
end interface

!  boolean dshftl

public :: dshftl

interface dshftl
   module procedure boolean_dshftl
end interface

!  boolean dshftr

public :: dshftr

interface dshftr
   module procedure boolean_dshftr
end interface

!  boolean dshftc

public :: dshftc

interface dshftc
   module procedure boolean_dshftc
end interface

! **********************************************************************

!  mil std bit functions

! **********************************************************************

!  boolean ibset

public :: operator( .set.)

interface operator( .set.)
   module procedure boolean_ibset
end interface

intrinsic :: ibset

public :: ibset

interface ibset
   module procedure boolean_ibset
end interface

!  boolean ibclr

public :: operator( .clear.)

interface operator( .clear.)
   module procedure boolean_ibclr
end interface

intrinsic :: ibclr

public :: ibclr

interface ibclr
   module procedure boolean_ibclr
end interface

!  boolean btest

public :: operator( .isset.)

interface operator( .isset.)
   module procedure boolean_btest
end interface

intrinsic :: btest

public :: btest

interface btest
   module procedure boolean_btest
end interface

!  boolean isclear

public :: operator( .isclear.)

interface operator( .isclear.)
   module procedure boolean_bztest
end interface

public :: bztest

interface bztest
   module procedure boolean_bztest
end interface

!  boolean mvbits

intrinsic :: mvbits

public :: mvbits

interface mvbits
   module procedure boolean_mvbits
end interface

!  boolean ibits

intrinsic :: ibits

public :: ibits

interface ibits
   module procedure boolean_ibits
end interface

! **********************************************************************

!  boolean rev_endian()

public :: rev_endian

interface rev_endian
   module procedure boolean_rev_endian
end interface

! **********************************************************************

!  boolean bit_size()

intrinsic :: bit_size

public :: bit_size

interface bit_size
   module procedure boolean_bit_size
end interface

! **********************************************************************

!  boolean swap()

public :: swap

interface swap
   module procedure boolean_swap
end interface

! **********************************************************************

!  data

! **********************************************************************

!  mask, maskl, maskr data

integer, dimension( bit_size( 0)), parameter :: left_mask = &
         int( [ z'80000000', z'c0000000', z'e0000000', z'f0000000', &
                z'f8000000', z'fc000000', z'fe000000', z'ff000000', &
                z'ff800000', z'ffc00000', z'ffe00000', z'fff00000', &
                z'fff80000', z'fffc0000', z'fffe0000', z'ffff0000', &
                z'ffff8000', z'ffffc000', z'ffffe000', z'fffff000', &
                z'fffff800', z'fffffc00', z'fffffe00', z'ffffff00', &
                z'ffffff80', z'ffffffc0', z'ffffffe0', z'fffffff0', &
                z'fffffff8', z'fffffffc', z'fffffffe', z'ffffffff'])

integer, dimension( bit_size( 0)), parameter :: right_mask = &
         int( [ z'00000001', z'00000003', z'00000007', z'0000000f', &
                z'0000001f', z'0000003f', z'0000007f', z'000000ff', &
                z'000001ff', z'000003ff', z'000007ff', z'00000fff', &
                z'00001fff', z'00003fff', z'00007fff', z'0000ffff', &
                z'0001ffff', z'0003ffff', z'0007ffff', z'000fffff', &
                z'001fffff', z'003fffff', z'007fffff', z'00ffffff', &
                z'01ffffff', z'03ffffff', z'07ffffff', z'0fffffff', &
                z'1fffffff', z'3fffffff', z'7fffffff', z'ffffffff'])

!  leadz, lastz, popcnt, poppar data

integer, parameter :: lead_probe16 = int( z'ffff0000')
integer, parameter :: lead_probe8 = int( z'ff00ff00')
integer, parameter :: lead_probe4 = int( z'f0f0f0f0')
integer, parameter :: lead_probe2 = int( z'cccccccc')
integer, parameter :: lead_probe1 = int( z'aaaaaaaa')

integer, parameter :: last_probe16 = int( z'0000ffff')
integer, parameter :: last_probe8 = int( z'00ff00ff')
integer, parameter :: last_probe4 = int( z'0f0f0f0f')
integer, parameter :: last_probe2 = int( z'33333333')
integer, parameter :: last_probe1 = int( z'55555555')

integer, parameter :: p1 = int( z'11111111')
integer, parameter :: p2 = int( z'22222222')
integer, parameter :: p4 = int( z'44444444')
integer, parameter :: p8 = int( z'88888888')

integer, parameter :: hi_nibble = int( z'f0f0f0f0')
integer, parameter :: lo_nibble = int( z'0f0f0f0f')

integer, parameter :: low_byte = int( z'000000ff')

integer, parameter :: low_bit = int( z'00000001')

! *********************************************************************

!  library

! **********************************************************************

contains

! **********************************************************************

!  assignment between boolean <--> other csu_per_nsu byte types

! **********************************************************************

!  integer = boolean

elemental subroutine b_to_i( i, b)

type( boolean_t), intent( in) :: b

integer, intent( out) :: i

!  b_to_i()

continue

   i = transfer( b, i)

return

!  b_to_i()

end subroutine b_to_i

! **********************************************************************

!  boolean = integer

elemental subroutine i_to_b( b, i)

integer, intent( in) :: i

type( boolean_t), intent( out) :: b

!  i_to_b()

continue

   b = transfer( i, b)

return

!  i_to_b()

end subroutine i_to_b

! **********************************************************************

!  real = boolean

elemental subroutine b_to_r( r, b)

type( boolean_t), intent( in) :: b

real, intent( out) :: r

!  begin

continue

   r = transfer( b, r)

return

!  b_to_r()

end subroutine b_to_r

! **********************************************************************

!  boolean = real

elemental subroutine r_to_b( b, r)

real, intent( in) :: r

type( boolean_t), intent( out) :: b

!  r_to_b()

continue

   b = transfer( r, b)

return

!  r_to_b()

end subroutine r_to_b

! **********************************************************************

!  logical = boolean

elemental subroutine b_to_l( l, b)

type( boolean_t), intent( in) :: b

logical, intent( out) :: l

!  b_to_l

continue

   l = transfer( b, l)

return

!  b_to_l()

end subroutine b_to_l

! **********************************************************************

!  boolean = logical

elemental subroutine l_to_b( b, l)

logical, intent( in) :: l

type( boolean_t), intent( out) :: b

!  l_to_b()

continue

   b = transfer( l, b)

return

!  l_to_b()

end subroutine l_to_b

! **********************************************************************

!  character*4 = boolean

elemental subroutine b_to_c( c, b)

type( boolean_t), intent( in) :: b

character( len= csu_per_nsu), intent( out) :: c

!  b_to_c()

continue

   c = transfer( b, c)

return

!  b_to_c()

end subroutine b_to_c

! **********************************************************************

!  boolean = character*4

elemental subroutine c_to_b( b, c)

character( len= csu_per_nsu), intent( in) :: c

type( boolean_t), intent( out) :: b

!  c_to_b()

continue

   b = transfer( c, b)

return

!  c_to_b()

end subroutine c_to_b

! **********************************************************************

!  explicit conversion to/from boolean

! **********************************************************************

!  boolean = bool( integer)

elemental type( boolean_t) function int_bool( i)

integer, intent( in) :: i

!  int_bool()

continue

   int_bool = transfer( i, int_bool)

return

!  int_bool()

end function int_bool

! **********************************************************************

!  boolean = bool( real)

elemental type( boolean_t) function single_bool( r)

real, intent( in) :: r

!  single_bool()

continue

   single_bool = transfer( r, single_bool)

return

!  single_bool()

end function single_bool

! **********************************************************************

!  boolean = bool( logical)

elemental type( boolean_t) function logical_bool( l)

logical, intent( in) :: l

!  logical_bool()

continue

   logical_bool = transfer( l, logical_bool)

return

!  logical_bool()

end function logical_bool

! **********************************************************************

!  boolean = bool( character*csu_per_nsu)

elemental type( boolean_t) function char_bool( c)

character( len= csu_per_nsu), intent( in) :: c

!  char_bool()

continue

   char_bool = transfer( c, char_bool)

return

!  char_bool()

end function char_bool

! **********************************************************************

!  int = int( boolean)

elemental integer function boolean_int( b)

type( boolean_t), intent( in) :: b

!  boolean_int

continue

   boolean_int = transfer( b, boolean_int)

return

!  boolean_int()

end function boolean_int

! **********************************************************************

!  single = real( boolean)

elemental real function boolean_real( b)

type( boolean_t), intent( in) :: b

!  boolean_real()

continue

   boolean_real = transfer( b, boolean_real)

return

!  boolean_real()

end function boolean_real

! **********************************************************************

!  speed = logical( boolean)

elemental logical function boolean_logical( b)

type( boolean_t), intent( in) :: b

!  boolean_logical()

continue

   boolean_logical = transfer( b, boolean_logical)

return

!  boolean_logical()

end function boolean_logical

! **********************************************************************

!  character*csu_per_nsu = char( boolean)

elemental character( len= csu_per_nsu) function boolean_char( b)

type( boolean_t), intent( in) :: b

!  boolean_char()

continue

   boolean_char = transfer( b, boolean_char)

return

!  boolean_char()

end function boolean_char

! **********************************************************************

!  unary operators: +, -

! **********************************************************************

!  boolean_plus(): +b

elemental type( boolean_t) function boolean_plus( b)

type( boolean_t), intent( in) :: b

!  boolean_plus()

continue

   boolean_plus% bits = +b% bits

return

!  boolean_plus()

end function boolean_plus

! **********************************************************************

!  boolean_minus(): -b

elemental type( boolean_t) function boolean_minus( b)

type( boolean_t), intent( in) :: b

!  boolean_minus()

continue

   boolean_minus% bits = -b% bits

return

!  boolean_minus()

end function boolean_minus

! **********************************************************************

!  operators and functions: .and., .or., .eor., .not., compl(), csmg()

! **********************************************************************

!  b1 .and. b2

elemental type( boolean_t) function boolean_and( b1, b2)

type( boolean_t), intent( in) :: b1, b2

!  boolean_and()

continue

   boolean_and% bits = iand( b1% bits, b2% bits)

return

!  boolean_and()

end function boolean_and

! **********************************************************************

!  b1 .or. b2

elemental type( boolean_t) function boolean_or( b1, b2)

type( boolean_t), intent( in) :: b1, b2

!  boolean_or()

continue

   boolean_or% bits = ior( b1% bits, b2% bits)

return

!  boolean_or()

end function boolean_or

! **********************************************************************

!  b1 .eor. b2

elemental type( boolean_t) function boolean_eor( b1, b2)

type( boolean_t), intent( in) :: b1, b2

!  boolean_eor()

continue

   boolean_eor% bits = ieor( b1% bits, b2% bits)

return

!  boolean_eor()

end function boolean_eor

! **********************************************************************

!  .not. b

elemental type( boolean_t) function boolean_not( b)

type( boolean_t), intent( in) :: b

!  boolean_not()

continue

   boolean_not% bits = not( b% bits)

return

!  boolean_not()

end function boolean_not

! **********************************************************************

!  boolean = compl( boolean)

elemental type( boolean_t) function boolean_compl( i)

type( boolean_t), intent( in) :: i

!  boolean_compl()

continue

   boolean_compl% bits = not( i% bits)

return

!  boolean_compl()

end function boolean_compl

! **********************************************************************

!  boolean = csmg( boolean, boolean, boolean)

elemental type( boolean_t) function boolean_csmg( i, j, k)

type( boolean_t), intent( in) :: i, j, k

!  boolean_csmg()

continue

   boolean_csmg% bits = ior( iand( i% bits, k% bits), iand( j% bits, not( k% bits)) )

return

!  boolean_csmg()

end function boolean_csmg

! **********************************************************************

!  bit-wise operators: .xor., .eqv., .neqv.

! **********************************************************************

!  b1 .xor. b2

elemental type( boolean_t) function boolean_xor( b1, b2)

type( boolean_t), intent( in) :: b1, b2

!  boolean_xor()

continue

   boolean_xor% bits = ieor( b1% bits, b2% bits)

return

!  boolean_xor()

end function boolean_xor

! **********************************************************************

!  b1 .eqv. b2

elemental type( boolean_t) function boolean_eqv( b1, b2)

type( boolean_t), intent( in) :: b1, b2

!  boolean_eqv()

continue

   boolean_eqv% bits = not( ieor( b1% bits, b2% bits) )

return

!  boolean_eqv()

end function boolean_eqv

! **********************************************************************

!  b1 .neqv. b2

elemental type( boolean_t) function boolean_neqv( b1, b2)

type( boolean_t), intent( in) :: b1, b2

!  boolean_neqv()

continue

   boolean_neqv% bits = ieor( b1% bits, b2% bits)

return

!  boolean_neqv()

end function boolean_neqv

! **********************************************************************

!  logical operators: .eq., .ne., .ge., .gt., .le., .lt.

! **********************************************************************

!  b1 == b2

elemental logical function boolean_eq( b1, b2)

type( boolean_t), intent( in) :: b1, b2

!  boolean_eq()

continue

   boolean_eq = b1% bits == b2% bits

return

!  boolean_eq()

end function boolean_eq

! **********************************************************************

!  b1 /= b2

elemental logical function boolean_ne( b1, b2)

type( boolean_t), intent( in) :: b1, b2

!  boolean_ne()

continue

   boolean_ne = b1% bits /= b2% bits

return

!  boolean_ne()

end function boolean_ne

! **********************************************************************

!  b1 >= b2

elemental logical function boolean_ge( b1, b2)

type( boolean_t), intent( in) :: b1, b2

!  boolean_ge()

continue

   boolean_ge = b1% bits >= b2% bits

return

!  boolean_ge()

end function boolean_ge

! **********************************************************************

!  b1 > b2

elemental logical function boolean_gt( b1, b2)

type( boolean_t), intent( in) :: b1, b2

!  boolean_gt()

continue

   boolean_gt = b1% bits > b2% bits

return

!  boolean_gt()

end function boolean_gt

! **********************************************************************

!  b1 <= b2

elemental logical function boolean_le( b1, b2)

type( boolean_t), intent( in) :: b1, b2

!  boolean_le()

continue

   boolean_le = b1% bits <= b2% bits

return

!  boolean_le()

end function boolean_le

! **********************************************************************

!  b1 < b2

elemental logical function boolean_lt( b1, b2)

type( boolean_t), intent( in) :: b1, b2

!  boolean_lt()

continue

   boolean_lt = b1% bits < b2% bits

return

!  boolean_lt()

end function boolean_lt

! **********************************************************************

!  operators: +, -, *, /

! **********************************************************************

!  b1 + b2

elemental type( boolean_t) function boolean_add( b1, b2)

type( boolean_t), intent( in) :: b1, b2

!  boolean_add()

continue

   boolean_add% bits = b1% bits + b2% bits

return

!  boolean_add()

end function boolean_add

! **********************************************************************

!  b1 - b2

elemental type( boolean_t) function boolean_sub( b1, b2)

type( boolean_t), intent( in) :: b1, b2

!  boolean_sub()

continue

   boolean_sub% bits = b1% bits - b2% bits

return

!  boolean_sub()

end function boolean_sub

! **********************************************************************

!  b1 * b2

elemental type( boolean_t) function boolean_mul( b1, b2)

type( boolean_t), intent( in) :: b1, b2

!  boolean_mul()

continue

   boolean_mul% bits = b1% bits * b2% bits

return

!  boolean_mul()

end function boolean_mul

! **********************************************************************

!  b1 / b2

elemental type( boolean_t) function boolean_div( b1, b2)

type( boolean_t), intent( in) :: b1, b2

!  boolean_div()

continue

   boolean_div% bits = b1% bits / b2% bits

return

!  boolean_div()

end function boolean_div

! **********************************************************************

!  bit counts: leadz(), lastz(), popcnt(), poppar()

! **********************************************************************

!  leadz( b)

elemental integer function boolean_leadz( b)

type( boolean_t), intent( in) :: b

!  scratch data and masks

   integer :: test, at_least

!  boolean_leadz()

continue

   test = b% bits

   if( test == 0 )then
      boolean_leadz = bit_size( 0)
      return
   endif

   if( iand( lead_probe16, test) == 0 )then
      at_least = 16
   else
      at_least = 0
      test = iand( lead_probe16, test)
   endif

   if( iand( lead_probe8, test) == 0 )then
      at_least = at_least + 8
   else
      test = iand( lead_probe8, test)
   endif

   if( iand( lead_probe4, test) == 0 )then
      at_least = at_least + 4
   else
      test = iand( lead_probe4, test)
   endif

   if( iand( lead_probe2, test) == 0 )then
      at_least = at_least + 2
   else
      test = iand( lead_probe2, test)
   endif

   if( iand( lead_probe1, test) == 0 )then
      at_least = at_least + 1
   endif

   boolean_leadz = at_least

return

!  boolean_leadz()

end function boolean_leadz

! **********************************************************************

!  lastz( b)

elemental integer function boolean_lastz( b)

type( boolean_t), intent( in) :: b

!  scratch data and masks

   integer :: test, at_least

!  boolean_lastz()

continue

   test = b% bits

   zero_arg: if( test == 0 )then

      boolean_lastz = bit_size( 0)
      return

   endif zero_arg

   if( iand( last_probe16, test) == 0 )then
      at_least = 16
   else
      at_least = 0
      test = iand( last_probe16, test)
   endif

   if( iand( last_probe8, test) == 0 )then
      at_least = at_least + 8
   else
      test = iand( last_probe8, test)
   endif

   if( iand( last_probe4, test) == 0 )then
      at_least = at_least + 4
   else
      test = iand( last_probe4, test)
   endif

   if( iand( last_probe2, test) == 0 )then
      at_least = at_least + 2
   else
      test = iand( last_probe2, test)
   endif

   if( iand( last_probe1, test) == 0 )then
      at_least = at_least + 1
   endif

   boolean_lastz = at_least

return

!  boolean_lastz()

end function boolean_lastz

! **********************************************************************

!  popcnt( b)

elemental integer function boolean_popcnt( b)

type( boolean_t), intent( in) :: b

!  scratch data and masks

   integer :: test, t1, t2, t4, t8

!  boolean_popcnt()

continue

   test = b% bits

   t1 = iand( test, p1)
   t2 = iand( test, p2)
   t4 = iand( test, p4)
   t8 = iand( test, p8)

   test = t1 + ishft( t2, -1) + ishft( t4, -2) + ishft( t8, -3)

! each nibble now contains [ 0, 1, 2, 3]

   t1 = iand( test, hi_nibble)
   t2 = iand( test, lo_nibble)

! add each of 4 high nibbles with each of 4 low nibbles

   test = iand( ishft( t1, -4) + t2, lo_nibble)

! add each of 4 bytes & mask off low byte

   test = test + ishft( test, -8) + ishft( test, -16) + ishft( test, -24)

   boolean_popcnt = iand( test, low_byte)

return

!  boolean_popcnt()

end function boolean_popcnt

! **********************************************************************

!  poppar( b)

elemental integer function boolean_poppar( b)

type( boolean_t), intent( in) :: b

!  local data

   integer :: test, t1, t2, t4, t8

!  boolean_poppar()

continue

   test = b% bits

   t1 = iand( test, p1)
   t2 = iand( test, p2)
   t4 = iand( test, p4)
   t8 = iand( test, p8)

   test = t1 + ishft( t2, -1) + ishft( t4, -2) + ishft( t8, -3)

   t1 = iand( test, hi_nibble)
   t2 = iand( test, lo_nibble)
   test = iand( ishft( t1, -4) + t2, lo_nibble)
   test = test + ishft( test, -8) + ishft( test, -16) + ishft( test, -24)

   boolean_poppar = iand( test, low_bit)

return

!  boolean_poppar()

end function boolean_poppar

! **********************************************************************

!  .hamd. hamming distance

elemental integer function boolean_hamd( b1, b2)

type( boolean_t), intent( in) :: b1, b2

!  boolean_hamd()

continue

   boolean_hamd = popcnt( bool( ieor( b1% bits, b2% bits)))

return

!  boolean_hamd()

end function boolean_hamd

! **********************************************************************

!  .shift. shift operator

elemental integer function boolean_shift( b, i)

type( boolean_t), intent( in) :: b

integer, intent( in) :: i

!  boolean_shift()

continue

   boolean_shift = ishft( b% bits, i)

return

!  boolean_shift()

end function boolean_shift

! **********************************************************************

!  .rotate. rotate operator

elemental integer function boolean_rotate( b, i)

type( boolean_t), intent( in) :: b

integer, intent( in) :: i

!  boolean_rotate()

continue

   boolean_rotate = ishftc( b% bits, i)

return

!  boolean_rotate()

end function boolean_rotate

! **********************************************************************

!  masks: mask(), maskl(), maskr()

! **********************************************************************

!  mask( i)

elemental type( boolean_t) function boolean_mask( i)

integer, intent( in) :: i

!  boolean_mask() local

   integer, parameter :: bs = bit_size( i)

!  boolean_mask()

continue

   bits: select case( i)

   case( 1: bs) bits

      boolean_mask% bits = right_mask( i)

   case( -bs: -1) bits

      boolean_mask% bits = left_mask( abs( i))

   case default bits

      boolean_mask% bits = 0

   end select bits

return

!  boolean_mask()

end function boolean_mask

! **********************************************************************

!  maskl( i)

elemental type( boolean_t) function boolean_maskl( i)

integer, intent( in) :: i

!  boolean_maskl() local

   integer, parameter :: bs = bit_size( i)

!  boolean_maskl()

continue

   bits: select case( i)

   case( 1: bs) bits

      boolean_maskl% bits = left_mask( i)

   case default bits

      boolean_maskl% bits = 0

   end select bits

return

!  boolean_maskl()

end function boolean_maskl

! **********************************************************************

!  boolean_maskr( i)

elemental type( boolean_t) function boolean_maskr( i)

integer, intent( in) :: i

!  boolean_maskr() local

   integer, parameter :: bs = bit_size( i)

!  boolean_maskr()

continue

   bits: select case( i)

   case( 1: bs) bits

      boolean_maskr% bits = right_mask( i)

   case default bits

      boolean_maskr% bits = 0

   end select bits

return

!  boolean_maskr()

end function boolean_maskr

! **********************************************************************

!  shifts: ishft(), ishftc(), dshftl(), dshftr(), dshftc()

! **********************************************************************

!  ishft( b, i)

elemental type( boolean_t) function boolean_ishft( b, i)

type( boolean_t), intent( in) :: b

integer, intent( in) :: i

!  boolean_ishft()

continue

   boolean_ishft% bits = ishft( b% bits, i)

return

!  boolean_ishft()

end function boolean_ishft

! **********************************************************************

!  ishftc( b, i1, i2)

elemental type( boolean_t) function boolean_ishftc( b, i1, i2)

type( boolean_t), intent( in) :: b

integer, intent( in) :: i1, i2

!  boolean_ishftc()

continue

   boolean_ishftc% bits = ishftc( b% bits, i1, i2)

return

!  boolean_ishftc()

end function boolean_ishftc

! **********************************************************************

!  dshftl( bl, br, i)

elemental type( boolean_t) function boolean_dshftl( bl, br, i)

type( boolean_t), intent( in) :: bl, br

integer, intent( in) :: i

!  local data

   type( boolean_t) :: btl, btr

!  boolean_dshftl()

continue

!  trap out endcase

   shift_zero: if( i < 0 )then

      boolean_dshftl% bits = 0
      return

   elseif( i == 0 )then shift_zero

      boolean_dshftl% bits = bl% bits
      return

   endif shift_zero

   shift_nonzero: if( i < bit_size( i) )then

      btl% bits = ishft( bl% bits, i)
      btr% bits = ishft( br% bits, i - bit_size( i))

      boolean_dshftl% bits = ior( btl% bits, btr% bits)

   elseif( i == bit_size( i) )then shift_nonzero

      boolean_dshftl% bits = br% bits

   else shift_nonzero

      boolean_dshftl% bits = 0

   endif shift_nonzero

return

!  boolean_dshftl()

end function boolean_dshftl

! **********************************************************************

!  dshftr( bl, br, i)

elemental type( boolean_t) function boolean_dshftr( bl, br, i)

type( boolean_t), intent( in) :: bl, br

integer, intent( in) :: i

!  local data

   type( boolean_t) :: btl, btr

!  boolean_dshftr()

continue

!  trap out endcase

   shift_zero: if( i < 0 )then

      boolean_dshftr% bits = 0
      return

   elseif( i == 0 )then shift_zero

      boolean_dshftr% bits = br% bits
      return

   endif shift_zero

   shift_nonzero: if( i < bit_size( i) )then

      btl% bits = ishft( bl% bits, bit_size( i) - i)
      btr% bits = ishft( br% bits, -i)

      boolean_dshftr% bits = ior( btl% bits, btr% bits)

   elseif( i == bit_size( i) )then shift_nonzero

      boolean_dshftr% bits = bl% bits

   else shift_nonzero

      boolean_dshftr% bits = 0

   endif shift_nonzero

return

!  boolean_dshftr()

end function boolean_dshftr

! **********************************************************************

!  dshftc( bl, br, i)

elemental subroutine boolean_dshftc( bl, br, i)

type( boolean_t), intent( inout) :: bl, br

integer, intent( in) :: i

!  local data

   type( boolean_t) :: btl, btr, carryl, carryr

   integer :: ia

!  boolean_dshftc()

continue

   ia = abs( i)

   shift_off: if( ia > bit_size( i) )then

      return

   endif shift_off

   shift_sign: if( ia == bit_size( i) )then

      btl% bits = br% bits
      btr% bits = bl% bits

      bl% bits = btl% bits
      br% bits = btr% bits

   elseif( i<bit_size( i) .and. i>=1 )then shift_sign

      carryl% bits = ishft( bl% bits, i - bit_size( i))
      carryr% bits = ishft( br% bits, i - bit_size( i))

      btl% bits = ishft( bl% bits, i)
      btr% bits = ishft( br% bits, i)

      bl% bits = ior( btl% bits, carryr% bits)
      br% bits = ior( btr% bits, carryl% bits)

   elseif( ia<bit_size( i) .and. ia>=1 )then shift_sign

      carryl% bits = ishft( bl% bits, bit_size( i) + i)
      carryr% bits = ishft( br% bits, bit_size( i) + i)

      btl% bits = ishft( bl% bits, i)
      btr% bits = ishft( br% bits, i)

      bl% bits = ior( btl% bits, carryr% bits)
      br% bits = ior( btr% bits, carryl% bits)

   endif shift_sign

return

!  boolean_dshftc()

end subroutine boolean_dshftc

! **********************************************************************

!  mil std bit functions

! **********************************************************************

!  boolean ibset

elemental type( boolean_t) function boolean_ibset(b, i)

type( boolean_t), intent( in) :: b

integer, intent( in) :: i

!  boolean_ibset()

continue

   boolean_ibset% bits = ibset( b% bits, i)

return

!  boolean_ibset()

end function boolean_ibset

! **********************************************************************

!  boolean ibclr

elemental type( boolean_t) function boolean_ibclr(b, i)

type( boolean_t), intent( in) :: b

integer, intent( in) :: i

!  boolean_ibclr()

continue

   boolean_ibclr% bits = ibclr( b% bits, i)

return

!  boolean_ibclr()

end function boolean_ibclr

! **********************************************************************

!  boolean btest

elemental logical function boolean_btest(b, i)

type( boolean_t), intent( in) :: b

integer, intent( in) :: i

!  boolean_btest()

continue

   boolean_btest = btest( b% bits, i)

return

!  boolean_btest()

end function boolean_btest

! **********************************************************************

!  boolean bztest

elemental logical function boolean_bztest(b, i)

type( boolean_t), intent( in) :: b

integer, intent( in) :: i

!  boolean_bztest() local

   integer, parameter :: bit = 1

!  boolean_bztest()

continue

   boolean_bztest = iand( ishft( bit, i), b% bits) == 0

return

!  boolean_bztest()

end function boolean_bztest

! **********************************************************************

!  boolean mvbits

elemental subroutine boolean_mvbits(b1, i, l, b2, j)

type( boolean_t), intent( in) :: b1

integer, intent( in) :: i, l, j

type( boolean_t), intent( out) :: b2

!  boolean_mvbits()

continue

   call mvbits( b1% bits, i, l, b2% bits, j)

return

!  boolean_mvbits()

end subroutinÑÀdñ  %,+$*$%ÑÀdð  %,+$+-'ÑÀdÿ  %,+$+,+ÑÀdþ  %,+$+,'ÑÀdý  %,+$+++ÑÀdü  %,+$+''ÑÀdû  %,+$+&'ÑÀdú  %,+$+%+ÑÀdù  %,+$+%%ÑÀdø  %,+$+$-ÑÀd÷  %,+$,+-ÑÀe  %,+$,*%ÑÀe  %,+$,)-ÑÀe  %,+$,)'ÑÀe  %,+$,&-ÑÀe  %,+$,$+ÑÀe   %,+$--%ÑÀe  %,+$-*%ÑÀe  %,+$-)%ÑÀe
  %,+$-''ÑÀe	  %,+$-&+ÑÀe  %,+$-%-ÑÀe  %,+$-$+ÑÀe  %,+%¥Ä  %,+%Ý	• %,+%$--ÑÀe  %,+%$,'ÑÀe  %,+%$,%ÑÀe  %,+%$)+ÑÀe  %,+%$'-ÑÀe  %,+%$&-ÑÀe  %,+%$&%ÑÀe  %,+%$%+ÑÀe  %,+%%,'ÑÀe  %,+%%+%ÑÀe  %,+%%)'ÑÀe  %,+%%(+ÑÀe  %,+%%'+ÑÀe  %,+%%%'ÑÀe  %,+%&-'ÑÀe  %,+%&+-ÑÀe  %,+%&*'ÑÀe  %,+%&(-ÑÀe  %,+%&%'ÑÀe  %,+%','ÑÀe$  %,+%')%ÑÀe#  %,+%''-ÑÀe"  %,+%'&+ÑÀe!  %,+%'&%ÑÀe   %,+%(-%ÑÀe.  %,+%(++ÑÀe-  %,+%(+'ÑÀe,  %,+%(*%ÑÀe+  %,+%()-ÑÀe*  %,+%((-ÑÀe)  %,+%((+ÑÀe(  %,+%('+ÑÀe'  %,+%(%+ÑÀe&  %,+%(%'ÑÀe%  %,+%)-%ÑÀe5  %,+%)*%ÑÀe4  %,+%)(-ÑÀe3  %,+%)('ÑÀe2  %,+%)'%ÑÀe1  %,+%)$-ÑÀe0  %,+%)$'ÑÀe/  %,+%*--ÑÀe?  %,+%*-'ÑÀe>  %,+%*++ÑÀe=  %,+%**-ÑÀe<  %,+%*)%ÑÀe;  %,+%*(%ÑÀe:  %,+%*&-ÑÀe9  %,+%*&+ÑÀe8  %,+%*&%ÑÀe7  %,+%*$'ÑÀe6  %,+%+,-ÑÀeE  %,+%+,'ÑÀeD  %,+%+++ÑÀeC  %,+%+('ÑÀeB  %,+%+%'ÑÀeA  %,+%+%%ÑÀe@  %,+%,+-ÑÀeK  %,+%,)%ÑÀeJ  %,+%,('ÑÀeI  %,+%,'-ÑÀeH  %,+%,&+ÑÀeG  %,+%,%'ÑÀeF  %,+%-,'ÑÀeS  %,+%-,%ÑÀeR  %,+%-)+ÑÀeQ  %,+%-)%ÑÀeP  %,+%-&-ÑÀeO  %,+%-&+ÑÀeN  %,+%-&'ÑÀeM  %,+%-%+ÑÀeL  %,+&¤	Ä#z, ‚G*7€º*  %,+&$-+ÑÀeX  %,+&$(-ÑÀeW  %,+&$('ÑÀeV  %,+&$$+ÑÀeU  %,+&$$%ÑÀeT  %,+&%+'ÑÀe]  %,+&%(-ÑÀe\  %,+&%'+ÑÀe[  %,+&%%'ÑÀeZ  %,+&%$-ÑÀeY  %,+&&,-ÑÀef  %,+&&,+ÑÀee  %,+&&,%ÑÀed  %,+&&+%ÑÀec  %,+&&)-ÑÀeb  %,+&&)'ÑÀea  %,+&&'-ÑÀe`  %,+&&&-ÑÀe_  %,+&&%+ÑÀe^  %,+&',-ÑÀel  %,+&''+ÑÀek  %,+&'&'ÑÀej  %,+&'%-ÑÀei  %,+&'%'ÑÀeh  %,+&'$%ÑÀeg  %,+&(-%