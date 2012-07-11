! bof
! **********************************************************************
! Fortran 95 module standard_functions

! **********************************************************************
! Source Control Strings

! $Id: stdfunc.fpp 1.3 2003/10/03 19:41:32Z Dan Release $

! **********************************************************************
! Copyright 2000 Purple Sage Computing Solutions, Inc.

! **********************************************************************
! adds a basic set of operators and functions to Fortran 95 programs

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
!    Purple Sage Computing Solutions, Inc.
!                               send email to dnagle@erols.com
!                                   or fax to 703 471 0684 (USA)
!                                  or mail to 12142 Purple Sage Ct.
!                                             Reston, VA 20194-5621 USA

! **********************************************************************

!  standard_functions constants

!     error_short_string integer encode() or decode() passed null or short string
!     error_bad_base integer encode() or decode()  base larger than translation table
!     error_not_in_table integer decode() input character not in translation table

!  standard_functions operators

!     .xor. binary logicals

!     .mod. binary integers, reals
!     .modulo. binary integers, reals

!     .gcd. binary integers
!     .lcm. binary integers

!     .cd. binary integers
!     .cr. binary integers

!  standard_functions library

!     iseven() logical( integer)
!     isodd() logical( integer)

!     gcd() greatest common divisor integer( integer, integer)
!     lcm() least common multiple integer( integer, integer)

!     cd() ceiling division [ ( j+ k- 1) /k ] integer( integer, integer)
!     cr() ceiling remainder [ j- k* cd() ] integer( integer, integer)

!     rrint() round up with probability ( a - int( a)) or down integer( real)

!     hex() string to integer
!     oct() string to integer
!     bin() string to integer

!     hexstr() integer to string
!     octstr() integer to string
!     binstr() integer to string

!     encode() integer to string, arbitrary base, via translate table
!     decode() string to integer, arbitrary base, via translate table

!     compl() for logicals

!     ismax()
!     ismin()
!     isamax()
!     isamin() for reals
!     icamax()
!     icamin() for complex

!     smach() machine constants
!     cmach()

!     pause() subroutine pause() | pause( char*(*)) | pause( integer)
!     stop() subroutine stop() | stop( char*(*)) | stop( integer)

!     swap() for all tk
!     rev_endian() for all tk size > 1 byte
!     rev_bits() for all tk size = 1 byte

!     get_logical_unit() a logical i/o unit number which may be opened

!?>?? ! *******************************************************************

!?>?? ! preprocessor dependencies

!?>?? include 'coco.inc'
!?>??! include coco.inc
!?>?? logical, parameter :: byte_k = .true.
!?>?? logical, parameter :: short_k = .true.
!?>?? logical, parameter :: int_k = .true.
!?>?? logical, parameter :: long_k = .true.
!?>?? logical, parameter :: l_byte_k = .true.
!?>?? logical, parameter :: l_short_k = .true.
!?>?? logical, parameter :: l_int_k = .true.
!?>?? logical, parameter :: l_long_k = .true.
!?>?? logical, parameter :: single_k = .true.
!?>?? logical, parameter :: double_k = .true.
!?>?? logical, parameter :: quad_k = .true.
!?>?? logical, parameter :: ascii_k = .true.
!?>?? logical, parameter :: ebcdic_k = .false.
!?>??! end include coco.inc

!?>?? ! *******************************************************************

! **********************************************************************

module standard_functions

! **********************************************************************

! use kind parameters

use, intrinsic :: iso_fortran_env, only: byte_k => int8, short_k => int16, int_k => int32, long_k => int64, &
                                         single_k => real32, double_k => real64, quad_k => real128, &
                                         numeric_storage_size, character_storage_size

! **********************************************************************

! explicit declaration

implicit none

! **********************************************************************

! explicit export

private

! **********************************************************************

!  RCS strings

! **********************************************************************

character( len= *), public, parameter :: standard_functions_rcs_id = &
   '$Id: stdfunc.fpp 1.3 2003/10/03 19:41:32Z Dan Release $'

! **********************************************************************

!  standard_functions constants

integer, parameter :: l_byte_k = byte_k
integer, parameter :: l_short_k = short_k
integer, parameter :: l_int_k = int_k
integer, parameter :: l_long_k = long_k

integer, parameter :: ascii_k = 1

integer, parameter :: csu_per_nsu = numeric_storage_size / character_storage_size

! **********************************************************************

!  encode()/decode() error codes

integer, public, parameter :: error_short_string = 1
integer, public, parameter :: error_bad_base = 2
integer, public, parameter :: error_not_in_table = 3

!  hex()/oct()/bin() string lengths ( note need for ceiling division! )

integer, public, parameter :: hexstr_len = bit_size( 0) / 4
integer, public, parameter :: octstr_len = bit_size( 0)/3 + 1
integer, public, parameter :: binstr_len = bit_size( 0)

! **********************************************************************

!  pause()/stop() formats

character( len= *), parameter :: ps_fmt_a = '( a)'
character( len= *), parameter :: ps_fmt_ai = '( a, i5.5)'
character( len= *), parameter :: ps_fmt_aa = '( a, a)'

!?>?? if( byte_k )then
! **********************************************************************

!  rev_bits masks and shift counts

integer( kind= byte_k) :: bit_1 ; data bit_1 / z'01'/

integer( kind= byte_k) :: bit_2 ; data bit_2 / z'02'/

integer( kind= byte_k) :: bit_3 ; data bit_3 / z'04'/

integer( kind= byte_k) :: bit_4 ; data bit_4 / z'08'/

integer( kind= byte_k) :: bit_5 ; data bit_5 / z'10'/

integer( kind= byte_k) :: bit_6 ; data bit_6 / z'20'/

integer( kind= byte_k) :: bit_7 ; data bit_7 / z'40'/

integer( kind= byte_k) :: bit_8 ; data bit_8 / z'80'/

integer( kind= byte_k), parameter :: sh_1 = 7
integer( kind= byte_k), parameter :: sh_2 = 5
integer( kind= byte_k), parameter :: sh_3 = 3
integer( kind= byte_k), parameter :: sh_4 = 1
integer( kind= byte_k), parameter :: sh_5 = -1
integer( kind= byte_k), parameter :: sh_6 = -3
integer( kind= byte_k), parameter :: sh_7 = -5
integer( kind= byte_k), parameter :: sh_8 = -7

!?>?? endif
! **********************************************************************

!  standard_functions library

! **********************************************************************

!  declare specific functions implementing the .xor. operator

public :: operator( .xor.)

interface operator( .xor.)
!?>?? if( l_byte_k )then
   module procedure l_byte_xor
!?>?? endif
!?>?? if( l_short_k )then
   module procedure l_short_xor
!?>?? endif
!?>?? if( l_int_k )then
   module procedure l_int_xor
!?>?? endif
!?>?? if( l_long_k )then
   module procedure l_long_xor
!?>?? endif
end interface

! **********************************************************************

!  declare specific functions implementing the .mod. operator

public :: operator( .mod.)

interface operator( .mod.)
!?>?? if( byte_k )then
   module procedure byte_mod
!?>?? endif
!?>?? if( short_k )then
   module procedure short_mod
!?>?? endif
!?>?? if( int_k )then
   module procedure int_mod
!?>?? endif
!?>?? if( long_k )then
   module procedure long_mod
!?>?? endif
!?>?? if( single_k )then
   module procedure single_mod
!?>?? endif
!?>?? if( double_k )then
   module procedure double_mod
!?>?? endif
!?>?? if( quad_k )then
   module procedure quad_mod
!?>?? endif
end interface

!  declare specific functions implementing the .modulo. operator

public :: operator( .modulo.)

interface operator( .modulo.)
!?>?? if( byte_k )then
   module procedure byte_modulo
!?>?? endif
!?>?? if( short_k )then
   module procedure short_modulo
!?>?? endif
!?>?? if( int_k )then
   module procedure int_modulo
!?>?? endif
!?>?? if( long_k )then
   module procedure long_modulo
!?>?? endif
!?>?? if( single_k )then
   module procedure single_modulo
!?>?? endif
!?>?? if( double_k )then
   module procedure double_modulo
!?>?? endif
!?>?? if( quad_k )then
   module procedure quad_modulo
!?>?? endif
end interface

! **********************************************************************

!  declare specific functions implementing the .gcd. operator

public :: operator( .gcd.)

interface operator( .gcd.)
!?>?? if( byte_k )then
   module procedure byte_gcd
!?>?? endif
!?>?? if( short_k )then
   module procedure short_gcd
!?>?? endif
!?>?? if( int_k )then
   module procedure int_gcd
!?>?? endif
!?>?? if( long_k )then
   module procedure long_gcd
!?>?? endif
end interface

!  declare specific functions implementing the gcd() function

public :: gcd

interface gcd
!?>?? if( byte_k )then
   module procedure byte_gcd
!?>?? endif
!?>?? if( short_k )then
   module procedure short_gcd
!?>?? endif
!?>?? if( int_k )then
   module procedure int_gcd
!?>?? endif
!?>?? if( long_k )then
   module procedure long_gcd
!?>?? endif
end interface

!  declare specific functions implementing the .lcm. operator

public :: operator( .lcm.)

interface operator( .lcm.)
!?>?? if( byte_k )then
   module procedure byte_lcm
!?>?? endif
!?>?? if( short_k )then
   module procedure short_lcm
!?>?? endif
!?>?? if( int_k )then
   module procedure int_lcm
!?>?? endif
!?>?? if( long_k )then
   module procedure long_lcm
!?>?? endif
end interface

!  declare specific functions implementing the lcm() function

public :: lcm

interface lcm
!?>?? if( byte_k )then
   module procedure byte_lcm
!?>?? endif
!?>?? if( short_k )then
   module procedure short_lcm
!?>?? endif
!?>?? if( int_k )then
   module procedure int_lcm
!?>?? endif
!?>?? if( long_k )then
   module procedure long_lcm
!?>?? endif
end interface

! **********************************************************************

!  declare specific functions implementing the .cd. operator

public :: operator( .cd.)

interface operator( .cd.)
!?>?? if( byte_k )then
   module procedure byte_cd
!?>?? endif
!?>?? if( short_k )then
   module procedure short_cd
!?>?? endif
!?>?? if( int_k )then
   module procedure int_cd
!?>?? endif
!?>?? if( long_k )then
   module procedure long_cd
!?>?? endif
end interface

!  declare specific functions implementing the cd() function

public :: cd

interface cd
!?>?? if( byte_k )then
   module procedure byte_cd
!?>?? endif
!?>?? if( short_k )then
   module procedure short_cd
!?>?? endif
!?>?? if( int_k )then
   module procedure int_cd
!?>?? endif
!?>?? if( long_k )then
   module procedure long_cd
!?>?? endif
end interface

!  declare specific functions implementing the .cr. operator

public :: operator( .cr.)

interface operator( .cr.)
!?>?? if( byte_k )then
   module procedure byte_cr
!?>?? endif
!?>?? if( short_k )then
   module procedure short_cr
!?>?? endif
!?>?? if( int_k )then
   module procedure int_cr
!?>?? endif
!?>?? if( long_k )then
   module procedure long_cr
!?>?? endif
end interface

!  declare specific functions implementing the cr() function

public :: cr

interface cr
!?>?? if( byte_k )then
   module procedure byte_cr
!?>?? endif
!?>?? if( short_k )then
   module procedure short_cr
!?>?? endif
!?>?? if( int_k )then
   module procedure int_cr
!?>?? endif
!?>?? if( long_k )then
   module procedure long_cr
!?>?? endif
end interface

! **********************************************************************

!  integer diagnostic functions

! **********************************************************************

!  declare specific functions implementing the iseven() function

public :: iseven

interface iseven
!?>?? if( byte_k )then
   module procedure byte_iseven
!?>?? endif
!?>?? if( short_k )then
   module procedure short_iseven
!?>?? endif
!?>?? if( int_k )then
   module procedure int_iseven
!?>?? endif
!?>?? if( long_k )then
   module procedure long_iseven
!?>?? endif
end interface

! **********************************************************************

!  declare specific functions implementing the isodd() function

public :: isodd

interface isodd
!?>?? if( byte_k )then
   module procedure byte_isodd
!?>?? endif
!?>?? if( short_k )then
   module procedure short_isodd
!?>?? endif
!?>?? if( int_k )then
   module procedure int_isodd
!?>?? endif
!?>?? if( long_k )then
   module procedure long_isodd
!?>?? endif
end interface

! **********************************************************************

!  real to integer functions

! **********************************************************************

!  declare specific functions implementing the rrint() function

public :: rrint

interface rrint
!?>?? if( single_k )then
   module procedure single_rrint
!?>?? endif
!?>?? if( double_k )then
   module procedure double_rrint
!?>?? endif
!?>?? if( quad_k )then
   module procedure quad_rrint
!?>?? endif
end interface

! **********************************************************************

!  logical utility functions

! **********************************************************************

!  declare specific functions implementing the compl() function

public :: compl

interface compl
!?>?? if( l_byte_k )then
   module procedure l_byte_compl
!?>?? endif
!?>?? if( l_short_k )then
   module procedure l_short_compl
!?>?? endif
!?>?? if( l_int_k )then
   module procedure l_int_compl
!?>?? endif
!?>?? if( l_long_k )then
   module procedure l_long_compl
!?>?? endif
end interface

! **********************************************************************

!  index finding functions

! **********************************************************************

!  declare specific function supporting generic function ismax()

public :: ismax

interface ismax
!?>?? if( single_k )then
   module procedure single_ismax
!?>?? endif
!?>?? if( double_k )then
   module procedure double_ismax
!?>?? endif
!?>?? if( quad_k )then
   module procedure quad_ismax
!?>?? endif
end interface

!  declare specific functions supporting generic function ismin()

public :: ismin

interface ismin
!?>?? if( single_k )then
   module procedure single_ismin
!?>?? endif
!?>?? if( double_k )then
   module procedure double_ismin
!?>?? endif
!?>?? if( quad_k )then
   module procedure quad_ismin
!?>?? endif
end interface

!  declare specific function supporting generic function isamax()

public :: isamax

interface isamax
!?>?? if( single_k )then
   module procedure single_isamax
!?>?? endif
!?>?? if( double_k )then
   module procedure double_isamax
!?>?? endif
!?>?? if( quad_k )then
   module procedure quad_isamax
!?>?? endif
end interface

!  declare specific functions supporting generic function isamin()

public :: isamin

interface isamin
!?>?? if( single_k )then
   module procedure single_isamin
!?>?? endif
!?>?? if( double_k )then
   module procedure double_isamin
!?>?? endif
!?>?? if( quad_k )then
   module procedure quad_isamin
!?>?? endif
end interface

!  declare specific function supporting generic function icamax()

public :: icamax

interface icamax
!?>?? if( single_k )then
   module procedure single_icamax
!?>?? endif
!?>?? if( double_k )then
   module procedure double_icamax
!?>?? endif
!?>?? if( quad_k )then
   module procedure quad_icamax
!?>?? endif
end interface

!  declare specific functions supporting generic function icamin()

public :: icamin

interface icamin
!?>?? if( single_k )then
   module procedure single_icamin
!?>?? endif
!?>?? if( double_k )then
   module procedure double_icamin
!?>?? endif
!?>?? if( quad_k )then
   module procedure quad_icamin
!?>?? endif
end interface

! **********************************************************************

!  machine constant functions

! **********************************************************************

!  declare specific function supporting generic function smach()

public :: smach

interface smach
!?>?? if( single_k )then
   module procedure single_smach
!?>?? endif
!?>?? if( double_k )then
   module procedure double_smach
!?>?? endif
!?>?? if( quad_k )then
   module procedure quad_smach
!?>?? endif
end interface

!  declare specific functions supporting generic function cmach()

public :: cmach

interface cmach
!?>?? if( single_k )then
   module procedure single_cmach
!?>?? endif
!?>?? if( double_k )then
   module procedure double_cmach
!?>?? endif
!?>?? if( quad_k )then
   module procedure quad_cmach
!?>?? endif
end interface

! **********************************************************************

!  subroutines to replace stop and pause statements

! **********************************************************************

!  declare specific subroutines supporting generic subroutine pause()

public :: pause

interface pause
   module procedure int_pause
   module procedure char_pause
end interface

!  declare specific subroutines supporting generic subroutine stop()

public :: stop

interface stop
   module procedure int_stop
   module procedure char_stop
end interface

! **********************************************************************

!  subroutines to implement the swap() routine

! **********************************************************************

!  declare specific subroutines supporting swap()

public :: swap

interface swap
!?>?? if( ascii_k )then
   module procedure ascii_character_swap
!?>?? endif
!?>?? if( ebcdic_k )then
!?>   module procedure ebcdic_character_swap
!?>?? endif
!?>?? if( byte_k )then
   module procedure byte_integer_swap
!?>?? endif
!?>?? if( short_k )then
   module procedure short_integer_swap
!?>?? endif
!?>?? if( int_k )then
   module procedure int_integer_swap
!?>?? endif
!?>?? if( long_k )then
   module procedure long_integer_swap
!?>?? endif
!?>?? if( l_byte_k )then
   module procedure l_byte_logical_swap
!?>?? endif
!?>?? if( l_short_k )then
   module procedure l_short_logical_swap
!?>?? endif
!?>?? if( l_int_k )then
   module procedure l_int_logical_swap
!?>?? endif
!?>?? if( l_long_k )then
   module procedure l_long_logical_swap
!?>?? endif
!?>?? if( single_k )then
   module procedure single_real_swap
!?>?? endif
!?>?? if( double_k )then
   module procedure double_real_swap
!?>?? endif
!?>?? if( quad_k )then
   module procedure quad_real_swap
!?>?? endif
!?>?? if( single_k )then
   module procedure single_complex_swap
!?>?? endif
!?>?? if( double_k )then
   module procedure double_complex_swap
!?>?? endif
!?>?? if( quad_k )then
   module procedure quad_complex_swap
!?>?? endif
end interface

! **********************************************************************

!  subroutines to implement the rev_endian() routine

! **********************************************************************

!  declare specific subroutines supporting rev_endian()

public :: rev_endian

interface rev_endian
!?>?? if( short_k )then
   module procedure short_rev_endian
!?>?? endif
!?>?? if( int_k )then
   module procedure int_rev_endian
!?>?? endif
!?>?? if( long_k )then
   module procedure long_rev_endian
!?>?? endif
!?>?? if( l_short_k )then
   module procedure l_short_rev_endian
!?>?? endif
!?>?? if( l_int_k )then
   module procedure l_int_rev_endian
!?>?? endif
!?>?? if( l_long_k )then
   module procedure l_long_rev_endian
!?>?? endif
!?>?? if( single_k )then
   module procedure single_rev_endian
!?>?? endif
!?>?? if( double_k )then
   module procedure double_rev_endian
!?>?? endif
!?>?? if( quad_k )then
   module procedure quad_rev_endian
!?>?? endif
!?>?? if( single_k )then
   module procedure single_complex_rev_endian
!?>?? endif
!?>?? if( double_k )then
   module procedure double_complex_rev_endian
!?>?? endif
!?>?? if( quad_k )then
   module procedure quad_complex_rev_endian
!?>?? endif
end interface

! **********************************************************************

!  subroutines to implement the rev_bits() routine

! **********************************************************************

!  declare specific subroutines supporting rev_bits()

public :: rev_bits

interface rev_bits
!?>?? if( ascii_k )then
   module procedure ascii_rev_bits
!?>?? endif
!?>?? if( ebcdic_k )then
!?>   module procedure ebcdic_rev_bits
!?>?? endif
!?>?? if( byte_k )then
   module procedure byte_rev_bits
!?>?? endif
!?>?? if( l_byte_k )then
   module procedure l_byte_rev_bits
!?>?? endif
end interface

! **********************************************************************

!  declare module procedure names public

public :: bin
public :: oct
public :: hex

public :: binstr
public :: octstr
public :: hexstr

public :: decode
public :: encode

public :: get_logical_unit

! **********************************************************************

!  module procedures

! **********************************************************************

contains

! **********************************************************************

!  define .xor. binary operator

!?>?? text :: xor( kind)
!?>! **********************************************************************
!?>
!?>!  ?kind?_xor: xor() for kind ?kind?
!?>
!?>elemental logical( kind= ?kind?_k) function ?kind?_xor( l1, l2)
!?>
!?>logical( kind= ?kind?_k), intent( in) :: l1, l2
!?>
!?>!  ?kind?_xor()
!?>
!?>continue
!?>
!?>   ?kind?_xor = l1 .neqv. l2
!?>
!?>return
!?>
!?>!  ?kind?_xor()
!?>
!?>end function ?kind?_xor
!?>
!?>?? end text xor
!?>?? if( l_byte_k )then
!?>?? copy :: xor( l_byte)
!?>?? ! text xor
! **********************************************************************

!  l_byte_xor: xor() for kind l_byte

elemental logical( kind= l_byte_k) function l_byte_xor( l1, l2)

logical( kind= l_byte_k), intent( in) :: l1, l2

!  l_byte_xor()

continue

   l_byte_xor = l1 .neqv. l2

return

!  l_byte_xor()

end function l_byte_xor

!?>?? ! end text xor
!?>?? endif
!?>?? if( l_short_k )then
!?>?? copy :: xor( l_short)
!?>?? ! text xor
! **********************************************************************

!  l_short_xor: xor() for kind l_short

elemental logical( kind= l_short_k) function l_short_xor( l1, l2)

logical( kind= l_short_k), intent( in) :: l1, l2

!  l_short_xor()

continue

   l_short_xor = l1 .neqv. l2

return

!  l_short_xor()

end function l_short_xor

!?>?? ! end text xor
!?>?? endif
!?>?? if( l_int_k )then
!?>?? copy :: xor( l_int)
!?>?? ! text xor
! **********************************************************************

!  l_int_xor: xor() for kind l_int

elemental logical( kind= l_int_k) function l_int_xor( l1, l2)

logical( kind= l_int_k), intent( in) :: l1, l2

!  l_int_xor()

continue

   l_int_xor = l1 .neqv. l2

return

!  l_int_xor()

end function l_int_xor

!?>?? ! end text xor
!?>?? endif
!?>?? if( l_long_k )then
!?>?? copy :: xor( l_long)
!?>?? ! text xor
! **********************************************************************

!  l_long_xor: xor() for kind l_long

elemental logical( kind= l_long_k) function l_long_xor( l1, l2)

logical( kind= l_long_k), intent( in) :: l1, l2

!  l_long_xor()

continue

   l_long_xor = l1 .neqv. l2

return

!  l_long_xor()

end function l_long_xor

!?>?? ! end text xor
!?>?? endif
! **********************************************************************

!  define .mod. binary operator

!?>?? text :: mod( type, kind)
!?>! **********************************************************************
!?>
!?>!  ?kind?_mod(): .mod. for kind ?kind?
!?>
!?>elemental ?type?( kind= ?kind?_k) function ?kind?_mod( a, p)
!?>
!?>?type?( kind= ?kind?_k), intent( in) :: a, p
!?>
!?>!  ?kind?_mod()
!?>
!?>continue
!?>
!?>   ?kind?_mod = mod( a, p)
!?>
!?>return
!?>
!?>!  ?kind?_mod()
!?>
!?>end function ?kind?_mod
!?>
!?>?? end text mod
!?>?? if( byte_k )then
!?>?? copy :: mod( integer, byte)
!?>?? ! text mod
! **********************************************************************

!  byte_mod(): .mod. for kind byte

elemental integer( kind= byte_k) function byte_mod( a, p)

integer( kind= byte_k), intent( in) :: a, p

!  byte_mod()

continue

   byte_mod = mod( a, p)

return

!  byte_mod()

end function byte_mod

!?>?? ! end text mod
!?>?? endif
!?>?? if( short_k )then
!?>?? copy :: mod( integer, short)
!?>?? ! text mod
! **********************************************************************

!  short_mod(): .mod. for kind short

elemental integer( kind= short_k) function short_mod( a, p)

integer( kind= short_k), intent( in) :: a, p

!  short_mod()

continue

   short_mod = mod( a, p)

return

!  short_mod()

end function short_mod

!?>?? ! end text mod
!?>?? endif
!?>?? if( int_k )then
!?>?? copy :: mod( integer, int)
!?>?? ! text mod
! **********************************************************************

!  int_mod(): .mod. for kind int

elemental integer( kind= int_k) function int_mod( a, p)

integer( kind= int_k), intent( in) :: a, p

!  int_mod()

continue

   int_mod = mod( a, p)

return

!  int_mod()

end function int_mod

!?>?? ! end text mod
!?>?? endif
!?>?? if( long_k )then
!?>?? copy :: mod( integer, long)
!?>?? ! text mod
! **********************************************************************

!  long_mod(): .mod. for kind long

elemental integer( kind= long_k) function long_mod( a, p)

integer( kind= long_k), intent( in) :: a, p

!  long_mod()

continue

   long_mod = mod( a, p)

return

!  long_mod()

end function long_mod

!?>?? ! end text mod
!?>?? endif
!?>?? if( single_k )then
!?>?? copy :: mod( real, single)
!?>?? ! text mod
! **********************************************************************

!  single_mod(): .mod. for kind single

elemental real( kind= single_k) function single_mod( a, p)

real( kind= single_k), intent( in) :: a, p

!  single_mod()

continue

   single_mod = mod( a, p)

return

!  single_mod()

end function single_mod

!?>?? ! end text mod
!?>?? endif
!?>?? if( double_k )then
!?>?? copy :: mod( real, double)
!?>?? ! text mod
! **********************************************************************

!  double_mod(): .mod. for kind double

elemental real( kind= double_k) function double_mod( a, p)

real( kind= double_k), intent( in) :: a, p

!  double_mod()

continue

   double_mod = mod( a, p)

return

!  double_mod()

end function double_mod

!?>?? ! end text mod
!?>?? endif
!?>?? if( quad_k )then
!?>?? copy :: mod( real, quad)
!?>?? ! text mod
! **********************************************************************

!  quad_mod(): .mod. for kind quad

elemental real( kind= quad_k) function quad_mod( a, p)

real( kind= quad_k), intent( in) :: a, p

!  quad_mod()

continue

   quad_mod = mod( a, p)

return

!  quad_mod()

end function quad_mod

!?>?? ! end text mod
!?>?? endif
! **********************************************************************

!  define .modulo. binary operator

!?>?? text :: modulo( type, kind)
!?>! **********************************************************************
!?>
!?>!  ?kind?_modulo(): .modulo. for kind ?kind?
!?>
!?>elemental ?type?( kind= ?kind?_k) function ?kind?_modulo( a, p)
!?>
!?>?type?( kind= ?kind?_k), intent( in) :: a, p
!?>
!?>!  ?kind?_modulo()
!?>
!?>continue
!?>
!?>   ?kind?_modulo = modulo( a, p)
!?>
!?>return
!?>
!?>!  ?kind?_modulo()
!?>
!?>end function ?kind?_modulo
!?>
!?>?? end text modulo
!?>?? if( byte_k )then
!?>?? copy :: modulo( integer, byte)
!?>?? ! text modulo
! **********************************************************************

!  byte_modulo(): .modulo. for kind byte

elemental integer( kind= byte_k) function byte_modulo( a, p)

integer( kind= byte_k), intent( in) :: a, p

!  byte_modulo()

continue

   byte_modulo = modulo( a, p)

return

!  byte_modulo()

end function byte_modulo

!?>?? ! end text modulo
!?>?? endif
!?>?? if( short_k )then
!?>?? copy :: modulo( integer, short)
!?>?? ! text modulo
! **********************************************************************

!  short_modulo(): .modulo. for kind short

elemental integer( kind= short_k) function short_modulo( a, p)

integer( kind= short_k), intent( in) :: a, p

!  short_modulo()

continue

   short_modulo = modulo( a, p)

return

!  short_modulo()

end function short_modulo

!?>?? ! end text modulo
!?>?? endif
!?>?? if( int_k )then
!?>?? copy :: modulo( integer, int)
!?>?? ! text modulo
! **********************************************************************

!  int_modulo(): .modulo. for kind int

elemental integer( kind= int_k) function int_modulo( a, p)

integer( kind= int_k), intent( in) :: a, p

!  int_modulo()

continue

   int_modulo = modulo( a, p)

return

!  int_modulo()

end function int_modulo

!?>?? ! end text modulo
!?>?? endif
!?>?? if( long_k )then
!?>?? copy :: modulo( integer, long)
!?>?? ! text modulo
! **********************************************************************

!  long_modulo(): .modulo. for kind long

elemental integer( kind= long_k) function long_modulo( a, p)

integer( kind= long_k), intent( in) :: a, p

!  long_modulo()

continue

   long_modulo = modulo( a, p)

return

!  long_modulo()

end function long_modulo

!?>?? ! end text modulo
!?>?? endif
!?>?? if( single_k )then
!?>?? copy :: modulo( real, single)
!?>?? ! text modulo
! **********************************************************************

!  single_modulo(): .modulo. for kind single

elemental real( kind= single_k) function single_modulo( a, p)

real( kind= single_k), intent( in) :: a, p

!  single_modulo()

continue

   single_modulo = modulo( a, p)

return

!  single_modulo()

end function single_modulo

!?>?? ! end text modulo
!?>?? endif
!?>?? if( double_k )then
!?>?? copy :: modulo( real, double)
!?>?? ! text modulo
! **********************************************************************

!  double_modulo(): .modulo. for kind double

elemental real( kind= double_k) function double_modulo( a, p)

real( kind= double_k), intent( in) :: a, p

!  double_modulo()

continue

   double_modulo = modulo( a, p)

return

!  double_modulo()

end function double_modulo

!?>?? ! end text modulo
!?>?? endif
!?>?? if( quad_k )then
!?>?? copy :: modulo( real, quad)
!?>?? ! text modulo
! **********************************************************************

!  quad_modulo(): .modulo. for kind quad

elemental real( kind= quad_k) function quad_modulo( a, p)

real( kind= quad_k), intent( in) :: a, p

!  quad_modulo()

continue

   quad_modulo = modulo( a, p)

return

!  quad_modulo()

end function quad_modulo

!?>?? ! end text modulo
!?>?? endif
! **********************************************************************

!  define iseven()/isodd() for integer kinds

!?>?? text :: iseven( kind)
!?>! **********************************************************************
!?>
!?>!  ?kind?_iseven(): iseven() for kind ?kind?
!?>
!?>elemental logical function ?kind?_iseven( a)
!?>
!?>integer( kind= ?kind?_k), intent( in) :: a
!?>
!?>!  ?kind?_iseven()
!?>
!?>continue
!?>
!?>   ?kind?_iseven = iand( a, 1_?kind?_k) == 0_?kind?_k
!?>
!?>return
!?>
!?>!  ?kind?_iseven()
!?>
!?>end function ?kind?_iseven
!?>
!?>?? end text iseven
!?>?? if( byte_k )then
!?>?? copy :: iseven( byte)
!?>?? ! text iseven
! **********************************************************************

!  byte_iseven(): iseven() for kind byte

elemental logical function byte_iseven( a)

integer( kind= byte_k), intent( in) :: a

!  byte_iseven()

continue

   byte_iseven = iand( a, 1_byte_k) == 0_byte_k

return

!  byte_iseven()

end function byte_iseven

!?>?? ! end text iseven
!?>?? endif
!?>?? if( short_k )then
!?>?? copy :: iseven( short)
!?>?? ! text iseven
! **********************************************************************

!  short_iseven(): iseven() for kind short

elemental logical function short_iseven( a)

integer( kind= short_k), intent( in) :: a

!  short_iseven()

continue

   short_iseven = iand( a, 1_short_k) == 0_short_k

return

!  short_iseven()

end function short_iseven

!?>?? ! end text iseven
!?>?? endif
!?>?? if( int_k )then
!?>?? copy :: iseven( int)
!?>?? ! text iseven
! **********************************************************************

!  int_iseven(): iseven() for kind int

elemental logical function int_iseven( a)

integer( kind= int_k), intent( in) :: a

!  int_iseven()

continue

   int_iseven = iand( a, 1_int_k) == 0_int_k

return

!  int_iseven()

end function int_iseven

!?>?? ! end text iseven
!?>?? endif
!?>?? if( long_k )then
!?>?? copy :: iseven( long)
!?>?? ! text iseven
! **********************************************************************

!  long_iseven(): iseven() for kind long

elemental logical function long_iseven( a)

integer( kind= long_k), intent( in) :: a

!  long_iseven()

continue

   long_iseven = iand( a, 1_long_k) == 0_long_k

return

!  long_iseven()

end function long_iseven

!?>?? ! end text iseven
!?>?? endif
!?>?? text :: isodd( kind)
!?>! **********************************************************************
!?>
!?>!  ?kind?_isodd(): isodd() for kind ?kind?
!?>
!?>elemental logical function ?kind?_isodd( a)
!?>
!?>integer( kind= ?kind?_k), intent( in) :: a
!?>
!?>!  ?kind?_isodd()
!?>
!?>continue
!?>
!?>   ?kind?_isodd = iand( a, 1_?kind?_k) == 1_?kind?_k
!?>
!?>return
!?>
!?>!  ?kind?_isodd()
!?>
!?>end function ?kind?_isodd
!?>
!?>?? end text isodd
!?>?? if( byte_k )then
!?>?? copy :: isodd( byte)
!?>?? ! text isodd
! **********************************************************************

!  byte_isodd(): isodd() for kind byte

elemental logical function byte_isodd( a)

integer( kind= byte_k), intent( in) :: a

!  byte_isodd()

continue

   byte_isodd = iand( a, 1_byte_k) == 1_byte_k

return

!  byte_isodd()

end function byte_isodd

!?>?? ! end text isodd
!?>?? endif
!?>?? if( short_k )then
!?>?? copy :: isodd( short)
!?>?? ! text isodd
! **********************************************************************

!  short_isodd(): isodd() for kind short

elemental logical function short_isodd( a)

integer( kind= short_k), intent( in) :: a

!  short_isodd()

continue

   short_isodd = iand( a, 1_short_k) == 1_short_k

return

!  short_isodd()

end function short_isodd

!?>?? ! end text isodd
!?>?? endif
!?>?? if( int_k )then
!?>?? copy :: isodd( int)
!?>?? ! text isodd
! **********************************************************************

!  int_isodd(): isodd() for kind int

elemental logical function int_isodd( a)

integer( kind= int_k), intent( in) :: a

!  int_isodd()

continue

   int_isodd = iand( a, 1_int_k) == 1_int_k

return

!  int_isodd()

end function int_isodd

!?>?? ! end text isodd
!?>?? endif
!?>?? if( long_k )then
!?>?? copy :: isodd( long)
!?>?? ! text isodd
! **********************************************************************

!  long_isodd(): isodd() for kind long

elemental logical function long_isodd( a)

integer( kind= long_k), intent( in) :: a

!  long_isodd()

continue

   long_isodd = iand( a, 1_long_k) == 1_long_k

return

!  long_isodd()

end function long_isodd

!?>?? ! end text isodd
!?>?? endif
! **********************************************************************

!  define gcd()/lcm()

!?>?? text :: gcd( kind)
!?>! **********************************************************************
!?>
!?>!  ?kind?_gcd() gcd() for kind ?kind?_k
!?>
!?>elemental integer( kind= ?kind?_k) function ?kind?_gcd( a, b)
!?>
!?>integer( kind= ?kind?_k), intent( in) :: a, b
!?>
!?>!  ?kind?_gcd() local
!?>
!?>   integer( kind= ?kind?_k) :: a_gcd, b_gcd, rnp1, rn, rnm1
!?>
!?>!  ?kind?_gcd()
!?>
!?>continue
!?>
!?>! if a or b zero, abs( other) is gcd
!?>
!?>   zero_a: if( a == 0_?kind?_k )then
!?>
!?>      ?kind?_gcd = abs( b)
!?>
!?>      return
!?>
!?>   endif zero_a
!?>
!?>   zero_b: if( b == 0_?kind?_k )then
!?>
!?>      ?kind?_gcd = abs( a)
!?>
!?>      return
!?>
!?>   endif zero_b
!?>
!?>! set |a| >= |b| ( > 0)
!?>! r1 = a .mod. b
!?>! r0 = b
!?>
!?>   a_gcd = max( abs( a), abs( b))
!?>
!?>   b_gcd = min( abs( a), abs( b))
!?>
!?>   rn = a_gcd .mod. b_gcd
!?>
!?>   rnm1 = b_gcd
!?>
!?>! while rn /= 0
!?>!    compute rn+1 = rn .mod. rn-1
!?>! gcd() = rnm1
!?>
!?>   zero_rem: do while( rn /= 0_?kind?_k)
!?>
!?>      rnp1 = rnm1 .mod. rn
!?>
!?>      rnm1 = rn
!?>
!?>      rn = rnp1
!?>
!?>   enddo zero_rem
!?>
!?>   ?kind?_gcd = rnm1
!?>
!?>return
!?>
!?>!  ?kind?_gcd()
!?>
!?>end function ?kind?_gcd
!?>
!?>?? end text gcd
!?>?? if( byte_k )then
!?>?? copy :: gcd( byte)
!?>?? ! text gcd
! **********************************************************************

!  byte_gcd() gcd() for kind byte_k

elemental integer( kind= byte_k) function byte_gcd( a, b)

integer( kind= byte_k), intent( in) :: a, b

!  byte_gcd() local

   integer( kind= byte_k) :: a_gcd, b_gcd, rnp1, rn, rnm1

!  byte_gcd()

continue

! if a or b zero, abs( other) is gcd

   zero_a: if( a == 0_byte_k )then

      byte_gcd = abs( b)

      return

   endif zero_a

   zero_b: if( b == 0_byte_k )then

      byte_gcd = abs( a)

      return

   endif zero_b

! set |a| >= |b| ( > 0)
! r1 = a .mod. b
! r0 = b

   a_gcd = max( abs( a), abs( b))

   b_gcd = min( abs( a), abs( b))

   rn = a_gcd .mod. b_gcd

   rnm1 = b_gcd

! while rn /= 0
!    compute rn+1 = rn .mod. rn-1
! gcd() = rnm1

   zero_rem: do while( rn /= 0_byte_k)

      rnp1 = rnm1 .mod. rn

      rnm1 = rn

      rn = rnp1

   enddo zero_rem

   byte_gcd = rnm1

return

!  byte_gcd()

end function byte_gcd

!?>?? ! end text gcd
!?>?? endif
!?>?? if( short_k )then
!?>?? copy :: gcd( short)
!?>?? ! text gcd
! **********************************************************************

!  short_gcd() gcd() for kind short_k

elemental integer( kind= short_k) function short_gcd( a, b)

integer( kind= short_k), intent( in) :: a, b

!  short_gcd() local

   integer( kind= short_k) :: a_gcd, b_gcd, rnp1, rn, rnm1

!  short_gcd()

continue

! if a or b zero, abs( other) is gcd

   zero_a: if( a == 0_short_k )then

      short_gcd = abs( b)

      return

   endif zero_a

   zero_b: if( b == 0_short_k )then

      short_gcd = abs( a)

      return

   endif zero_b

! set |a| >= |b| ( > 0)
! r1 = a .mod. b
! r0 = b

   a_gcd = max( abs( a), abs( b))

   b_gcd = min( abs( a), abs( b))

   rn = a_gcd .mod. b_gcd

   rnm1 = b_gcd

! while rn /= 0
!    compute rn+1 = rn .mod. rn-1
! gcd() = rnm1

   zero_rem: do while( rn /= 0_short_k)

      rnp1 = rnm1 .mod. rn

      rnm1 = rn

      rn = rnp1

   enddo zero_rem

   short_gcd = rnm1

return

!  short_gcd()

end function short_gcd

!?>?? ! end text gcd
!?>?? endif
!?>?? if( int_k )then
!?>?? copy :: gcd( int)
!?>?? ! text gcd
! **********************************************************************

!  int_gcd() gcd() for kind int_k

elemental integer( kind= int_k) function int_gcd( a, b)

integer( kind= int_k), intent( in) :: a, b

!  int_gcd() local

   integer( kind= int_k) :: a_gcd, b_gcd, rnp1, rn, rnm1

!  int_gcd()

continue

! if a or b zero, abs( other) is gcd

   zero_a: if( a == 0_int_k )then

      int_gcd = abs( b)

      return

   endif zero_a

   zero_b: if( b == 0_int_k )then

      int_gcd = abs( a)

      return

   endif zero_b

! set |a| >= |b| ( > 0)
! r1 = a .mod. b
! r0 = b

   a_gcd = max( abs( a), abs( b))

   b_gcd = min( abs( a), abs( b))

   rn = a_gcd .mod. b_gcd

   rnm1 = b_gcd

! while rn /= 0
!    compute rn+1 = rn .mod. rn-1
! gcd() = rnm1

   zero_rem: do while( rn /= 0_int_k)

      rnp1 = rnm1 .mod. rn

      rnm1 = rn

      rn = rnp1

   enddo zero_rem

   int_gcd = rnm1

return

!  int_gcd()

end function int_gcd

!?>?? ! end text gcd
!?>?? endif
!?>?? if( long_k )then
!?>?? copy :: gcd( long)
!?>?? ! text gcd
! **********************************************************************

!  long_gcd() gcd() for kind long_k

elemental integer( kind= long_k) function long_gcd( a, b)

integer( kind= long_k), intent( in) :: a, b

!  long_gcd() local

   integer( kind= long_k) :: a_gcd, b_gcd, rnp1, rn, rnm1

!  long_gcd()

continue

! if a or b zero, abs( other) is gcd

   zero_a: if( a == 0_long_k )then

      long_gcd = abs( b)

      return

   endif zero_a

   zero_b: if( b == 0_long_k )then

      long_gcd = abs( a)

      return

   endif zero_b

! set |a| >= |b| ( > 0)
! r1 = a .mod. b
! r0 = b

   a_gcd = max( abs( a), abs( b))

   b_gcd = min( abs( a), abs( b))

   rn = a_gcd .mod. b_gcd

   rnm1 = b_gcd

! while rn /= 0
!    compute rn+1 = rn .mod. rn-1
! gcd() = rnm1

   zero_rem: do while( rn /= 0_long_k)

      rnp1 = rnm1 .mod. rn

      rnm1 = rn

      rn = rnp1

   enddo zero_rem

   long_gcd = rnm1

return

!  long_gcd()

end function long_gcd

!?>?? ! end text gcd
!?>?? endif
! **********************************************************************

!  lcm

!?>?? text :: lcm( kind)
!?>! **********************************************************************
!?>
!?>!  ?kind?_lcm() lcm() for kind= ?kind?_k
!?>
!?>elemental integer( kind= ?kind?_k) function ?kind?_lcm( a, b)
!?>
!?>integer( kind= ?kind?_k), intent( in) :: a, b
!?>
!?>!  ?kind?_lcm()
!?>
!?>continue
!?>
!?>   ?kind?_lcm = ( a* b) / gcd( a, b)
!?>
!?>return
!?>
!?>!  ?kind?_lcm()
!?>
!?>end function ?kind?_lcm
!?>
!?>?? end text lcm
!?>?? if( int_k )then
!?>?? copy :: lcm( byte)
!?>?? ! text lcm
! **********************************************************************

!  byte_lcm() lcm() for kind= byte_k

elemental integer( kind= byte_k) function byte_lcm( a, b)

integer( kind= byte_k), intent( in) :: a, b

!  byte_lcm()

continue

   byte_lcm = ( a* b) / gcd( a, b)

return

!  byte_lcm()

end function byte_lcm

!?>?? ! end text lcm
!?>?? endif
!?>?? if( short_k )then
!?>?? copy :: lcm( short)
!?>?? ! text lcm
! **********************************************************************

!  short_lcm() lcm() for kind= short_k

elemental integer( kind= short_k) function short_lcm( a, b)

integer( kind= short_k), intent( in) :: a, b

!  short_lcm()

continue

   short_lcm = ( a* b) / gcd( a, b)

return

!  short_lcm()

end function short_lcm

!?>?? ! end text lcm
!?>?? endif
!?>?? if( int_k )then
!?>?? copy :: lcm( int)
!?>?? ! text lcm
! **********************************************************************

!  int_lcm() lcm() for kind= int_k

elemental integer( kind= int_k) function int_lcm( a, b)

integer( kind= int_k), intent( in) :: a, b

!  int_lcm()

continue

   int_lcm = ( a* b) / gcd( a, b)

return

!  int_lcm()

end function int_lcm

!?>?? ! end text lcm
!?>?? endif
!?>?? if( long_k )then
!?>?? copy :: lcm( long)
!?>?? ! text lcm
! **********************************************************************

!  long_lcm() lcm() for kind= long_k

elemental integer( kind= long_k) function long_lcm( a, b)

integer( kind= long_k), intent( in) :: a, b

!  long_lcm()

continue

   long_lcm = ( a* b) / gcd( a, b)

return

!  long_lcm()

end function long_lcm

!?>?? ! end text lcm
!?>?? endif
! **********************************************************************

!  define cd()/cr() for integer kinds

!?>?? text :: cd( kind)
!?>! **********************************************************************
!?>
!?>!  ?kind?_cd(): .cd., cd() for kind ?kind?
!?>
!?>elemental integer( kind= ?kind?_k) function ?kind?_cd( j, k)
!?>
!?>integer( kind= ?kind?_k), intent( in) :: j, k
!?>
!?>!  ?kind?_cd()
!?>
!?>continue
!?>
!?>   ?kind?_cd = ( j + k - 1_?kind?_k) / k
!?>
!?>return
!?>
!?>!  ?kind?_cd()
!?>
!?>end function ?kind?_cd
!?>
!?>?? end text cd
!?>?? if( byte_k )then
!?>?? copy :: cd( byte)
!?>?? ! text cd
! **********************************************************************

!  byte_cd(): .cd., cd() for kind byte

elemental integer( kind= byte_k) function byte_cd( j, k)

integer( kind= byte_k), intent( in) :: j, k

!  byte_cd()

continue

   byte_cd = ( j + k - 1_byte_k) / k

return

!  byte_cd()

end function byte_cd

!?>?? ! end text cd
!?>?? endif
!?>?? if( short_k )then
!?>?? copy :: cd( short)
!?>?? ! text cd
! **********************************************************************

!  short_cd(): .cd., cd() for kind short

elemental integer( kind= short_k) function short_cd( j, k)

integer( kind= short_k), intent( in) :: j, k

!  short_cd()

continue

   short_cd = ( j + k - 1_short_k) / k

return

!  short_cd()

end function short_cd

!?>?? ! end text cd
!?>?? endif
!?>?? if( int_k )then
!?>?? copy :: cd( int)
!?>?? ! text cd
! **********************************************************************

!  int_cd(): .cd., cd() for kind int

elemental integer( kind= int_k) function int_cd( j, k)

integer( kind= int_k), intent( in) :: j, k

!  int_cd()

continue

   int_cd = ( j + k - 1_int_k) / k

return

!  int_cd()

end function int_cd

!?>?? ! end text cd
!?>?? endif
!?>?? if( long_k )then
!?>?? copy :: cd( long)
!?>?? ! text cd
! **********************************************************************

!  long_cd(): .cd., cd() for kind long

elemental integer( kind= long_k) function long_cd( j, k)

integer( kind= long_k), intent( in) :: j, k

!  long_cd()

continue

   long_cd = ( j + k - 1_long_k) / k

return

!  long_cd()

end function long_cd

!?>?? ! end text cd
!?>?? endif
!?>?? text :: cr( kind)
!?>! **********************************************************************
!?>
!?>!  ?kind?_cr(): .cr., cr() for kind ?kind?
!?>
!?>elemental integer( kind= ?kind?_k) function ?kind?_cr( j, k)
!?>
!?>integer( kind= ?kind?_k), intent( in) :: j, k
!?>
!?>!  ?kind?_cr()
!?>
!?>continue
!?>
!?>   ?kind?_cr = j - k * cd( j, k)
!?>
!?>return
!?>
!?>!  ?kind?_cr()
!?>
!?>end function ?kind?_cr
!?>
!?>?? end text cr
!?>?? if( byte_k )then
!?>?? copy :: cr( byte)
!?>?? ! text cr
! **********************************************************************

!  byte_cr(): .cr., cr() for kind byte

elemental integer( kind= byte_k) function byte_cr( j, k)

integer( kind= byte_k), intent( in) :: j, k

!  byte_cr()

continue

   byte_cr = j - k * cd( j, k)

return

!  byte_cr()

end function byte_cr

!?>?? ! end text cr
!?>?? endif
!?>?? if( short_k )then
!?>?? copy :: cr( short)
!?>?? ! text cr
! **********************************************************************

!  short_cr(): .cr., cr() for kind short

elemental integer( kind= short_k) function short_cr( j, k)

integer( kind= short_k), intent( in) :: j, k

!  short_cr()

continue

   short_cr = j - k * cd( j, k)

return

!  short_cr()

end function short_cr

!?>?? ! end text cr
!?>?? endif
!?>?? if( int_k )then
!?>?? copy :: cr( int)
!?>?? ! text cr
! **********************************************************************

!  int_cr(): .cr., cr() for kind int

elemental integer( kind= int_k) function int_cr( j, k)

integer( kind= int_k), intent( in) :: j, k

!  int_cr()

continue

   int_cr = j - k * cd( j, k)

return

!  int_cr()

end function int_cr

!?>?? ! end text cr
!?>?? endif
!?>?? if( long_k )then
!?>?? copy :: cr( long)
!?>?? ! text cr
! **********************************************************************

!  long_cr(): .cr., cr() for kind long

elemental integer( kind= long_k) function long_cr( j, k)

integer( kind= long_k), intent( in) :: j, k

!  long_cr()

continue

   long_cr = j - k * cd( j, k)

return

!  long_cr()

end function long_cr

!?>?? ! end text cr
!?>?? endif
! **********************************************************************

!  define hex()/oct()/bin() integer from string

! **********************************************************************

!  hex() hex string to int

integer function hex( hexstr, stat)

character( len= *), intent( in) :: hexstr

integer, optional, intent( out) :: stat

!  hex() local

   character( len= hexstr_len) :: str

   integer :: istr, jloc

!  hex() digits table

   character( len= *), parameter :: lc_digits = '0123456789abcdef'

! **********************************************************************

!  hex()

continue

!  make local copy of input string

   str = adjustl( hexstr)

!  force lower case for A-F digits

   to_lc: do istr = 1, hexstr_len

      force_lc: select case( str( istr: istr))

      case( 'A': 'F' ) force_lc

         str( istr: istr) = char( ichar( str( istr: istr)) + 32)

      end select force_lc

   enddo to_lc

!  initialize for decode loop

   istr = 1
   hex = 0

!  decode each digit

   each_digit: do while( str( istr: ) /= ' ' )

      jloc = index( lc_digits, str( istr: istr))

      no_hex: if( jloc == 0 )then

         bad_char: if( present( stat) )then

            stat = istr

         endif bad_char

         hex = 0

         return

      endif no_hex

      hex = hex * 16 + jloc - 1

      istr = istr + 1

   enddo each_digit

!  status and normal exit

   status_arg: if( present( stat) )then

      stat = 0

   endif status_arg

return

!  hex()

end function hex

! **********************************************************************

!  oct() octal string to int

integer function oct( octstr, stat)

character( len= *), intent( in) :: octstr

integer, optional, intent( out) :: stat

!  oct() local

   character( len= octstr_len) :: str

   integer :: istr, jloc

!  oct() digits table

   character( len= *), parameter :: lc_digits = '01234567'

! **********************************************************************

!  oct()

continue

!  make local copy of input string

   str = adjustl( octstr)

!  initialize for decode loop

   istr = 1
   oct = 0

!  decode each digit

   each_digit: do while( str( istr: ) /= ' ' )

      jloc = index( lc_digits, str( istr: istr))

      no_oct: if( jloc == 0 )then

         bad_char: if( present( stat) )then

            stat = istr

         endif bad_char

         oct = 0

         return

      endif no_oct

      oct = oct * 8 + jloc - 1

      istr = istr + 1

   enddo each_digit

!  status and normal exit

   status_arg: if( present( stat) )then

       stat = 0

   endif status_arg

return

!  oct()

end function oct

! **********************************************************************

!  bin() binary string to int

integer function bin( binstr, stat)

character( len= *), intent( in) :: binstr

integer, optional, intent( out) :: stat

!  bin() local

   character( len= binstr_len) :: str

   integer :: istr, jloc

!  bin() digits table

   character( len= *), parameter :: lc_digits = '01'

! **********************************************************************

!  bin()

continue

!  make local copy of input string

   str = adjustl( binstr)

!  initialize for decode loop

   istr = 1
   bin = 0

!  decode each digit

   each_digit: do while( str( istr: ) /= ' ' )

      jloc = index( lc_digits( 1: 2), str( istr: istr))

      no_bin: if( jloc == 0 )then

         bad_char: if( present( stat) )then

            stat = istr

         endif bad_char

         bin = 0

         return

      endif no_bin

      bin = bin * 2 + jloc - 1

      istr = istr + 1

   enddo each_digit

!  status and normal exit

   status_arg: if( present( stat) )then

      stat = 0

   endif status_arg

return

!  bin()

end function bin

! **********************************************************************

!  define hexstr()/octstr()/binstr() string from integer

! **********************************************************************

!  hexstr() integer to hex string

character( len= hexstr_len) function hexstr( i, lc)

integer, intent( in) :: i

logical, optional, intent( in) :: lc

!  hexstr() digits tables

   character( len= *), parameter :: lc_digits = '0123456789abcdef'
   character( len= *), parameter :: uc_digits = '0123456789ABCDEF'

!  hexstr() local

   character( len= len( lc_digits)) :: hex_digits

   integer :: hex_i, jstr, kdig

   logical :: hex_lc

! **********************************************************************

!  hexstr()

continue

! lc argument is present

   lc_arg: if( present( lc) )then

      hex_lc = lc

   else lc_arg

      hex_lc = .true.

   endif lc_arg

!  lower case or upper case

   lc_uc: if( hex_lc )then

      hex_digits = lc_digits

   else lc_uc

      hex_digits = uc_digits

   endif lc_uc

!  initialize encode loop

   jstr = hexstr_len

   hexstr = ' '
   hexstr( jstr: jstr) = '0'

   hex_i = i

!  encode loop

   each_digit: do while( hex_i /= 0)

      kdig = iand( hex_i, 15) + 1

      hexstr( jstr: jstr) = hex_digits( kdig: kdig)

      hex_i = ishft( hex_i, -4)

      jstr = jstr - 1

   enddo each_digit

   hexstr = adjustl( hexstr)

!  successful return

return

!  hexstr()

end function hexstr

! **********************************************************************

!  octstr() integer to octal string

character( len= octstr_len) function octstr( i)

integer, intent( in) :: i

!  octstr() local

   integer :: oct_i, jstr, kdig

!  hex()/oct()/bin() digits table

   character( len= *), parameter :: lc_digits = '01234567'

! **********************************************************************

!  octstr()

continue

!  initialize encode loop

   jstr = octstr_len

   octstr = ' '
   octstr( jstr: jstr) = '0'

   oct_i = i

!  encode loop

   each_digit: do while( oct_i /= 0)

      kdig = iand( oct_i, 7) + 1

      octstr( jstr: jstr) = lc_digits( kdig: kdig)

      oct_i = ishft( oct_i, -3)

      jstr = jstr - 1

   enddo each_digit

   octstr = adjustl( octstr)

!  successful return

return

!  octstr()

end function octstr

! **********************************************************************

!  binstr() integer to binary string

character( len= binstr_len) function binstr( i)

integer, intent( in) :: i

!  binstr() local

   integer :: bin_i, jstr, kdig

!  hex()/oct()/bin() digits table

   character( len= *), parameter :: lc_digits = '01'

! **********************************************************************

!  binstr()

continue

!  initialize encode loop

   jstr = binstr_len

   binstr = ' '
   binstr( jstr: jstr) = '0'

   bin_i = i

!  encode loop

   each_digit: do while( bin_i /= 0)

      kdig = iand( bin_i, 1) + 1

      binstr( jstr: jstr) = lc_digits( kdig: kdig)

      bin_i = ishft( bin_i, -1)

      jstr = jstr - 1

   enddo each_digit

   binstr = adjustl( binstr)

!  successful return

return

!  binstr()

end function binstr

! **********************************************************************

!  define encode() & decode()

! **********************************************************************

!  encode(): integer to character string using translation table

pure subroutine encode( str, i, ttable, stat)

character( len= *), intent( out) :: str

integer, intent( in) :: i

character( len= *), intent( in) :: ttable

integer, optional, intent( out) :: stat

!  encode() local

   integer :: base

   integer :: jstr, lenstr, loci, next_char

!  encode()

continue

!  string lengths

   lenstr = len( str)

   base = len( ttable)

   bad_base: if( base < 1 )then

      status_error: if( present( stat) )then

         stat = error_bad_base

      endif status_error

      return

   endif bad_base

   loci = i

   str = ' '

!  encode i in str

   next_char = lenstr + 1

!  do positive i

   all_chars: do while( loci > 0 )

      jstr = (loci .mod. base) + 1

      overflow: if( next_char > 1 )then

         next_char = next_char - 1

         str( next_char: next_char) = ttable( jstr: jstr)

      elseif( next_char == 1 )then overflow

         str = '*'

         if( present( stat) ) stat = error_short_string

         return

      endif overflow

      loci = loci / base

   enddo all_chars

   status_arg: if( present( stat) )then

      stat = 0

   endif status_arg

return

!  encode()

end subroutine encode

! **********************************************************************

!  decode(): character string to integer using ttable

pure subroutine decode( i, str, ttable, stat)

integer, intent( out) :: i

character( len= *), intent( in) :: str

character( len= *), intent( in) :: ttable

integer, optional, intent( out) :: stat

!  decode() local

   character( len= len( str)) :: str_buff

   integer :: base

   integer :: jstr

!  decode()

continue

!  check input

   str_buff = adjustl( str)

   base = len( ttable)

   i = 0

   bad_base: if( base < 1 )then

      status_error: if( present( stat) )then

         stat = error_bad_base

      endif status_error

      return

   endif bad_base

!  scan str

   each_char: do while( str_buff /= ' ')

      jstr = index( ttable, str_buff( 1: 1))

      bad_char: if( jstr == 0 )then

         i = 0

         if( present( stat) ) stat = error_not_in_table

         return

      endif bad_char

      i = i*base + ( jstr - 1)

      str_buff = str_buff( 2: )

   enddo each_char

   status_arg: if( present( stat) )then

      stat = 0

   endif status_arg

return

!  decode()

end subroutine decode

! **********************************************************************

!  rrint()- randomly round real to integer

!?>?? text :: rrint( kind)
!?>! **********************************************************************
!?>
!?>!  ?kind?_rrint: rrint() for kind ?kind?
!?>
!?>integer function ?kind?_rrint( a)
!?>
!?>real( kind= ?kind?_k), intent( in) :: a
!?>
!?>!  ?kind?_rrint() local
!?>
!?>   real( kind= ?kind?_k) :: r
!?>
!?>   integer :: i
!?>
!?>!  ?kind?_rrint()
!?>
!?>continue
!?>
!?>   overflow: if( int( a) >= huge( i) )then
!?>
!?>      ?kind?_rrint = huge( i)
!?>
!?>      return
!?>
!?>   elseif( int( a) <= -huge( i) )then overflow
!?>
!?>      ?kind?_rrint = -huge( i)
!?>
!?>      return
!?>
!?>   endif overflow
!?>
!?>   call random_number( r)
!?>
!?>!  down if close to floor, up if close to ceiling
!?>
!?>   hi_lo: if( ( a - real( floor( a), kind= ?kind?_k)) <= r )then
!?>
!?>      i = floor( a)
!?>
!?>   else hi_lo
!?>
!?>      i = ceiling( a)
!?>
!?>   endif hi_lo
!?>
!?>   ?kind?_rrint = i
!?>
!?>return
!?>
!?>!  ?kind?_rrint()
!?>
!?>end function ?kind?_rrint
!?>
!?>?? end text rrint
!?>?? if( single_k )then
!?>?? copy :: rrint( single)
!?>?? ! text rrint
! **********************************************************************

!  single_rrint: rrint() for kind single

integer function single_rrint( a)

real( kind= single_k), intent( in) :: a

!  single_rrint() local

   real( kind= single_k) :: r

   integer :: i

!  single_rrint()

continue

   overflow: if( int( a) >= huge( i) )then

      single_rrint = huge( i)

      return

   elseif( int( a) <= -huge( i) )then overflow

      single_rrint = -huge( i)

      return

   endif overflow

   call random_number( r)

!  down if close to floor, up if close to ceiling

   hi_lo: if( ( a - real( floor( a), kind= single_k)) <= r )then

      i = floor( a)

   else hi_lo

      i = ceiling( a)

   endif hi_lo

   single_rrint = i

return

!  single_rrint()

end function single_rrint

!?>?? ! end text rrint
!?>?? endif
!?>?? if( double_k )then
!?>?? copy :: rrint( double)
!?>?? ! text rrint
! **********************************************************************

!  double_rrint: rrint() for kind double

integer function double_rrint( a)

real( kind= double_k), intent( in) :: a

!  double_rrint() local

   real( kind= double_k) :: r

   integer :: i

!  double_rrint()

continue

   overflow: if( int( a) >= huge( i) )then

      double_rrint = huge( i)

      return

   elseif( int( a) <= -huge( i) )then overflow

      double_rrint = -huge( i)

      return

   endif overflow

   call random_number( r)

!  down if close to floor, up if close to ceiling

   hi_lo: if( ( a - real( floor( a), kind= double_k)) <= r )then

      i = floor( a)

   else hi_lo

      i = ceiling( a)

   endif hi_lo

   double_rrint = i

return

!  double_rrint()

end function double_rrint

!?>?? ! end text rrint
!?>?? endif
!?>?? if( quad_k )then
!?>?? copy :: rrint( quad)
!?>?? ! text rrint
! **********************************************************************

!  quad_rrint: rrint() for kind quad

integer function quad_rrint( a)

real( kind= quad_k), intent( in) :: a

!  quad_rrint() local

   real( kind= quad_k) :: r

   integer :: i

!  quad_rrint()

continue

   overflow: if( int( a) >= huge( i) )then

      quad_rrint = huge( i)

      return

   elseif( int( a) <= -huge( i) )then overflow

      quad_rrint = -huge( i)

      return

   endif overflow

   call random_number( r)

!  down if close to floor, up if close to ceiling

   hi_lo: if( ( a - real( floor( a), kind= quad_k)) <= r )then

      i = floor( a)

   else hi_lo

      i = ceiling( a)

   endif hi_lo

   quad_rrint = i

return

!  quad_rrint()

end function quad_rrint

!?>?? ! end text rrint
!?>?? endif
! **********************************************************************

!  define compl()

!?>?? text :: compl( kind)
!?>! **********************************************************************
!?>
!?>!  ?kind?_compl: compl() for kind ?kind?
!?>
!?>elemental logical( kind= ?kind?_k) function ?kind?_compl( l)
!?>
!?>logical( kind= ?kind?_k), intent( in) :: l
!?>
!?>!  ?kind?_compl()
!?>
!?>continue
!?>
!?>   ?kind?_compl = .not. l
!?>
!?>return
!?>
!?>!  ?kind?_compl()
!?>
!?>end function ?kind?_compl
!?>
!?>?? end text compl
!?>?? if( l_byte_k )then
!?>?? copy :: compl( l_byte)
!?>?? ! text compl
! **********************************************************************

!  l_byte_compl: compl() for kind l_byte

elemental logical( kind= l_byte_k) function l_byte_compl( l)

logical( kind= l_byte_k), intent( in) :: l

!  l_byte_compl()

continue

   l_byte_compl = .not. l

return

!  l_byte_compl()

end function l_byte_compl

!?>?? ! end text compl
!?>?? endif
!?>?? if( l_short_k )then
!?>?? copy :: compl( l_short)
!?>?? ! text compl
! **********************************************************************

!  l_short_compl: compl() for kind l_short

elemental logical( kind= l_short_k) function l_short_compl( l)

logical( kind= l_short_k), intent( in) :: l

!  l_short_compl()

continue

   l_short_compl = .not. l

return

!  l_short_compl()

end function l_short_compl

!?>?? ! end text compl
!?>?? endif
!?>?? if( l_int_k )then
!?>?? copy :: compl( l_int)
!?>?? ! text compl
! **********************************************************************

!  l_int_compl: compl() for kind l_int

elemental logical( kind= l_int_k) function l_int_compl( l)

logical( kind= l_int_k), intent( in) :: l

!  l_int_compl()

continue

   l_int_compl = .not. l

return

!  l_int_compl()

end function l_int_compl

!?>?? ! end text compl
!?>?? endif
!?>?? if( l_long_k )then
!?>?? copy :: compl( l_long)
!?>?? ! text compl
! **********************************************************************

!  l_long_compl: compl() for kind l_long

elemental logical( kind= l_long_k) function l_long_compl( l)

logical( kind= l_long_k), intent( in) :: l

!  l_long_compl()

continue

   l_long_compl = .not. l

return

!  l_long_compl()

end function l_long_compl

!?>?? ! end text compl
!?>?? endif
! **********************************************************************

!  ismax(), ismin(), isamax(), isamin() for real kinds

!?>?? text :: ismax( kind)
!?>! **********************************************************************
!?>
!?>!  ?kind?_ismax(): ismax() for kind ?kind?
!?>
!?>pure integer function ?kind?_ismax( n, x, incx)
!?>
!?>!  array x of length n stride incx
!?>
!?>integer, intent( in) :: n, incx
!?>
!?>real( kind= ?kind?_k), dimension( n), intent( in) :: x
!?>
!?>!  ?kind?_ismax() local
!?>
!?>   integer, dimension( 1) :: indx
!?>
!?>!  ?kind?_ismax()
!?>
!?>continue
!?>
!?>!  zero or fewer elements
!?>
!?>   n_zero: if( n <= 0 )then
!?>
!?>      ?kind?_ismax = 0
!?>
!?>      return
!?>
!?>   endif n_zero
!?>
!?>!  direction of stride
!?>
!?>   incx_p_m: if( incx > 0 )then
!?>
!?>      indx = maxloc( x( 1: n: incx))
!?>      ?kind?_ismax = (( indx( 1) - 1)/incx) + 1
!?>
!?>   elseif( incx < 0 )then incx_p_m
!?>
!?>      indx = maxloc( x( n: 1: incx))
!?>      ?kind?_ismax = (( -indx( 1) + n)/incx) + 1
!?>
!?>   else incx_p_m
!?>
!?>      ?kind?_ismax = 0
!?>
!?>   endif incx_p_m
!?>
!?>return
!?>
!?>!  ?kind?_ismax()
!?>
!?>end function ?kind?_ismax
!?>
!?>?? end text ismax
!?>?? if( single_k )then
!?>?? copy :: ismax( single)
!?>?? ! text ismax
! **********************************************************************

!  single_ismax(): ismax() for kind single

pure integer function single_ismax( n, x, incx)

!  array x of length n stride incx

integer, intent( in) :: n, incx

real( kind= single_k), dimension( n), intent( in) :: x

!  single_ismax() local

   integer, dimension( 1) :: indx

!  single_ismax()

continue

!  zero or fewer elements

   n_zero: if( n <= 0 )then

      single_ismax = 0

      return

   endif n_zero

!  direction of stride

   incx_p_m: if( incx > 0 )then

      indx = maxloc( x( 1: n: incx))
      single_ismax = (( indx( 1) - 1)/incx) + 1

   elseif( incx < 0 )then incx_p_m

      indx = maxloc( x( n: 1: incx))
      single_ismax = (( -indx( 1) + n)/incx) + 1

   else incx_p_m

      single_ismax = 0

   endif incx_p_m

return

!  single_ismax()

end function single_ismax

!?>?? ! end text ismax
!?>?? endif
!?>?? if( double_k )then
!?>?? copy :: ismax( double)
!?>?? ! text ismax
! **********************************************************************

!  double_ismax(): ismax() for kind double

pure integer function double_ismax( n, x, incx)

!  array x of length n stride incx

integer, intent( in) :: n, incx

real( kind= double_k), dimension( n), intent( in) :: x

!  double_ismax() local

   integer, dimension( 1) :: indx

!  double_ismax()

continue

!  zero or fewer elements

   n_zero: if( n <= 0 )then

      double_ismax = 0

      return

   endif n_zero

!  direction of stride

   incx_p_m: if( incx > 0 )then

      indx = maxloc( x( 1: n: incx))
      double_ismax = (( indx( 1) - 1)/incx) + 1

   elseif( incx < 0 )then incx_p_m

      indx = maxloc( x( n: 1: incx))
      double_ismax = (( -indx( 1) + n)/incx) + 1

   else incx_p_m

      double_ismax = 0

   endif incx_p_m

return

!  double_ismax()

end function double_ismax

!?>?? ! end text ismax
!?>?? endif
!?>?? if( quad_k )then
!?>?? copy :: ismax( quad)
!?>?? ! text ismax
! **********************************************************************

!  quad_ismax(): ismax() for kind quad

pure integer function quad_ismax( n, x, incx)

!  array x of length n stride incx

integer, intent( in) :: n, incx

real( kind= quad_k), dimension( n), intent( in) :: x

!  quad_ismax() local

   integer, dimension( 1) :: indx

!  quad_ismax()

continue

!  zero or fewer elements

   n_zero: if( n <= 0 )then

      quad_ismax = 0

      return

   endif n_zero

!  direction of stride

   incx_p_m: if( incx > 0 )then

      indx = maxloc( x( 1: n: incx))
      quad_ismax = (( indx( 1) - 1)/incx) + 1

   elseif( incx < 0 )then incx_p_m

      indx = maxloc( x( n: 1: incx))
      quad_ismax = (( -indx( 1) + n)/incx) + 1

   else incx_p_m

      quad_ismax = 0

   endif incx_p_m

return

!  quad_ismax()

end function quad_ismax

!?>?? ! end text ismax
!?>?? endif
!?>?? text :: ismin( kind)
!?>! **********************************************************************
!?>
!?>!  ?kind?_ismin(): ismin() for kind ?kind?
!?>
!?>pure integer function ?kind?_ismin( n, x, incx)
!?>
!?>!  array x of length n stride incx
!?>
!?>integer, intent( in) :: n, incx
!?>
!?>real( kind= ?kind?_k), dimension( n), intent( in) :: x
!?>
!?>!  ?kind?_ismin() local
!?>
!?>   integer, dimension( 1) :: indx
!?>
!?>!  ?kind?_ismin()
!?>
!?>continue
!?>
!?>!  zero or fewer elements
!?>
!?>   n_zero: if( n <= 0 )then
!?>
!?>      ?kind?_ismin = 0
!?>
!?>      return
!?>
!?>   endif n_zero
!?>
!?>!  direction of stride
!?>
!?>   incx_p_m: if( incx > 0 )then
!?>
!?>      indx = minloc( x( 1: n: incx))
!?>      ?kind?_ismin = (( indx( 1) - 1)/incx) + 1
!?>
!?>   elseif( incx < 0 )then incx_p_m
!?>
!?>      indx = minloc( x( n: 1: incx))
!?>      ?kind?_ismin = (( -indx( 1) + n)/incx) + 1
!?>
!?>   else incx_p_m
!?>
!?>      ?kind?_ismin = 0
!?>
!?>   endif incx_p_m
!?>
!?>return
!?>
!?>!  ?kind?_ismin()
!?>
!?>end function ?kind?_ismin
!?>
!?>?? end text ismin
!?>?? if( single_k )then
!?>?? copy :: ismin( single)
!?>?? ! text ismin
! **********************************************************************

!  single_ismin(): ismin() for kind single

pure integer function single_ismin( n, x, incx)

!  array x of length n stride incx

integer, intent( in) :: n, incx

real( kind= single_k), dimension( n), intent( in) :: x

!  single_ismin() local

   integer, dimension( 1) :: indx

!  single_ismin()

continue

!  zero or fewer elements

   n_zero: if( n <= 0 )then

      single_ismin = 0

      return

   endif n_zero

!  direction of stride

   incx_p_m: if( incx > 0 )then

      indx = minloc( x( 1: n: incx))
      single_ismin = (( indx( 1) - 1)/incx) + 1

   elseif( incx < 0 )then incx_p_m

      indx = minloc( x( n: 1: incx))
      single_ismin = (( -indx( 1) + n)/incx) + 1

   else incx_p_m

      single_ismin = 0

   endif incx_p_m

return

!  single_ismin()

end function single_ismin

!?>?? ! end text ismin
!?>?? endif
!?>?? if( double_k )then
!?>?? copy :: ismin( double)
!?>?? ! text ismin
! **********************************************************************

!  double_ismin(): ismin() for kind double

pure integer function double_ismin( n, x, incx)

!  array x of length n stride incx

integer, intent( in) :: n, incx

real( kind= double_k), dimension( n), intent( in) :: x

!  double_ismin() local

   integer, dimension( 1) :: indx

!  double_ismin()

continue

!  zero or fewer elements

   n_zero: if( n <= 0 )then

      double_ismin = 0

      return

   endif n_zero

!  direction of stride

   incx_p_m: if( incx > 0 )then

      indx = minloc( x( 1: n: incx))
      double_ismin = (( indx( 1) - 1)/incx) + 1

   elseif( incx < 0 )then incx_p_m

      indx = minloc( x( n: 1: incx))
      double_ismin = (( -indx( 1) + n)/incx) + 1

   else incx_p_m

      double_ismin = 0

   endif incx_p_m

return

!  double_ismin()

end function double_ismin

!?>?? ! end text ismin
!?>?? endif
!?>?? if( quad_k )then
!?>?? copy :: ismin( quad)
!?>?? ! text ismin
! **********************************************************************

!  quad_ismin(): ismin() for kind quad

pure integer function quad_ismin( n, x, incx)

!  array x of length n stride incx

integer, intent( in) :: n, incx

real( kind= quad_k), dimension( n), intent( in) :: x

!  quad_ismin() local

   integer, dimension( 1) :: indx

!  quad_ismin()

continue

!  zero or fewer elements

   n_zero: if( n <= 0 )then

      quad_ismin = 0

      return

   endif n_zero

!  direction of stride

   incx_p_m: if( incx > 0 )then

      indx = minloc( x( 1: n: incx))
      quad_ismin = (( indx( 1) - 1)/incx) + 1

   elseif( incx < 0 )then incx_p_m

      indx = minloc( x( n: 1: incx))
      quad_ismin = (( -indx( 1) + n)/incx) + 1

   else incx_p_m

      quad_ismin = 0

   endif incx_p_m

return

!  quad_ismin()

end function quad_ismin

!?>?? ! end text ismin
!?>?? endif
!?>?? text :: isamax( kind)
!?>! **********************************************************************
!?>
!?>!  ?kind?_isamax(): isamax() for kind ?kind?
!?>
!?>pure integer function ?kind?_isamax( n, x, incx)
!?>
!?>!  array x of length n stride incx
!?>
!?>integer, intent( in) :: n, incx
!?>
!?>real( kind= ?kind?_k), dimension( n), intent( in) :: x
!?>
!?>!  ?kind?_isamax() local
!?>
!?>   integer, dimension( 1) :: indx
!?>
!?>!  ?kind?_isamax()
!?>
!?>continue
!?>
!?>!  zero or fewer elements
!?>
!?>   n_zero: if( n <= 0 )then
!?>
!?>      ?kind?_isamax = 0
!?>
!?>      return
!?>
!?>   endif n_zero
!?>
!?>!  direction of stride
!?>
!?>   incx_p_m: if( incx > 0 )then
!?>
!?>      indx = maxloc( abs( x( 1: n: incx)) )
!?>      ?kind?_isamax = (( indx( 1) - 1)/incx) + 1
!?>
!?>   elseif( incx < 0 )then incx_p_m
!?>
!?>      indx = maxloc( abs( x( n: 1: incx)) )
!?>      ?kind?_isamax = (( -indx( 1) + n)/incx) + 1
!?>
!?>   else incx_p_m
!?>
!?>      ?kind?_isamax = 0
!?>
!?>   endif incx_p_m
!?>
!?>return
!?>
!?>!  ?kind?_isamax()
!?>
!?>end function ?kind?_isamax
!?>
!?>?? end text isamax
!?>?? if( single_k )then
!?>?? copy :: isamax( single)
!?>?? ! text isamax
! **********************************************************************

!  single_isamax(): isamax() for kind single

pure integer function single_isamax( n, x, incx)

!  array x of length n stride incx

integer, intent( in) :: n, incx

real( kind= single_k), dimension( n), intent( in) :: x

!  single_isamax() local

   integer, dimension( 1) :: indx

!  single_isamax()

continue

!  zero or fewer elements

   n_zero: if( n <= 0 )then

      single_isamax = 0

      return

   endif n_zero

!  direction of stride

   incx_p_m: if( incx > 0 )then

      indx = maxloc( abs( x( 1: n: incx)) )
      single_isamax = (( indx( 1) - 1)/incx) + 1

   elseif( incx < 0 )then incx_p_m

      indx = maxloc( abs( x( n: 1: incx)) )
      single_isamax = (( -indx( 1) + n)/incx) + 1

   else incx_p_m

      single_isamax = 0

   endif incx_p_m

return

!  single_isamax()

end function single_isamax

!?>?? ! end text isamax
!?>?? endif
!?>?? if( double_k )then
!?>?? copy :: isamax( double)
!?>?? ! text isamax
! **********************************************************************

!  double_isamax(): isamax() for kind double

pure integer function double_isamax( n, x, incx)

!  array x of length n stride incx

integer, intent( in) :: n, incx

real( kind= double_k), dimension( n), intent( in) :: x

!  double_isamax() local

   integer, dimension( 1) :: indx

!  double_isamax()

continue

!  zero or fewer elements

   n_zero: if( n <= 0 )then

      double_isamax = 0

      return

   endif n_zero

!  direction of stride

   incx_p_m: if( incx > 0 )then

      indx = maxloc( abs( x( 1: n: incx)) )
      double_isamax = (( indx( 1) - 1)/incx) + 1

   elseif( incx < 0 )then incx_p_m

      indx = maxloc( abs( x( n: 1: incx)) )
      double_isamax = (( -indx( 1) + n)/incx) + 1

   else incx_p_m

      double_isamax = 0

   endif incx_p_m

return

!  double_isamax()

end function double_isamax

!?>?? ! end text isamax
!?>?? endif
!?>?? if( quad_k )then
!?>?? copy :: isamax( quad)
!?>?? ! text isamax
! **********************************************************************

!  quad_isamax(): isamax() for kind quad

pure integer function quad_isamax( n, x, incx)

!  array x of length n stride incx

integer, intent( in) :: n, incx

real( kind= quad_k), dimension( n), intent( in) :: x

!  quad_isamax() local

   integer, dimension( 1) :: indx

!  quad_isamax()

continue

!  zero or fewer elements

   n_zero: if( n <= 0 )then

      quad_isamax = 0

      return

   endif n_zero

!  direction of stride

   incx_p_m: if( incx > 0 )then

      indx = maxloc( abs( x( 1: n: incx)) )
      quad_isamax = (( indx( 1) - 1)/incx) + 1

   elseif( incx < 0 )then incx_p_m

      indx = maxloc( abs( x( n: 1: incx)) )
      quad_isamax = (( -indx( 1) + n)/incx) + 1

   else incx_p_m

      quad_isamax = 0

   endif incx_p_m

return

!  quad_isamax()

end function quad_isamax

!?>?? ! end text isamax
!?>?? endif
!?>?? text :: isamin( kind)
!?>! **********************************************************************
!?>
!?>!  ?kind?_isamin(): isamin() for kind ?kind?
!?>
!?>pure integer function ?kind?_isamin( n, x, incx)
!?>
!?>!  array x of length n stride incx
!?>
!?>integer, intent( in) :: n, incx
!?>
!?>real( kind= ?kind?_k), dimension( n), intent( in) :: x
!?>
!?>!  ?kind?_isamin() local
!?>
!?>   integer, dimension( 1) :: indx
!?>
!?>!  ?kind?_isamin()
!?>
!?>continue
!?>
!?>!  zero or fewer elements
!?>
!?>   n_zero: if( n <= 0 )then
!?>
!?>      ?kind?_isamin = 0
!?>
!?>      return
!?>
!?>   endif n_zero
!?>
!?>!  direction of stride
!?>
!?>   incx_p_m: if( incx > 0 )then
!?>
!?>      indx = minloc( abs( x( 1: n: incx)) )
!?>      ?kind?_isamin = (( indx( 1) - 1)/incx) + 1
!?>
!?>   elseif( incx < 0 )then incx_p_m
!?>
!?>      indx = minloc( abs( x( n: 1: incx)) )
!?>      ?kind?_isamin = (( -indx( 1) + n)/incx) + 1
!?>
!?>   else incx_p_m
!?>
!?>      ?kind?_isamin = 0
!?>
!?>   endif incx_p_m
!?>
!?>return
!?>
!?>!  ?kind?_isamin()
!?>
!?>end function ?kind?_isamin
!?>
!?>?? end text isamin
!?>?? if( single_k )then
!?>?? copy :: isamin( single)
!?>?? ! text isamin
! **********************************************************************

!  single_isamin(): isamin() for kind single

pure integer function single_isamin( n, x, incx)

!  array x of length n stride incx

integer, intent( in) :: n, incx

real( kind= single_k), dimension( n), intent( in) :: x

!  single_isamin() local

   integer, dimension( 1) :: indx

!  single_isamin()

continue

!  zero or fewer elements

   n_zero: if( n <= 0 )then

      single_isamin = 0

      return

   endif n_zero

!  direction of stride

   incx_p_m: if( incx > 0 )then

      indx = minloc( abs( x( 1: n: incx)) )
      single_isamin = (( indx( 1) - 1)/incx) + 1

   elseif( incx < 0 )then incx_p_m

      indx = minloc( abs( x( n: 1: incx)) )
      single_isamin = (( -indx( 1) + n)/incx) + 1

   else incx_p_m

      single_isamin = 0

   endif incx_p_m

return

!  single_isamin()

end function single_isamin

!?>?? ! end text isamin
!?>?? endif
!?>?? if( double_k )then
!?>?? copy :: isamin( double)
!?>?? ! text isamin
! **********************************************************************

!  double_isamin(): isamin() for kind double

pure integer function double_isamin( n, x, incx)

!  array x of length n stride incx

integer, intent( in) :: n, incx

real( kind= double_k), dimension( n), intent( in) :: x

!  double_isamin() local

   integer, dimension( 1) :: indx

!  double_isamin()

continue

!  zero or fewer elements

   n_zero: if( n <= 0 )then

      double_isamin = 0

      return

   endif n_zero

!  direction of stride

   incx_p_m: if( incx > 0 )then

      indx = minloc( abs( x( 1: n: incx)) )
      double_isamin = (( indx( 1) - 1)/incx) + 1

   elseif( incx < 0 )then incx_p_m

      indx = minloc( abs( x( n: 1: incx)) )
      double_isamin = (( -indx( 1) + n)/incx) + 1

   else incx_p_m

      double_isamin = 0

   endif incx_p_m

return

!  double_isamin()

end function double_isamin

!?>?? ! end text isamin
!?>?? endif
!?>?? if( quad_k )then
!?>?? copy :: isamin( quad)
!?>?? ! text isamin
! **********************************************************************

!  quad_isamin(): isamin() for kind quad

pure integer function quad_isamin( n, x, incx)

!  array x of length n stride incx

integer, intent( in) :: n, incx

real( kind= quad_k), dimension( n), intent( in) :: x

!  quad_isamin() local

   integer, dimension( 1) :: indx

!  quad_isamin()

continue

!  zero or fewer elements

   n_zero: if( n <= 0 )then

      quad_isamin = 0

      return

   endif n_zero

!  direction of stride

   incx_p_m: if( incx > 0 )then

      indx = minloc( abs( x( 1: n: incx)) )
      quad_isamin = (( indx( 1) - 1)/incx) + 1

   elseif( incx < 0 )then incx_p_m

      indx = minloc( abs( x( n: 1: incx)) )
      quad_isamin = (( -indx( 1) + n)/incx) + 1

   else incx_p_m

      quad_isamin = 0

   endif incx_p_m

return

!  quad_isamin()

end function quad_isamin

!?>?? ! end text isamin
!?>?? endif
! **********************************************************************

!  icamax(), icamin() for complex kinds

!?>?? text :: icamax( kind)
!?>! **********************************************************************
!?>
!?>!  ?kind?_icamax(): icamax() for kind ?kind?
!?>
!?>pure integer function ?kind?_icamax( n, x, incx)
!?>
!?>!  array x of length n stride incx
!?>
!?>integer, intent( in) :: n, incx
!?>
!?>complex( kind= ?kind?_k), dimension( n), intent( in) :: x
!?>
!?>!  ?kind?_icamax() local
!?>
!?>   integer, dimension( 1) :: indx
!?>
!?>!  ?kind?_icamax()
!?>
!?>continue
!?>
!?>!  zero or fewer elements
!?>
!?>   n_zero: if( n <= 0 )then
!?>
!?>      ?kind?_icamax = 0
!?>
!?>      return
!?>
!?>   endif n_zero
!?>
!?>!  direction of stride
!?>
!?>   incx_p_m: if( incx > 0 )then
!?>
!?>      indx = maxloc( abs( x( 1: n: incx) ))
!?>      ?kind?_icamax = (( indx( 1) - 1)/incx) + 1
!?>
!?>   elseif( incx < 0 )then incx_p_m
!?>
!?>      indx = maxloc( abs( x( n: 1: incx) ))
!?>      ?kind?_icamax = (( -indx( 1) + n)/incx) + 1
!?>
!?>   else incx_p_m
!?>
!?>      ?kind?_icamax = 0
!?>
!?>   endif incx_p_m
!?>
!?>return
!?>
!?>!  ?kind?_icamax()
!?>
!?>end function ?kind?_icamax
!?>
!?>?? end text icamax
!?>?? if( single_k )then
!?>?? copy :: icamax( single)
!?>?? ! text icamax
! **********************************************************************

!  single_icamax(): icamax() for kind single

pure integer function single_icamax( n, x, incx)

!  array x of length n stride incx

integer, intent( in) :: n, incx

complex( kind= single_k), dimension( n), intent( in) :: x

!  single_icamax() local

   integer, dimension( 1) :: indx

!  single_icamax()

continue

!  zero or fewer elements

   n_zero: if( n <= 0 )then

      single_icamax = 0

      return

   endif n_zero

!  direction of stride

   incx_p_m: if( incx > 0 )then

      indx = maxloc( abs( x( 1: n: incx) ))
      single_icamax = (( indx( 1) - 1)/incx) + 1

   elseif( incx < 0 )then incx_p_m

      indx = maxloc( abs( x( n: 1: incx) ))
      single_icamax = (( -indx( 1) + n)/incx) + 1

   else incx_p_m

      single_icamax = 0

   endif incx_p_m

return

!  single_icamax()

end function single_icamax

!?>?? ! end text icamax
!?>?? endif
!?>?? if( double_k )then
!?>?? copy :: icamax( double)
!?>?? ! text icamax
! **********************************************************************

!  double_icamax(): icamax() for kind double

pure integer function double_icamax( n, x, incx)

!  array x of length n stride incx

integer, intent( in) :: n, incx

complex( kind= double_k), dimension( n), intent( in) :: x

!  double_icamax() local

   integer, dimension( 1) :: indx

!  double_icamax()

continue

!  zero or fewer elements

   n_zero: if( n <= 0 )then

      double_icamax = 0

      return

   endif n_zero

!  direction of stride

   incx_p_m: if( incx > 0 )then

      indx = maxloc( abs( x( 1: n: incx) ))
      double_icamax = (( indx( 1) - 1)/incx) + 1

   elseif( incx < 0 )then incx_p_m

      indx = maxloc( abs( x( n: 1: incx) ))
      double_icamax = (( -indx( 1) + n)/incx) + 1

   else incx_p_m

      double_icamax = 0

   endif incx_p_m

return

!  double_icamax()

end function double_icamax

!?>?? ! end text icamax
!?>?? endif
!?>?? if( quad_k )then
!?>?? copy :: icamax( quad)
!?>?? ! text icamax
! **********************************************************************

!  quad_icamax(): icamax() for kind quad

pure integer function quad_icamax( n, x, incx)

!  array x of length n stride incx

integer, intent( in) :: n, incx

complex( kind= quad_k), dimension( n), intent( in) :: x

!  quad_icamax() local

   integer, dimension( 1) :: indx

!  quad_icamax()

continue

!  zero or fewer elements

   n_zero: if( n <= 0 )then

      quad_icamax = 0

      return

   endif n_zero

!  direction of stride

   incx_p_m: if( incx > 0 )then

      indx = maxloc( abs( x( 1: n: incx) ))
      quad_icamax = (( indx( 1) - 1)/incx) + 1

   elseif( incx < 0 )then incx_p_m

      indx = maxloc( abs( x( n: 1: incx) ))
      quad_icamax = (( -indx( 1) + n)/incx) + 1

   else incx_p_m

      quad_icamax = 0

   endif incx_p_m

return

!  quad_icamax()

end function quad_icamax

!?>?? ! end text icamax
!?>?? endif
!?>?? text :: icamin( kind)
!?>! **********************************************************************
!?>
!?>!  ?kind?_icamin(): icamin() for kind ?kind?
!?>
!?>pure integer function ?kind?_icamin( n, x, incx)
!?>
!?>!  array x of length n stride incx
!?>
!?>integer, intent( in) :: n, incx
!?>
!?>complex( kind= ?kind?_k), dimension( n), intent( in) :: x
!?>
!?>!  single_icamin() local
!?>
!?>   integer, dimension( 1) :: indx
!?>
!?>!  ?kind?_icamin()
!?>
!?>continue
!?>
!?>!  zero or fewer elements
!?>
!?>   n_zero: if( n <= 0 )then
!?>
!?>      ?kind?_icamin = 0
!?>
!?>      return
!?>
!?>   endif n_zero
!?>
!?>!  direction of stride
!?>
!?>   incx_p_m: if( incx > 0 )then
!?>
!?>      indx = minloc( abs( x( 1: n: incx) ))
!?>      ?kind?_icamin = (( indx( 1) - 1)/incx) + 1
!?>
!?>   elseif( incx < 0 )then incx_p_m
!?>
!?>      indx = minloc( abs( x( n: 1: incx) ))
!?>      ?kind?_icamin = (( -indx( 1) + n)/incx) + 1
!?>
!?>   else incx_p_m
!?>
!?>      ?kind?_icamin = 0
!?>
!?>   endif incx_p_m
!?>
!?>return
!?>
!?>!  ?kind?_icamin()
!?>
!?>end function ?kind?_icamin
!?>
!?>?? end text icamin
!?>?? if( single_k )then
!?>?? copy :: icamin( single)
!?>?? ! text icamin
! **********************************************************************

!  single_icamin(): icamin() for kind single

pure integer function single_icamin( n, x, incx)

!  array x of length n stride incx

integer, intent( in) :: n, incx

complex( kind= single_k), dimension( n), intent( in) :: x

!  single_icamin() local

   integer, dimension( 1) :: indx

!  single_icamin()

continue

!  zero or fewer elements

   n_zero: if( n <= 0 )then

      single_icamin = 0

      return

   endif n_zero

!  direction of stride

   incx_p_m: if( incx > 0 )then

      indx = minloc( abs( x( 1: n: incx) ))
      single_icamin = (( indx( 1) - 1)/incx) + 1

   elseif( incx < 0 )then incx_p_m

      indx = minloc( abs( x( n: 1: incx) ))
      single_icamin = (( -indx( 1) + n)/incx) + 1

   else incx_p_m

      single_icamin = 0

   endif incx_p_m

return

!  single_icamin()

end function single_icamin

!?>?? ! end text icamin
!?>?? endif
!?>?? if( double_k )then
!?>?? copy :: icamin( double)
!?>?? ! text icamin
! **********************************************************************

!  double_icamin(): icamin() for kind double

pure integer function double_icamin( n, x, incx)

!  array x of length n stride incx

integer, intent( in) :: n, incx

complex( kind= double_k), dimension( n), intent( in) :: x

!  single_icamin() local

   integer, dimension( 1) :: indx

!  double_icamin()

continue

!  zero or fewer elements

   n_zero: if( n <= 0 )then

      double_icamin = 0

      return

   endif n_zero

!  direction of stride

   incx_p_m: if( incx > 0 )then

      indx = minloc( abs( x( 1: n: incx) ))
      double_icamin = (( indx( 1) - 1)/incx) + 1

   elseif( incx < 0 )then incx_p_m

      indx = minloc( abs( x( n: 1: incx) ))
      double_icamin = (( -indx( 1) + n)/incx) + 1

   else incx_p_m

      double_icamin = 0

   endif incx_p_m

return

!  double_icamin()

end function double_icamin

!?>?? ! end text icamin
!?>?? endif
!?>?? if( quad_k )then
!?>?? copy :: icamin( quad)
!?>?? ! text icamin
! **********************************************************************

!  quad_icamin(): icamin() for kind quad

pure integer function quad_icamin( n, x, incx)

!  array x of length n stride incx

integer, intent( in) :: n, incx

complex( kind= quad_k), dimension( n), intent( in) :: x

!  single_icamin() local

   integer, dimension( 1) :: indx

!  quad_icamin()

continue

!  zero or fewer elements

   n_zero: if( n <= 0 )then

      quad_icamin = 0

      return

   endif n_zero

!  direction of stride

   incx_p_m: if( incx > 0 )then

      indx = minloc( abs( x( 1: n: incx) ))
      quad_icamin = (( indx( 1) - 1)/incx) + 1

   elseif( incx < 0 )then incx_p_m

      indx = minloc( abs( x( n: 1: incx) ))
      quad_icamin = (( -indx( 1) + n)/incx) + 1

   else incx_p_m

      quad_icamin = 0

   endif incx_p_m

return

!  quad_icamin()

end function quad_icamin

!?>?? ! end text icamin
!?>?? endif
! **********************************************************************

!  define smach() & cmach()

! **********************************************************************

!?>?? text :: smach( kind)
!?>!  ?kind?_smach(): pause or pause n subroutine
!?>
!?>subroutine ?kind?_smach( n, mach)
!?>
!?>integer, intent( in) :: n
!?>
!?>real( kind= ?kind?_k), intent( out) :: mach
!?>
!?>!  ?kind?_smach()
!?>
!?>continue
!?>
!?>   switch_arg: select case( n)
!?>
!?>   case( 1) switch_arg
!?>
!?>      mach = epsilon( 0.0_?kind?_k)
!?>
!?>   case( 2) switch_arg
!?>
!?>      mach = tiny( 0.0_?kind?_k)
!?>
!?>   case( 3) switch_arg
!?>
!?>      mach = huge( 0.0_?kind?_k)
!?>
!?>   case default switch_arg
!?>
!?>      mach = 0.0_?kind?_k
!?>
!?>   end select switch_arg
!?>
!?>return
!?>
!?>!  ?kind?_smach()
!?>
!?>end subroutine ?kind?_smach
!?>
!?>?? end text smach
!?>?? if( single_k )then
!?>?? copy :: smach( single)
!?>?? ! text smach
!  single_smach(): pause or pause n subroutine

subroutine single_smach( n, mach)

integer, intent( in) :: n

real( kind= single_k), intent( out) :: mach

!  single_smach()

continue

   switch_arg: select case( n)

   case( 1) switch_arg

      mach = epsilon( 0.0_single_k)

   case( 2) switch_arg

      mach = tiny( 0.0_single_k)

   case( 3) switch_arg

      mach = huge( 0.0_single_k)

   case default switch_arg

      mach = 0.0_single_k

   end select switch_arg

return

!  single_smach()

end subroutine single_smach

!?>?? ! end text smach
!?>?? endif
!?>?? if( double_k )then
!?>?? copy :: smach( double)
!?>?? ! text smach
!  double_smach(): pause or pause n subroutine

subroutine double_smach( n, mach)

integer, intent( in) :: n

real( kind= double_k), intent( out) :: mach

!  double_smach()

continue

   switch_arg: select case( n)

   case( 1) switch_arg

      mach = epsilon( 0.0_double_k)

   case( 2) switch_arg

      mach = tiny( 0.0_double_k)

   case( 3) switch_arg

      mach = huge( 0.0_double_k)

   case default switch_arg

      mach = 0.0_double_k

   end select switch_arg

return

!  double_smach()

end subroutine double_smach

!?>?? ! end text smach
!?>?? endif
!?>?? if( quad_k )then
!?>?? copy :: smach( quad)
!?>?? ! text smach
!  quad_smach(): pause or pause n subroutine

subroutine quad_smach( n, mach)

integer, intent( in) :: n

real( kind= quad_k), intent( out) :: mach

!  quad_smach()

continue

   switch_arg: select case( n)

   case( 1) switch_arg

      mach = epsilon( 0.0_quad_k)

   case( 2) switch_arg

      mach = tiny( 0.0_quad_k)

   case( 3) switch_arg

      mach = huge( 0.0_quad_k)

   case default switch_arg

      mach = 0.0_quad_k

   end select switch_arg

return

!  quad_smach()

end subroutine quad_smach

!?>?? ! end text smach
!?>?? endif
!?>?? text :: cmach( kind)
!?>! ----------------------------------------------------------------------
!?>
!?>!  cmach(): pause 'string' function
!?>
!?>subroutine ?kind?_cmach( n, mach)
!?>
!?>integer, intent( in) :: n
!?>
!?>real( kind= ?kind?_k), intent( out) :: mach
!?>
!?>!  ?kind?_cmach()
!?>
!?>continue
!?>
!?>   switch_arg: select case( n)
!?>
!?>   case( 1) switch_arg
!?>
!?>      mach = sqrt( epsilon( 0.0_?kind?_k))
!?>
!?>   case( 2) switch_arg
!?>
!?>      mach = sqrt( tiny( 0.0_?kind?_k))
!?>
!?>   case( 3) switch_arg
!?>
!?>      mach = sqrt( huge( 0.0_?kind?_k))
!?>
!?>   case default switch_arg
!?>
!?>      mach = 0.0_?kind?_k
!?>
!?>   end select switch_arg
!?>
!?>return
!?>
!?>!  ?kind?_cmach()
!?>
!?>end subroutine ?kind?_cmach
!?>
!?>?? end text cmach
!?>?? if( single_k )then
!?>?? copy :: cmach( single)
!?>?? ! text cmach
! ----------------------------------------------------------------------

!  cmach(): pause 'string' function

subroutine single_cmach( n, mach)

integer, intent( in) :: n

real( kind= single_k), intent( out) :: mach

!  single_cmach()

continue

   switch_arg: select case( n)

   case( 1) switch_arg

      mach = sqrt( epsilon( 0.0_single_k))

   case( 2) switch_arg

      mach = sqrt( tiny( 0.0_single_k))

   case( 3) switch_arg

      mach = sqrt( huge( 0.0_single_k))

   case default switch_arg

      mach = 0.0_single_k

   end select switch_arg

return

!  single_cmach()

end subroutine single_cmach

!?>?? ! end text cmach
!?>?? endif
!?>?? if( double_k )then
!?>?? copy :: cmach( double)
!?>?? ! text cmach
! ----------------------------------------------------------------------

!  cmach(): pause 'string' function

subroutine double_cmach( n, mach)

integer, intent( in) :: n

real( kind= double_k), intent( out) :: mach

!  double_cmach()

continue

   switch_arg: select case( n)

   case( 1) switch_arg

      mach = sqrt( epsilon( 0.0_double_k))

   case( 2) switch_arg

      mach = sqrt( tiny( 0.0_double_k))

   case( 3) switch_arg

      mach = sqrt( huge( 0.0_double_k))

   case default switch_arg

      mach = 0.0_double_k

   end select switch_arg

return

!  double_cmach()

end subroutine double_cmach

!?>?? ! end text cmach
!?>?? endif
!?>?? if( quad_k )then
!?>?? copy :: cmach( quad)
!?>?? ! text cmach
! ----------------------------------------------------------------------

!  cmach(): pause 'string' function

subroutine quad_cmach( n, mach)

integer, intent( in) :: n

real( kind= quad_k), intent( out) :: mach

!  quad_cmach()

continue

   switch_arg: select case( n)

   case( 1) switch_arg

      mach = sqrt( epsilon( 0.0_quad_k))

   case( 2) switch_arg

      mach = sqrt( tiny( 0.0_quad_k))

   case( 3) switch_arg

      mach = sqrt( huge( 0.0_quad_k))

   case default switch_arg

      mach = 0.0_quad_k

   end select switch_arg

return

!  quad_cmach()

end subroutine quad_cmach

!?>?? ! end text cmach
!?>?? endif
! **********************************************************************

!  define pause() & stop()

! **********************************************************************

!  int_pause(): pause or pause n subroutine

subroutine int_pause( n)

integer, intent( in), optional :: n

!  int_pause()

continue

   n_arg: if( present( n) )then

      write( unit= *, fmt= ps_fmt_ai) ' pause: ', n

   else n_arg

      write( unit= *, fmt= ps_fmt_a) ' pause'

   endif n_arg

   read( unit= *, fmt= *)

return

!  int_pause()

end subroutine int_pause

! ----------------------------------------------------------------------

!  char_pause(): pause 'string' subroutine

subroutine char_pause( string)

character( len= *), intent( in) :: string

!  char_pause()

continue

   write( unit= *, fmt= ps_fmt_aa) ' pause: ', trim( string)

   read( unit= *, fmt= *)

return

!  char_pause()

end subroutine char_pause

! **********************************************************************

!  int_stop(): stop or stop n subroutine

subroutine int_stop( n)

integer, intent( in), optional :: n

!  int_stop()

continue

   n_arg: if( present( n) )then

      write( unit= *, fmt= ps_fmt_ai) ' stop: ', n

   else n_arg

      write( unit= *, fmt= ps_fmt_a) ' stop'

   endif n_arg

   call exit( 0)

return

!  int_stop()

end subroutine int_stop

! ----------------------------------------------------------------------

!  char_stop(): stop 'string' subroutine

subroutine char_stop( string)

character( len= *), intent( in) :: string

!  char_stop()

continue

   write( unit= *, fmt= ps_fmt_aa) ' stop: ', trim( string)

   call exit( 0)

return

!  char_stop()

end subroutine char_stop

! **********************************************************************

!  swap()

!?>?? text :: swap( type, kind)
!?>! **********************************************************************
!?>
!?>!  ?kind?_?type?_swap()
!?>
!?>elemental subroutine ?kind?_?type?_swap( a, b)
!?>
!?>?type?( kind= ?kind?_k), intent( inout) :: a, b
!?>
!?>!  ?kind?_?type?_swap() local
!?>
!?>   ?type?( kind= ?kind?_k) :: t1, t2
!?>
!?>!  ?kind?_?type?_swap()
!?>
!?>continue
!?>
!?>   t1 = a
!?>
!?>   t2 = b
!?>
!?>   b = t1
!?>
!?>   a = t2
!?>
!?>return
!?>
!?>!  ?kind?_?type?_swap()
!?>
!?>end subroutine ?kind?_?type?_swap
!?>
!?>?? end text swap
!?>?? if( ascii_k )then
!?>?? copy :: swap( character, ascii)
!?>?? ! text swap
! **********************************************************************

!  ascii_character_swap()

elemental subroutine ascii_character_swap( a, b)

character( kind= ascii_k), intent( inout) :: a, b

!  ascii_character_swap() local

   character( kind= ascii_k) :: t1, t2

!  ascii_character_swap()

continue

   t1 = a

   t2 = b

   b = t1

   a = t2

return

!  ascii_character_swap()

end subroutine ascii_character_swap

!?>?? ! end text swap
!?>?? endif
!?>?? if( ebcdic_k )then
!?>?? copy :: swap( character, ebcdic)
!?>?? endif
!?>?? if( byte_k )then
!?>?? copy :: swap( integer, byte)
!?>?? ! text swap
! **********************************************************************

!  byte_integer_swap()

elemental subroutine byte_integer_swap( a, b)

integer( kind= byte_k), intent( inout) :: a, b

!  byte_integer_swap() local

   integer( kind= byte_k) :: t1, t2

!  byte_integer_swap()

continue

   t1 = a

   t2 = b

   b = t1

   a = t2

return

!  byte_integer_swap()

end subroutine byte_integer_swap

!?>?? ! end text swap
!?>?? endif
!?>?? if( short_k )then
!?>?? copy :: swap( integer, short)
!?>?? ! text swap
! **********************************************************************

!  short_integer_swap()

elemental subroutine short_integer_swap( a, b)

integer( kind= short_k), intent( inout) :: a, b

!  short_integer_swap() local

   integer( kind= short_k) :: t1, t2

!  short_integer_swap()

continue

   t1 = a

   t2 = b

   b = t1

   a = t2

return

!  short_integer_swap()

end subroutine short_integer_swap

!?>?? ! end text swap
!?>?? endif
!?>?? if( int_k )then
!?>?? copy :: swap( integer, int)
!?>?? ! text swap
! **********************************************************************

!  int_integer_swap()

elemental subroutine int_integer_swap( a, b)

integer( kind= int_k), intent( inout) :: a, b

!  int_integer_swap() local

   integer( kind= int_k) :: t1, t2

!  int_integer_swap()

continue

   t1 = a

   t2 = b

   b = t1

   a = t2

return

!  int_integer_swap()

end subroutine int_integer_swap

!?>?? ! end text swap
!?>?? endif
!?>?? if( long_k )then
!?>?? copy :: swap( integer, long)
!?>?? ! text swap
! **********************************************************************

!  long_integer_swap()

elemental subroutine long_integer_swap( a, b)

integer( kind= long_k), intent( inout) :: a, b

!  long_integer_swap() local

   integer( kind= long_k) :: t1, t2

!  long_integer_swap()

continue

   t1 = a

   t2 = b

   b = t1

   a = t2

return

!  long_integer_swap()

end subroutine long_integer_swap

!?>?? ! end text swap
!?>?? endif
!?>?? if( l_byte_k )then
!?>?? copy :: swap( logical, l_byte)
!?>?? ! text swap
! **********************************************************************

!  l_byte_logical_swap()

elemental subroutine l_byte_logical_swap( a, b)

logical( kind= l_byte_k), intent( inout) :: a, b

!  l_byte_logical_swap() local

   logical( kind= l_byte_k) :: t1, t2

!  l_byte_logical_swap()

continue

   t1 = a

   t2 = b

   b = t1

   a = t2

return

!  l_byte_logical_swap()

end subroutine l_byte_logical_swap

!?>?? ! end text swap
!?>?? endif
!?>?? if( l_short_k )then
!?>?? copy :: swap( logical, l_short)
!?>?? ! text swap
! **********************************************************************

!  l_short_logical_swap()

elemental subroutine l_short_logical_swap( a, b)

logical( kind= l_short_k), intent( inout) :: a, b

!  l_short_logical_swap() local

   logical( kind= l_short_k) :: t1, t2

!  l_short_logical_swap()

continue

   t1 = a

   t2 = b

   b = t1

   a = t2

return

!  l_short_logical_swap()

end subroutine l_short_logical_swap

!?>?? ! end text swap
!?>?? endif
!?>?? if( l_int_k )then
!?>?? copy :: swap( logical, l_int)
!?>?? ! text swap
! **********************************************************************

!  l_int_logical_swap()

elemental subroutine l_int_logical_swap( a, b)

logical( kind= l_int_k), intent( inout) :: a, b

!  l_int_logical_swap() local

   logical( kind= l_int_k) :: t1, t2

!  l_int_logical_swap()

continue

   t1 = a

   t2 = b

   b = t1

   a = t2

return

!  l_int_logical_swap()

end subroutine l_int_logical_swap

!?>?? ! end text swap
!?>?? endif
!?>?? if( l_long_k )then
!?>?? copy :: swap( logical, l_long)
!?>?? ! text swap
! **********************************************************************

!  l_long_logical_swap()

elemental subroutine l_long_logical_swap( a, b)

logical( kind= l_long_k), intent( inout) :: a, b

!  l_long_logical_swap() local

   logical( kind= l_long_k) :: t1, t2

!  l_long_logical_swap()

continue

   t1 = a

   t2 = b

   b = t1

   a = t2

return

!  l_long_logical_swap()

end subroutine l_long_logical_swap

!?>?? ! end text swap
!?>?? endif
!?>?? if( single_k )then
!?>?? copy :: swap( real, single)
!?>?? ! text swap
! **********************************************************************

!  single_real_swap()

elemental subroutine single_real_swap( a, b)

real( kind= single_k), intent( inout) :: a, b

!  single_real_swap() local

   real( kind= single_k) :: t1, t2

!  single_real_swap()

continue

   t1 = a

   t2 = b

   b = t1

   a = t2

return

!  single_real_swap()

end subroutine single_real_swap

!?>?? ! end text swap
!?>?? endif
!?>?? if( double_k )then
!?>?? copy :: swap( real, double)
!?>?? ! text swap
! **********************************************************************

!  double_real_swap()

elemental subroutine double_real_swap( a, b)

real( kind= double_k), intent( inout) :: a, b

!  double_real_swap() local

   real( kind= double_k) :: t1, t2

!  double_real_swap()

continue

   t1 = a

   t2 = b

   b = t1

   a = t2

return

!  double_real_swap()

end subroutine double_real_swap

!?>?? ! end text swap
!?>?? endif
!?>?? if( quad_k )then
!?>?? copy :: swap( real, quad)
!?>?? ! text swap
! **********************************************************************

!  quad_real_swap()

elemental subroutine quad_real_swap( a, b)

real( kind= quad_k), intent( inout) :: a, b

!  quad_real_swap() local

   real( kind= quad_k) :: t1, t2

!  quad_real_swap()

continue

   t1 = a

   t2 = b

   b = t1

   a = t2

return

!  quad_real_swap()

end subroutine quad_real_swap

!?>?? ! end text swap
!?>?? endif
!?>?? if( single_k )then
!?>?? copy :: swap( complex, single)
!?>?? ! text swap
! **********************************************************************

!  single_complex_swap()

elemental subroutine single_complex_swap( a, b)

complex( kind= single_k), intent( inout) :: a, b

!  single_complex_swap() local

   complex( kind= single_k) :: t1, t2

!  single_complex_swap()

continue

   t1 = a

   t2 = b

   b = t1

   a = t2

return

!  single_complex_swap()

end subroutine single_complex_swap

!?>?? ! end text swap
!?>?? endif
!?>?? if( double_k )then
!?>?? copy :: swap( complex, double)
!?>?? ! text swap
! **********************************************************************

!  double_complex_swap()

elemental subroutine double_complex_swap( a, b)

complex( kind= double_k), intent( inout) :: a, b

!  double_complex_swap() local

   complex( kind= double_k) :: t1, t2

!  double_complex_swap()

continue

   t1 = a

   t2 = b

   b = t1

   a = t2

return

!  double_complex_swap()

end subroutine double_complex_swap

!?>?? ! end text swap
!?>?? endif
!?>?? if( quad_k )then
!?>?? copy :: swap( complex, quad)
!?>?? ! text swap
! **********************************************************************

!  quad_complex_swap()

elemental subroutine quad_complex_swap( a, b)

complex( kind= quad_k), intent( inout) :: a, b

!  quad_complex_swap() local

   complex( kind= quad_k) :: t1, t2

!  quad_complex_swap()

continue

   t1 = a

   t2 = b

   b = t1

   a = t2

return

!  quad_complex_swap()

end subroutine quad_complex_swap

!?>?? ! end text swap
!?>?? endif
! **********************************************************************

!  rev_endian()

!?>?? if( short_k )then
! **********************************************************************

!  short_rev_endian()

elemental integer( kind= short_k) function short_rev_endian( a)

integer( kind= short_k), intent( in) :: a

!  short_rev_endian() local

   integer, parameter :: num_bytes = bit_size( 0_short_k) / bit_size( 0_byte_k)

   integer( kind= byte_k), dimension( num_bytes) :: tmp

!  short_rev_endian()

continue

   tmp = transfer( a, tmp)

   tmp( 1: num_bytes) = tmp( num_bytes: 1: -1)

   short_rev_endian = transfer( tmp, short_rev_endian)

return

!  short_rev_endian()

end function short_rev_endian

!?>?? endif
!?>?? if( int_k )then
! ----------------------------------------------------------------------

!  int_rev_endian()

elemental integer( kind= int_k) function int_rev_endian( a)

integer( kind= int_k), intent( in) :: a

!  int_rev_endian() local

   integer, parameter :: num_bytes = bit_size( 0_int_k) / bit_size( 0_byte_k)

   integer( kind= byte_k), dimension( num_bytes) :: tmp

!  int_rev_endian()

continue

   tmp = transfer( a, tmp)

   tmp( 1: num_bytes) = tmp( num_bytes: 1: -1)

   int_rev_endian = transfer( tmp, int_rev_endian)

return

!  int_rev_endian()

end function int_rev_endian

!?>?? endif
!?>?? if( long_k )then
! ----------------------------------------------------------------------

!  long_rev_endian()

elemental integer( kind= long_k) function long_rev_endian( a)

integer( kind= long_k), intent( in) :: a

!  long_rev_endian() local

   integer, parameter :: num_bytes = bit_size( 0_long_k) / bit_size( 0_byte_k)

   integer( kind= byte_k), dimension( num_bytes) :: tmp

!  long_rev_endian()

continue

   tmp = transfer( a, tmp)

   tmp( 1: num_bytes) = tmp( num_bytes: 1: -1)

   long_rev_endian = transfer( tmp, long_rev_endian)

return

!  long_rev_endian()

end function long_rev_endian

!?>?? endif
!?>?? if( l_short_k )then
! ----------------------------------------------------------------------

!  l_short_rev_endian()

elemental logical( kind= l_short_k) function l_short_rev_endian( a)

logical( kind= l_short_k), intent( in) :: a

!  l_short_rev_endian() local

   integer, parameter :: num_bytes = csu_per_nsu

   integer( kind= byte_k), dimension( num_bytes) :: tmp

!  l_short_rev_endian()

continue

   tmp = transfer( a, tmp)

   tmp( 1: num_bytes) = tmp( num_bytes: 1: -1)

   l_short_rev_endian = transfer( tmp, l_short_rev_endian)

return

!  l_short_rev_endian()

end function l_short_rev_endian

!?>?? endif
!?>?? if( l_int_k )then
! ----------------------------------------------------------------------

!  l_int_rev_endian()

elemental logical( kind= l_int_k) function l_int_rev_endian( a)

logical( kind= l_int_k), intent( in) :: a

!  l_int_rev_endian() local

   integer, parameter :: num_bytes = csu_per_nsu

   integer( kind= byte_k), dimension( num_bytes) :: tmp

!  l_int_rev_endian()

continue

   tmp = transfer( a, tmp)

   tmp( 1: num_bytes) = tmp( num_bytes: 1: -1)

   l_int_rev_endian = transfer( tmp, l_int_rev_endian)

return

!  l_int_rev_endian()

end function l_int_rev_endian

!?>?? endif
!?>?? if( l_long_k )then
! ----------------------------------------------------------------------

!  l_long_rev_endian()

elemental logical( kind= l_long_k) function l_long_rev_endian( a)

logical( kind= l_long_k), intent( in) :: a

!  l_long_rev_endian() local

   integer, parameter :: num_bytes = csu_per_nsu

   integer( kind= byte_k), dimension( num_bytes) :: tmp

!  l_long_rev_endian()

continue

   tmp = transfer( a, tmp)

   tmp( 1: num_bytes) = tmp( num_bytes: 1: -1)

   l_long_rev_endian = transfer( tmp, l_long_rev_endian)

return

!  l_long_rev_endian()

end function l_long_rev_endian

!?>?? endif
!?>?? if( single_k )then
! ----------------------------------------------------------------------

!  single_rev_endian()

elemental real( kind= single_k) function single_rev_endian( a)

real( kind= single_k), intent( in) :: a

!  single_rev_endian() local

   integer, parameter :: num_bytes = csu_per_nsu

   integer( kind= byte_k), dimension( csu_per_nsu) :: tmp

!  single_rev_endian()

continue

   tmp = transfer( a, tmp)

   tmp( 1: num_bytes) = tmp( num_bytes: 1: -1)

   single_rev_endian = transfer( tmp, single_rev_endian)

return

!  single_rev_endian()

end function single_rev_endian

!?>?? endif
!?>?? if( double_k )then
! ----------------------------------------------------------------------

!  double_rev_endian()

elemental real( kind= double_k) function double_rev_endian( a)

real( kind= double_k), intent( in) :: a

!  double_rev_endian() local

   integer, parameter :: num_bytes = 2 * csu_per_nsu

   integer( kind= byte_k), dimension( num_bytes) :: tmp

!  double_rev_endian()

continue

   tmp = transfer( a, tmp)

   tmp( 1: num_bytes) = tmp( num_bytes: 1: -1)

   double_rev_endian= transfer( tmp, double_rev_endian)

return

!  double_rev_endian()

end function double_rev_endian

!?>?? endif
!?>?? if( quad_k )then
! ----------------------------------------------------------------------

!  quad_rev_endian()

elemental real( kind= quad_k) function quad_rev_endian( a)

real( kind= quad_k), intent( in) :: a

!  quad_rev_endian() local

   integer, parameter :: num_bytes = 4 * csu_per_nsu

   integer( kind= byte_k), dimension( num_bytes) :: tmp

!  quad_rev_endian()

continue

   tmp = transfer( a, tmp)

   tmp( 1: num_bytes) = tmp( num_bytes: 1: -1)

   quad_rev_endian= transfer( tmp, quad_rev_endian)

return

!  quad_rev_endian()

end function quad_rev_endian

!?>?? endif
!?>?? if( single_k )then
! ----------------------------------------------------------------------

!  single_complex_rev_endian()

elemental complex( kind= single_k) function single_complex_rev_endian( a)

complex( kind= single_k), intent( in) :: a

!  single_complex_rev_endian() local

   integer, parameter :: num_bytes = 2 * csu_per_nsu

   integer( kind= byte_k), dimension( num_bytes) :: tmp

!  single_complex_rev_endian()

continue

   tmp = transfer( a, tmp)

   tmp( 1: num_bytes) = tmp( num_bytes: 1: -1)

   single_complex_rev_endian = transfer( tmp, single_complex_rev_endian)

return

!  single_complex_rev_endian()

end function single_complex_rev_endian

!?>?? endif
!?>?? if( double_k )then
! ----------------------------------------------------------------------

!  double_complex_rev_endian()

elemental complex( kind= double_k) function double_complex_rev_endian( a)

complex( kind= double_k), intent( in) :: a

!  double_complex_rev_endian() local

   integer, parameter :: num_bytes = 4 * csu_per_nsu

   integer( kind= byte_k), dimension( num_bytes) :: tmp

!  double_complex_rev_endian()

continue

   tmp = transfer( a, tmp)

   tmp( 1: num_bytes) = tmp( num_bytes: 1: -1)

   double_complex_rev_endian = transfer( tmp, double_complex_rev_endian)

return

!  double_complex_rev_endian()

end function double_complex_rev_endian

!?>?? endif
!?>?? if( quad_k )then
! ----------------------------------------------------------------------

!  quad_complex_rev_endian()

elemental complex( kind= quad_k) function quad_complex_rev_endian( a)

complex( kind= quad_k), intent( in) :: a

!  quad_complex_rev_endian() local

   integer, parameter :: num_bytes = 8 * csu_per_nsu

   integer( kind= byte_k), dimension( num_bytes) :: tmp

!  quad_complex_rev_endian()

continue

   tmp = transfer( a, tmp)

   tmp( 1: num_bytes) = tmp( num_bytes: 1: -1)

   quad_complex_rev_endian = transfer( tmp, quad_complex_rev_endian)

return

!  quad_complex_rev_endian()

end function quad_complex_rev_endian

!?>?? endif
! **********************************************************************

!  rev_bits()

!?>?? if( ascii_k )then
! **********************************************************************

!  ascii_rev_bits()

elemental character( kind= ascii_k) function ascii_rev_bits( c)

character( len= 1, kind= ascii_k), intent( in) :: c

!  ascii_rev_buts() local

   integer( kind= byte_k) :: loci

!  ascii_rev_bits()

continue

   loci = 0

   loci = ior( loci, ishft( iand( int( ichar( c), kind= byte_k), bit_1), sh_1))
   loci = ior( loci, ishft( iand( int( ichar( c), kind= byte_k), bit_2), sh_2))
   loci = ior( loci, ishft( iand( int( ichar( c), kind= byte_k), bit_3), sh_3))
   loci = ior( loci, ishft( iand( int( ichar( c), kind= byte_k), bit_4), sh_4))
   loci = ior( loci, ishft( iand( int( ichar( c), kind= byte_k), bit_5), sh_5))
   loci = ior( loci, ishft( iand( int( ichar( c), kind= byte_k), bit_6), sh_6))
   loci = ior( loci, ishft( iand( int( ichar( c), kind= byte_k), bit_7), sh_7))
   loci = ior( loci, ishft( iand( int( ichar( c), kind= byte_k), bit_8), sh_8))

   ascii_rev_bits = char( loci)

return

!  ascii_rev_bits()

end function ascii_rev_bits

!?>?? endif
!?>?? if( ebcdic_k )then
!?>! **********************************************************************
!?>
!?>!  ebcdic_rev_bits()
!?>
!?>elemental character( kind= ebcdic_k) function ebcdic_rev_bits( c)
!?>
!?>character( len= 1, kind= ebcdic_k), intent( in) :: c
!?>
!?>!  ebcdic_rev_buts() local
!?>
!?>   integer( kind= byte_k) :: loci
!?>
!?>!  ebcdic_rev_bits()
!?>
!?>continue
!?>
!?>   loci = 0
!?>
!?>   loci = ior( loci, ishft( iand( int( ichar( c), kind= byte_k), bit_1), sh_1))
!?>   loci = ior( loci, ishft( iand( int( ichar( c), kind= byte_k), bit_2), sh_2))
!?>   loci = ior( loci, ishft( iand( int( ichar( c), kind= byte_k), bit_3), sh_3))
!?>   loci = ior( loci, ishft( iand( int( ichar( c), kind= byte_k), bit_4), sh_4))
!?>   loci = ior( loci, ishft( iand( int( ichar( c), kind= byte_k), bit_5), sh_5))
!?>   loci = ior( loci, ishft( iand( int( ichar( c), kind= byte_k), bit_6), sh_6))
!?>   loci = ior( loci, ishft( iand( int( ichar( c), kind= byte_k), bit_7), sh_7))
!?>   loci = ior( loci, ishft( iand( int( ichar( c), kind= byte_k), bit_8), sh_8))
!?>
!?>   ebcdic_rev_bits = char( loci)
!?>
!?>return
!?>
!?>!  ebcdic_rev_bits()
!?>
!?>end function ebcdic_rev_bits
!?>
!?>?? endif
!?>?? if( byte_k )then
! ----------------------------------------------------------------------

!  byte_rev_bits()

elemental integer( kind= byte_k) function byte_rev_bits( i)

integer( kind= byte_k), intent( in) :: i

!  byte_rev_buts() local

   integer( kind= byte_k) :: loci

!  byte_rev_bits()

continue

   loci = 0

   loci = ior( loci, ishft( iand( i, bit_1), sh_1))
   loci = ior( loci, ishft( iand( i, bit_2), sh_2))
   loci = ior( loci, ishft( iand( i, bit_3), sh_3))
   loci = ior( loci, ishft( iand( i, bit_4), sh_4))
   loci = ior( loci, ishft( iand( i, bit_5), sh_5))
   loci = ior( loci, ishft( iand( i, bit_6), sh_6))
   loci = ior( loci, ishft( iand( i, bit_7), sh_7))
   loci = ior( loci, ishft( iand( i, bit_8), sh_8))

   byte_rev_bits = loci

return

!  rev_bits()

end function byte_rev_bits

!?>?? endif
!?>?? if( l_byte_k )then
! ----------------------------------------------------------------------

!  l_byte_rev_bits()

elemental logical( kind= l_byte_k) function l_byte_rev_bits( i)

logical( kind= l_byte_k), intent( in) :: i

!  l_byte_rev_buts() local

   integer( kind= byte_k) :: loci, locj

!  l_byte_rev_bits()

continue

   loci = 0
   locj = transfer( i, loci)

   loci = ior( loci, ishft( iand( locj, bit_1), sh_1))
   loci = ior( loci, ishft( iand( locj, bit_2), sh_2))
   loci = ior( loci, ishft( iand( locj, bit_3), sh_3))
   loci = ior( loci, ishft( iand( locj, bit_4), sh_4))
   loci = ior( loci, ishft( iand( locj, bit_5), sh_5))
   loci = ior( loci, ishft( iand( locj, bit_6), sh_6))
   loci = ior( loci, ishft( iand( locj, bit_7), sh_7))
   loci = ior( loci, ishft( iand( locj, bit_8), sh_8))

   l_byte_rev_bits = transfer( loci, l_byte_rev_bits)

return

!  rev_bits()

end function l_byte_rev_bits

!?>?? endif
! **********************************************************************

!  find io unit which may be opened

!  upon return: if >0, an io unit to open; else -1

! **********************************************************************

!  integer function get_logical_unit()

integer function get_logical_unit( min_unit, max_unit, exclude)

integer, optional, intent( in) :: min_unit
integer, optional, intent( in) :: max_unit

integer, optional, dimension(:), intent( in) :: exclude

!  inquire return values

   integer :: io_stat, io_unit

   logical :: is_open, is_unit

!  local min_unit, max_unit

   integer :: l_min, l_max

!  get_logical_unit()

continue

!  prepare search limits

   set_min: if( present( min_unit) )then

      l_min = min_unit

   else set_min

      l_min = 0

   endif set_min

   set_max: if( present( max_unit) )then

      l_max = max_unit

   else set_max

      l_max = huge( 0)

   endif set_max

!  search loop

   io_units:   do io_unit = l_min, l_max

      inquire( unit= io_unit, opened= is_open, exist= is_unit, iostat= io_stat)

      error:   if( io_stat > 0 )then

         get_logical_unit = -1

         return

      endif error

      ready:   if( is_unit .and. (.not. is_open) )then

         not_list: if( present( exclude) )then

            on_list: if( any( io_unit == exclude) )then

               cycle io_units

            endif on_list

         endif not_list

         get_logical_unit = io_unit

         return

      endif ready

   enddo io_units

!  none found

   get_logical_unit = -1

return

!  get_logical_unit()

end function get_logical_unit

! **********************************************************************

!  standard_functions

! $Id: stdfunc.fpp 1.3 2003/10/03 19:41:32Z Dan Release $
! **********************************************************************

end module standard_functions
!?>?? This was produced using the following SET file
