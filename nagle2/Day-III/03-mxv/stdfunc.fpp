! bof
! **********************************************************************
! Fortran 95 module standard_functions

! **********************************************************************
! Source Control Strings

! $Id: stdfunc.fpp 1.3 2003/10/03 19:41:32Z Dan Release $

! **********************************************************************
! Copyright 2009 Purple Sage Computing Solutions, Inc.

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
!                               send email to dannagle@verizon.net
!                                  or mail to 4311-G Bob Ct.
!                                             Fairfax, VA 22030 USA

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

!     start_rng() starts the rng

!     timestamp() writes a time-stamped message to a file

!     pause() subroutine pause() | pause( char*(*)) | pause( integer)

!     swap() for all tk
!     rev_endian() for all tk size > 1 byte
!     rev_bits() for all tk size = 1 byte

!     get_logical_unit() a logical i/o unit number which may be opened

?? ! *******************************************************************

?? ! preprocessor dependencies

?? include 'coco.inc'

?? ! *******************************************************************

! **********************************************************************

module standard_functions

! **********************************************************************

! use kind parameters

use standard_types

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

?? if( byte_k )then
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

?? endif
! **********************************************************************

!  standard_functions library

! **********************************************************************

!  declare specific functions implementing the .xor. operator

public :: operator( .xor.)

interface operator( .xor.)
?? if( l_byte_k )then
   module procedure l_byte_xor
?? endif
?? if( l_short_k )then
   module procedure l_short_xor
?? endif
?? if( l_int_k )then
   module procedure l_int_xor
?? endif
?? if( l_long_k )then
   module procedure l_long_xor
?? endif
end interface

! **********************************************************************

!  declare specific functions implementing the .mod. operator

public :: operator( .mod.)

interface operator( .mod.)
?? if( byte_k )then
   module procedure byte_mod
?? endif
?? if( short_k )then
   module procedure short_mod
?? endif
?? if( int_k )then
   module procedure int_mod
?? endif
?? if( long_k )then
   module procedure long_mod
?? endif
?? if( single_k )then
   module procedure single_mod
?? endif
?? if( double_k )then
   module procedure double_mod
?? endif
?? if( quad_k )then
   module procedure quad_mod
?? endif
end interface

!  declare specific functions implementing the .modulo. operator

public :: operator( .modulo.)

interface operator( .modulo.)
?? if( byte_k )then
   module procedure byte_modulo
?? endif
?? if( short_k )then
   module procedure short_modulo
?? endif
?? if( int_k )then
   module procedure int_modulo
?? endif
?? if( long_k )then
   module procedure long_modulo
?? endif
?? if( single_k )then
   module procedure single_modulo
?? endif
?? if( double_k )then
   module procedure double_modulo
?? endif
?? if( quad_k )then
   module procedure quad_modulo
?? endif
end interface

! **********************************************************************

!  declare specific functions implementing the .gcd. operator

public :: operator( .gcd.)

interface operator( .gcd.)
?? if( byte_k )then
   module procedure byte_gcd
?? endif
?? if( short_k )then
   module procedure short_gcd
?? endif
?? if( int_k )then
   module procedure int_gcd
?? endif
?? if( long_k )then
   module procedure long_gcd
?? endif
end interface

!  declare specific functions implementing the gcd() function

public :: gcd

interface gcd
?? if( byte_k )then
   module procedure byte_gcd
?? endif
?? if( short_k )then
   module procedure short_gcd
?? endif
?? if( int_k )then
   module procedure int_gcd
?? endif
?? if( long_k )then
   module procedure long_gcd
?? endif
end interface

!  declare specific functions implementing the .lcm. operator

public :: operator( .lcm.)

interface operator( .lcm.)
?? if( byte_k )then
   module procedure byte_lcm
?? endif
?? if( short_k )then
   module procedure short_lcm
?? endif
?? if( int_k )then
   module procedure int_lcm
?? endif
?? if( long_k )then
   module procedure long_lcm
?? endif
end interface

!  declare specific functions implementing the lcm() function

public :: lcm

interface lcm
?? if( byte_k )then
   module procedure byte_lcm
?? endif
?? if( short_k )then
   module procedure short_lcm
?? endif
?? if( int_k )then
   module procedure int_lcm
?? endif
?? if( long_k )then
   module procedure long_lcm
?? endif
end interface

! **********************************************************************

!  declare specific functions implementing the .cd. operator

public :: operator( .cd.)

interface operator( .cd.)
?? if( byte_k )then
   module procedure byte_cd
?? endif
?? if( short_k )then
   module procedure short_cd
?? endif
?? if( int_k )then
   module procedure int_cd
?? endif
?? if( long_k )then
   module procedure long_cd
?? endif
end interface

!  declare specific functions implementing the cd() function

public :: cd

interface cd
?? if( byte_k )then
   module procedure byte_cd
?? endif
?? if( short_k )then
   module procedure short_cd
?? endif
?? if( int_k )then
   module procedure int_cd
?? endif
?? if( long_k )then
   module procedure long_cd
?? endif
end interface

!  declare specific functions implementing the .cr. operator

public :: operator( .cr.)

interface operator( .cr.)
?? if( byte_k )then
   module procedure byte_cr
?? endif
?? if( short_k )then
   module procedure short_cr
?? endif
?? if( int_k )then
   module procedure int_cr
?? endif
?? if( long_k )then
   module procedure long_cr
?? endif
end interface

!  declare specific functions implementing the cr() function

public :: cr

interface cr
?? if( byte_k )then
   module procedure byte_cr
?? endif
?? if( short_k )then
   module procedure short_cr
?? endif
?? if( int_k )then
   module procedure int_cr
?? endif
?? if( long_k )then
   module procedure long_cr
?? endif
end interface

! **********************************************************************

!  integer diagnostic functions

! **********************************************************************

!  declare specific functions implementing the iseven() function

public :: iseven

interface iseven
?? if( byte_k )then
   module procedure byte_iseven
?? endif
?? if( short_k )then
   module procedure short_iseven
?? endif
?? if( int_k )then
   module procedure int_iseven
?? endif
?? if( long_k )then
   module procedure long_iseven
?? endif
end interface

! **********************************************************************

!  declare specific functions implementing the isodd() function

public :: isodd

interface isodd
?? if( byte_k )then
   module procedure byte_isodd
?? endif
?? if( short_k )then
   module procedure short_isodd
?? endif
?? if( int_k )then
   module procedure int_isodd
?? endif
?? if( long_k )then
   module procedure long_isodd
?? endif
end interface

! **********************************************************************

!  real to integer functions

! **********************************************************************

!  declare specific functions implementing the rrint() function

public :: rrint

interface rrint
?? if( single_k )then
   module procedure single_rrint
?? endif
?? if( double_k )then
   module procedure double_rrint
?? endif
?? if( quad_k )then
   module procedure quad_rrint
?? endif
end interface

! **********************************************************************

!  rng starter functions

! **********************************************************************

!  declare specific function supporting generic function start_rng()

public :: start_rng

interface start_rng
?? if( single_k )then
   module procedure single_start_rng
?? endif
?? if( double_k )then
   module procedure double_start_rng
?? endif
?? if( quad_k )then
   module procedure quad_start_rng
?? endif
end interface

! **********************************************************************

!  subroutines to replace pause statements

! **********************************************************************

!  declare specific subroutines supporting generic subroutine pause()

public :: pause

interface pause
   module procedure int_pause
   module procedure char_pause
end interface

! **********************************************************************

!  subroutines to implement the swap() routine

! **********************************************************************

!  declare specific subroutines supporting swap()

public :: swap

interface swap
?? if( ascii_k )then
   module procedure ascii_character_swap
?? endif
?? if( ebcdic_k )then
   module procedure ebcdic_character_swap
?? endif
?? if( byte_k )then
   module procedure byte_integer_swap
?? endif
?? if( short_k )then
   module procedure short_integer_swap
?? endif
?? if( int_k )then
   module procedure int_integer_swap
?? endif
?? if( long_k )then
   module procedure long_integer_swap
?? endif
?? if( l_byte_k )then
   module procedure l_byte_logical_swap
?? endif
?? if( l_short_k )then
   module procedure l_short_logical_swap
?? endif
?? if( l_int_k )then
   module procedure l_int_logical_swap
?? endif
?? if( l_long_k )then
   module procedure l_long_logical_swap
?? endif
?? if( single_k )then
   module procedure single_real_swap
?? endif
?? if( double_k )then
   module procedure double_real_swap
?? endif
?? if( quad_k )then
   module procedure quad_real_swap
?? endif
?? if( single_k )then
   module procedure single_complex_swap
?? endif
?? if( double_k )then
   module procedure double_complex_swap
?? endif
?? if( quad_k )then
   module procedure quad_complex_swap
?? endif
end interface

! **********************************************************************

!  subroutines to implement the rev_endian() routine

! **********************************************************************

!  declare specific subroutines supporting rev_endian()

public :: rev_endian

interface rev_endian
?? if( short_k )then
   module procedure short_rev_endian
?? endif
?? if( int_k )then
   module procedure int_rev_endian
?? endif
?? if( long_k )then
   module procedure long_rev_endian
?? endif
?? if( l_short_k )then
   module procedure l_short_rev_endian
?? endif
?? if( l_int_k )then
   module procedure l_int_rev_endian
?? endif
?? if( l_long_k )then
   module procedure l_long_rev_endian
?? endif
?? if( single_k )then
   module procedure single_rev_endian
?? endif
?? if( double_k )then
   module procedure double_rev_endian
?? endif
?? if( quad_k )then
   module procedure quad_rev_endian
?? endif
?? if( single_k )then
   module procedure single_complex_rev_endian
?? endif
?? if( double_k )then
   module procedure double_complex_rev_endian
?? endif
?? if( quad_k )then
   module procedure quad_complex_rev_endian
?? endif
end interface

! **********************************************************************

!  subroutines to implement the rev_bits() routine

! **********************************************************************

!  declare specific subroutines supporting rev_bits()

public :: rev_bits

interface rev_bits
?? if( ascii_k )then
   module procedure ascii_rev_bits
?? endif
?? if( ebcdic_k )then
   module procedure ebcdic_rev_bits
?? endif
?? if( byte_k )then
   module procedure byte_rev_bits
?? endif
?? if( l_byte_k )then
   module procedure l_byte_rev_bits
?? endif
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

?? text :: xor( kind)
! **********************************************************************

!  ?kind?_xor: xor() for kind ?kind?

elemental logical( kind= ?kind?_k) function ?kind?_xor( l1, l2)

logical( kind= ?kind?_k), intent( in) :: l1, l2

!  ?kind?_xor()

continue

   ?kind?_xor = l1 .neqv. l2

return

!  ?kind?_xor()

end function ?kind?_xor

?? end text xor
?? if( l_byte_k )then
?? copy :: xor( l_byte)
?? endif
?? if( l_short_k )then
?? copy :: xor( l_short)
?? endif
?? if( l_int_k )then
?? copy :: xor( l_int)
?? endif
?? if( l_long_k )then
?? copy :: xor( l_long)
?? endif
! **********************************************************************

!  define .mod. binary operator

?? text :: mod( type, kind)
! **********************************************************************

!  ?kind?_mod(): .mod. for kind ?kind?

elemental ?type?( kind= ?kind?_k) function ?kind?_mod( a, p)

?type?( kind= ?kind?_k), intent( in) :: a, p

!  ?kind?_mod()

continue

   ?kind?_mod = mod( a, p)

return

!  ?kind?_mod()

end function ?kind?_mod

?? end text mod
?? if( byte_k )then
?? copy :: mod( integer, byte)
?? endif
?? if( short_k )then
?? copy :: mod( integer, short)
?? endif
?? if( int_k )then
?? copy :: mod( integer, int)
?? endif
?? if( long_k )then
?? copy :: mod( integer, long)
?? endif
?? if( single_k )then
?? copy :: mod( real, single)
?? endif
?? if( double_k )then
?? copy :: mod( real, double)
?? endif
?? if( quad_k )then
?? copy :: mod( real, quad)
?? endif
! **********************************************************************

!  define .modulo. binary operator

?? text :: modulo( type, kind)
! **********************************************************************

!  ?kind?_modulo(): .modulo. for kind ?kind?

elemental ?type?( kind= ?kind?_k) function ?kind?_modulo( a, p)

?type?( kind= ?kind?_k), intent( in) :: a, p

!  ?kind?_modulo()

continue

   ?kind?_modulo = modulo( a, p)

return

!  ?kind?_modulo()

end function ?kind?_modulo

?? end text modulo
?? if( byte_k )then
?? copy :: modulo( integer, byte)
?? endif
?? if( short_k )then
?? copy :: modulo( integer, short)
?? endif
?? if( int_k )then
?? copy :: modulo( integer, int)
?? endif
?? if( long_k )then
?? copy :: modulo( integer, long)
?? endif
?? if( single_k )then
?? copy :: modulo( real, single)
?? endif
?? if( double_k )then
?? copy :: modulo( real, double)
?? endif
?? if( quad_k )then
?? copy :: modulo( real, quad)
?? endif
! **********************************************************************

!  define iseven()/isodd() for integer kinds

?? text :: iseven( kind)
! **********************************************************************

!  ?kind?_iseven(): iseven() for kind ?kind?

elemental logical function ?kind?_iseven( a)

integer( kind= ?kind?_k), intent( in) :: a

!  ?kind?_iseven()

continue

   ?kind?_iseven = iand( a, 1_?kind?_k) == 0_?kind?_k

return

!  ?kind?_iseven()

end function ?kind?_iseven

?? end text iseven
?? if( byte_k )then
?? copy :: iseven( byte)
?? endif
?? if( short_k )then
?? copy :: iseven( short)
?? endif
?? if( int_k )then
?? copy :: iseven( int)
?? endif
?? if( long_k )then
?? copy :: iseven( long)
?? endif
?? text :: isodd( kind)
! **********************************************************************

!  ?kind?_isodd(): isodd() for kind ?kind?

elemental logical function ?kind?_isodd( a)

integer( kind= ?kind?_k), intent( in) :: a

!  ?kind?_isodd()

continue

   ?kind?_isodd = iand( a, 1_?kind?_k) == 1_?kind?_k

return

!  ?kind?_isodd()

end function ?kind?_isodd

?? end text isodd
?? if( byte_k )then
?? copy :: isodd( byte)
?? endif
?? if( short_k )then
?? copy :: isodd( short)
?? endif
?? if( int_k )then
?? copy :: isodd( int)
?? endif
?? if( long_k )then
?? copy :: isodd( long)
?? endif
! **********************************************************************

!  define gcd()/lcm()

?? text :: gcd( kind)
! **********************************************************************

!  ?kind?_gcd() gcd() for kind ?kind?_k

elemental integer( kind= ?kind?_k) function ?kind?_gcd( a, b)

integer( kind= ?kind?_k), intent( in) :: a, b

!  ?kind?_gcd() local

   integer( kind= ?kind?_k) :: a_gcd, b_gcd, rnp1, rn, rnm1

!  ?kind?_gcd()

continue

! if a or b zero, abs( other) is gcd

   zero_a: if( a == 0_?kind?_k )then

      ?kind?_gcd = abs( b)

      return

   endif zero_a

   zero_b: if( b == 0_?kind?_k )then

      ?kind?_gcd = abs( a)

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

   zero_rem: do while( rn /= 0_?kind?_k)

      rnp1 = rnm1 .mod. rn

      rnm1 = rn

      rn = rnp1

   enddo zero_rem

   ?kind?_gcd = rnm1

return

!  ?kind?_gcd()

end function ?kind?_gcd

?? end text gcd
?? if( byte_k )then
?? copy :: gcd( byte)
?? endif
?? if( short_k )then
?? copy :: gcd( short)
?? endif
?? if( int_k )then
?? copy :: gcd( int)
?? endif
?? if( long_k )then
?? copy :: gcd( long)
?? endif
! **********************************************************************

!  lcm

?? text :: lcm( kind)
! **********************************************************************

!  ?kind?_lcm() lcm() for kind= ?kind?_k

elemental integer( kind= ?kind?_k) function ?kind?_lcm( a, b)

integer( kind= ?kind?_k), intent( in) :: a, b

!  ?kind?_lcm()

continue

   ?kind?_lcm = ( a* b) / gcd( a, b)

return

!  ?kind?_lcm()

end function ?kind?_lcm

?? end text lcm
?? if( int_k )then
?? copy :: lcm( byte)
?? endif
?? if( short_k )then
?? copy :: lcm( short)
?? endif
?? if( int_k )then
?? copy :: lcm( int)
?? endif
?? if( long_k )then
?? copy :: lcm( long)
?? endif
! **********************************************************************

!  define cd()/cr() for integer kinds

?? text :: cd( kind)
! **********************************************************************

!  ?kind?_cd(): .cd., cd() for kind ?kind?

elemental integer( kind= ?kind?_k) function ?kind?_cd( j, k)

integer( kind= ?kind?_k), intent( in) :: j, k

!  ?kind?_cd()

continue

   ?kind?_cd = ( j + k - 1_?kind?_k) / k

return

!  ?kind?_cd()

end function ?kind?_cd

?? end text cd
?? if( byte_k )then
?? copy :: cd( byte)
?? endif
?? if( short_k )then
?? copy :: cd( short)
?? endif
?? if( int_k )then
?? copy :: cd( int)
?? endif
?? if( long_k )then
?? copy :: cd( long)
?? endif
?? text :: cr( kind)
! **********************************************************************

!  ?kind?_cr(): .cr., cr() for kind ?kind?

elemental integer( kind= ?kind?_k) function ?kind?_cr( j, k)

integer( kind= ?kind?_k), intent( in) :: j, k

!  ?kind?_cr()

continue

   ?kind?_cr = j - k * cd( j, k)

return

!  ?kind?_cr()

end function ?kind?_cr

?? end text cr
?? if( byte_k )then
?? copy :: cr( byte)
?? endif
?? if( short_k )then
?? copy :: cr( short)
?? endif
?? if( int_k )then
?? copy :: cr( int)
?? endif
?? if( long_k )then
?? copy :: cr( long)
?? endif
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

      no_hex: if( jloc == substring_not_found )then

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

      stat = status_aok

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

      no_oct: if( jloc == substring_not_found )then

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

       stat = status_aok

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

      no_bin: if( jloc == substring_not_found )then

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

      stat = status_aok

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

      stat = status_aok

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

      bad_char: if( jstr == substring_not_found )then

         i = 0

         if( present( stat) ) stat = error_not_in_table

         return

      endif bad_char

      i = i*base + ( jstr - 1)

      str_buff = str_buff( 2: )

   enddo each_char

   status_arg: if( present( stat) )then

      stat = status_aok

   endif status_arg

return

!  decode()

end subroutine decode

! **********************************************************************

!  rrint()- randomly round real to integer

?? text :: rrint( kind)
! **********************************************************************

!  ?kind?_rrint: rrint() for kind ?kind?

integer function ?kind?_rrint( a)

real( kind= ?kind?_k), intent( in) :: a

!  ?kind?_rrint() local

   real( kind= ?kind?_k) :: r

   integer :: i

!  ?kind?_rrint()

continue

   overflow: if( int( a) >= huge( i) )then

      ?kind?_rrint = huge( i)

      return

   elseif( int( a) <= -huge( i) )then overflow

      ?kind?_rrint = -huge( i)

      return

   endif overflow

   call random_number( r)

!  down if close to floor, up if close to ceiling

   hi_lo: if( ( a - real( floor( a), kind= ?kind?_k)) <= r )then

      i = floor( a)

   else hi_lo

      i = ceiling( a)

   endif hi_lo

   ?kind?_rrint = i

return

!  ?kind?_rrint()

end function ?kind?_rrint

?? end text rrint
?? if( single_k )then
?? copy :: rrint( single)
?? endif
?? if( double_k )then
?? copy :: rrint( double)
?? endif
?? if( quad_k )then
?? copy :: rrint( quad)
?? endif
! **********************************************************************

!  start_rng() for real kinds

?? text :: start_rng( kind)
! **********************************************************************

!  ?kind?_start_rng(): start_rng() for kind ?kind?

subroutine ?kind?_start_rng( rngs, log_unit)

!  array of random variables

real( kind= ?kind?_k), dimension( :), intent( out) :: rngs

!  unit to receive the seed value

integer, optional, intent( in) :: log_unit

!  format of the seed value

character( len= *), parameter :: seed_fmt = '( a, 99i10)'

!  ?kind?_start_rng() local

   character( len= date_len) :: date_str
   character( len= time_len) :: time_str

   integer, dimension( values_size) :: dt_values

   integer, dimension( :), allocatable :: rng_seed

   integer :: seed_size

!  ?kind?_start_rng()

continue

   call random_seed( size = seed_size)

   allocate( rng_seed( seed_size))

   call date_and_time( date= date_str, time= time_str, values= dt_values)

   rng_seed( 1) = sum( dt_values * dt_values)

   if( seed_size > 1 ) rng_seed( 2) = sum( dt_values * (/ dt_values( 2: seed_size), dt_values( 1) /) )

   if( seed_size > 2 ) rng_seed( 3) = sum( dt_values * (/ dt_values( 3: seed_size), dt_values( 1: 2) /) )

   if( seed_size > 3 ) rng_seed( 4) = sum( dt_values * (/ dt_values( 4: seed_size), dt_values( 1: 3) /) )

   if( seed_size > 4 ) rng_seed( 5) = sum( dt_values * (/ dt_values( 5: seed_size), dt_values( 1: 4) /) )

   if( seed_size > 5 ) rng_seed( 6) = sum( dt_values * (/ dt_values( 6: seed_size), dt_values( 1: 5) /) )

   if( seed_size > 6 ) rng_seed( 7) = sum( dt_values * (/ dt_values( 7: seed_size), dt_values( 1: 6) /) )

   if( seed_size > 7 ) rng_seed( 8) = sum( dt_values * (/ dt_values( 8: seed_size), dt_values( 1: 7) /) )

   if( seed_size > 8 ) rng_seed( 9: ) = 0

   got_unit: if( present( log_unit) )then

      write( unit= log_unit, fmt= seed_fmt) 'random seed is ', rng_seed

   else got_unit

      write( unit= *, fmt= seed_fmt) 'random seed is ', rng_seed

   end if got_unit

   call random_number( harvest= rngs)

return

!  ?kind?_start_rng()

end subroutine ?kind?_start_rng

?? end text start_rng
?? if( single_k )then
?? copy :: start_rng( single)
?? endif
?? if( double_k )then
?? copy :: start_rng( double)
?? endif
?? if( quad_k )then
?? copy :: start_rng( quad)
?? endif
! **********************************************************************

!  define timestamp()

! **********************************************************************

!  timestamp(): write a timestamped message to a log file

subroutine timestamp( log_unit, message)

integer, intent( in), optional :: log_unit

character( len= *), intent( in) :: message

character( len= *), parameter :: message_fmt = '( a)'

character( len= *), parameter :: slash = '/'
character( len= *), parameter :: colon = ':'

!  timestamp() local

   character( len= date_len) :: date_str
   character( len= time_len) :: time_str

!  timestamp()

continue

   call date_and_time( date= date_str, time= time_str)

   got_unit: if( present( log_unit) )then

      write( unit= log_unit, fmt= message_fmt) date_str( 1: 4) // slash // date_str( 5: 6) // date_str( 7: 8) // ' ' &
                                            // time_str( 1: 2) // colon // time_str( 3: 4) // time_str( 5: 6) // '.' time_str( 7: time_len) &
                                            // trim( message)

   else got_unit

      write( unit= *, fmt= message_fmt) date_str( 1: 4) // slash // date_str( 5: 6) // date_str( 7: 8) // ' ' &
                                     // time_str( 1: 2) // colon // time_str( 3: 4) // time_str( 5: 6) // '.' time_str( 7: time_len) &
                                     // trim( message)

   end if got_unit

return

!  timestamp()

end subroutine timestamp

! ----------------------------------------------------------------------

! **********************************************************************

!  define pause()

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

   end if n_arg

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

!  swap()

?? text :: swap( type, kind)
! **********************************************************************

!  ?kind?_?type?_swap()

elemental subroutine ?kind?_?type?_swap( a, b)

?type?( kind= ?kind?_k), intent( inout) :: a, b

!  ?kind?_?type?_swap() local

   ?type?( kind= ?kind?_k) :: t1, t2

!  ?kind?_?type?_swap()

continue

   t1 = a

   t2 = b

   b = t1

   a = t2

return

!  ?kind?_?type?_swap()

end subroutine ?kind?_?type?_swap

?? end text swap
?? if( ascii_k )then
?? copy :: swap( character, ascii)
?? endif
?? if( ebcdic_k )then
?? copy :: swap( character, ebcdic)
?? endif
?? if( byte_k )then
?? copy :: swap( integer, byte)
?? endif
?? if( short_k )then
?? copy :: swap( integer, short)
?? endif
?? if( int_k )then
?? copy :: swap( integer, int)
?? endif
?? if( long_k )then
?? copy :: swap( integer, long)
?? endif
?? if( l_byte_k )then
?? copy :: swap( logical, l_byte)
?? endif
?? if( l_short_k )then
?? copy :: swap( logical, l_short)
?? endif
?? if( l_int_k )then
?? copy :: swap( logical, l_int)
?? endif
?? if( l_long_k )then
?? copy :: swap( logical, l_long)
?? endif
?? if( single_k )then
?? copy :: swap( real, single)
?? endif
?? if( double_k )then
?? copy :: swap( real, double)
?? endif
?? if( quad_k )then
?? copy :: swap( real, quad)
?? endif
?? if( single_k )then
?? copy :: swap( complex, single)
?? endif
?? if( double_k )then
?? copy :: swap( complex, double)
?? endif
?? if( quad_k )then
?? copy :: swap( complex, quad)
?? endif
! **********************************************************************

!  rev_endian()

?? if( short_k )then
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

?? endif
?? if( int_k )then
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

?? endif
?? if( long_k )then
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

?? endif
?? if( l_short_k )then
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

?? endif
?? if( l_int_k )then
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

?? endif
?? if( l_long_k )then
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

?? endif
?? if( single_k )then
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

?? endif
?? if( double_k )then
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

?? endif
?? if( quad_k )then
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

?? endif
?? if( single_k )then
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

?? endif
?? if( double_k )then
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

?? endif
?? if( quad_k )then
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

?? endif
! **********************************************************************

!  rev_bits()

?? if( ascii_k )then
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

?? endif
?? if( ebcdic_k )then
! **********************************************************************

!  ebcdic_rev_bits()

elemental character( kind= ebcdic_k) function ebcdic_rev_bits( c)

character( len= 1, kind= ebcdic_k), intent( in) :: c

!  ebcdic_rev_buts() local

   integer( kind= byte_k) :: loci

!  ebcdic_rev_bits()

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

   ebcdic_rev_bits = char( loci)

return

!  ebcdic_rev_bits()

end function ebcdic_rev_bits

?? endif
?? if( byte_k )then
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

?? endif
?? if( l_byte_k )then
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

?? endif
! **********************************************************************

!  find io unit which may be opened

!  upon return: if >0, an io unit to open; else not_a_unit

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

      error:   if( is_io_error( io_stat) )then

         get_logical_unit = not_a_unit

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

   get_logical_unit = not_a_unit

return

!  get_logical_unit()

end function get_logical_unit

! **********************************************************************

!  standard_functions

! $Id: stdfunc.fpp 1.3 2003/10/03 19:41:32Z Dan Release $
! **********************************************************************

end module standard_functions
