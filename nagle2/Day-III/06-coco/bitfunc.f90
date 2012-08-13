! bof
! **********************************************************************
! Fortran 95 module bit_functions

! **********************************************************************
! Source Control Strings

! $Id: bitfunc.fpp 1.3 2003/10/03 19:28:00Z Dan Release $

! **********************************************************************
! Copyright 2003 Purple Sage Computing Solutions, Inc.

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
!                                   or fax to 703 424 8525 (USA)
!                                  or mail to 4311-G Bob Ct.
!                                             Fairfax, VA 22030 USA

! **********************************************************************
! functions and operators for basic bit-level calculations

! **********************************************************************

!  bit_functions operators

!     .not. bit-wise for integers

!     .and. bit-wise for integers
!     .or. bit-wise for integers
!     .eor. bit-wise for integers

!     .eqv. bit-wise for integers
!     .neqv. bit-wise for integers
!     .xor. bit-wise for integers

!     .hamd. for integers

!     .shift. linear shift operator
!     .rotate. circular shift operator

!  bit_functions library

!     csmg() [ = ( i& k) ! ( j& ~k) ]
!     compl() for integers

!     leadz()
!     lastz()
!     popcnt()
!     poppar() for integers

!     ilen() width (a la HPF)
!     hamd() hamming distance integer( integer, integer)

!     dshftl()
!     dshftr()
!     dshftc() for integers

!     mask()
!     maskl()
!     maskr() for integers





! **********************************************************************

!  bit_functions

module bit_functions

! **********************************************************************
! use standard parameterization of processor dependencies

use standard_types

! **********************************************************************

!  explicit declarations

implicit none

! **********************************************************************

!  explicit export

private

! **********************************************************************

!  RCS strings

! **********************************************************************

character( len= *), parameter :: bit_functions_rcs_source = &
   '$Id: bitfunc.fpp 1.3 2003/10/03 19:28:00Z Dan Release $'

! **********************************************************************

!  bit_functions library

! **********************************************************************

!  bit functions csmg(), compl(), leadz(), lastz(), popcnt(), poppar()

! **********************************************************************

!  declare specific functions supporting generic function csmg()

public :: csmg                                                       ! generic name

interface csmg
   module procedure byte_csmg
   module procedure short_csmg
   module procedure int_csmg
   module procedure long_csmg
end interface

!  declare specific functions implementing the compl() function

public :: compl                                                      ! generic name

interface compl
   module procedure byte_compl
   module procedure short_compl
   module procedure int_compl
   module procedure long_compl
end interface

! **********************************************************************

!  declare specific functions supporting generic function leadz()

public :: leadz                                                      ! generic name

interface leadz
   module procedure byte_leadz
   module procedure short_leadz
   module procedure int_leadz
   module procedure long_leadz
end interface

! **********************************************************************

!  declare specific functions supporting generic function lastz()

public :: lastz                                                      ! generic name

interface lastz
   module procedure byte_lastz
   module procedure short_lastz
   module procedure int_lastz
   module procedure long_lastz
end interface

! **********************************************************************

!  declare specific functions supporting generic function popcnt()

public :: popcnt                                                     ! generic name

interface popcnt
   module procedure byte_popcnt
   module procedure short_popcnt
   module procedure int_popcnt
   module procedure long_popcnt
end interface

! **********************************************************************

!  declare specific functions supporting generic function poppar()

public :: poppar                                                     ! generic name

interface poppar
   module procedure byte_poppar
   module procedure short_poppar
   module procedure int_poppar
   module procedure long_poppar
end interface

! **********************************************************************

!  bit length of an integer .ilen. ilen()

! **********************************************************************

!  declare specific functions supporting .ilen.

public :: operator( .ilen.)                                          ! operator name

interface operator( .ilen.)
   module procedure byte_ilen
   module procedure short_ilen
   module procedure int_ilen
   module procedure long_ilen
end interface

!  declare specific functions supporting generic ilen()

public :: ilen                                                       ! generic name

interface ilen
   module procedure byte_ilen
   module procedure short_ilen
   module procedure int_ilen
   module procedure long_ilen
end interface

! **********************************************************************

!  hamming distance .hamd. hamd()

! **********************************************************************

!  declare specific functions supporting .hamd.

public :: operator( .hamd.)                                          ! operator name

interface operator( .hamd.)
   module procedure byte_hamd
   module procedure short_hamd
   module procedure int_hamd
   module procedure long_hamd
end interface

!  declare specific functions supporting generic hamd()

public :: hamd                                                       ! generic name

interface hamd
   module procedure byte_hamd
   module procedure short_hamd
   module procedure int_hamd
   module procedure long_hamd
end interface

! **********************************************************************

!  mask functions mask(), maskl(), maskr()

! **********************************************************************

!  declare specific functions supporting generic function mask()

public :: mask                                                       ! generic name

interface mask
   module procedure byte_mask
   module procedure short_mask
   module procedure int_mask
   module procedure long_mask
end interface

! **********************************************************************

!  declare specific functions supporting generic function maskl()

public :: maskl                                                      ! generic name

interface maskl
   module procedure byte_maskl
   module procedure short_maskl
   module procedure int_maskl
   module procedure long_maskl
end interface

! **********************************************************************

!  declare specific functions supporting generic function maskr()

public :: maskr                                                      ! generic name

interface maskr
   module procedure byte_maskr
   module procedure short_maskr
   module procedure int_maskr
   module procedure long_maskr
end interface

! **********************************************************************

!  shifts as binary operators .shift. .rotate.

! **********************************************************************

!  declare specific functions supporting .shift.

public :: operator( .shift.)                                         ! operator name

interface operator( .shift.)
   module procedure byte_shift
   module procedure short_shift
   module procedure int_shift
   module procedure long_shift
end interface

! **********************************************************************

!  declare specific functions supporting .rotate.

public :: operator( .rotate.)                                        ! operator name

interface operator( .rotate.)
   module procedure byte_rotate
   module procedure short_rotate
   module procedure int_rotate
   module procedure long_rotate
end interface

! **********************************************************************

!  two word shift functions dshftl(), dshftr(), dshftc()

! **********************************************************************

!  declare specific functions supporting generic function dshftl()

public :: dshftl                                                     ! generic name

interface dshftl
   module procedure byte_dshftl
   module procedure short_dshftl
   module procedure int_dshftl
   module procedure long_dshftl
end interface

! **********************************************************************

!  declare specific functions supporting generic function dshftr()

public :: dshftr                                                     ! generic name

interface dshftr
   module procedure byte_dshftr
   module procedure short_dshftr
   module procedure int_dshftr
   module procedure long_dshftr
end interface

! **********************************************************************

!  declare specific functions supporting generic function dshftc()

public :: dshftc                                                     ! generic name

interface dshftc
   module procedure byte_dshftc
   module procedure short_dshftc
   module procedure int_dshftc
   module procedure long_dshftc
end interface

! **********************************************************************

!  unary operator: .not.

! **********************************************************************

!  declare specific functions implementing the .not. operator

public :: operator( .not.)                                           ! operator name

interface operator( .not.)
   module procedure byte_not
   module procedure short_not
   module procedure int_not
   module procedure long_not
end interface

! **********************************************************************

!  binary operators: .and., .or., .eor., .xor., .eqv., .neqv., .xor.

! **********************************************************************

!  declare specific functions implementing the .and. operator

public :: operator( .and.)                                           ! operator name

interface operator( .and.)
   module procedure byte_and
   module procedure short_and
   module procedure int_and
   module procedure long_and
end interface

!  declare specific functions implementing the .or. operator

public :: operator( .or.)                                            ! operator name

interface operator( .or.)
   module procedure byte_or
   module procedure short_or
   module procedure int_or
   module procedure long_or
end interface

!  declare specific functions implementing the .eor. operator

public :: operator( .eor.)                                           ! operator name

interface operator( .eor.)
   module procedure byte_eor
   module procedure short_eor
   module procedure int_eor
   module procedure long_eor
end interface

!  declare specific functions implementing the .eqv. operator

public :: operator( .eqv.)                                           ! operator name

interface operator( .eqv.)
   module procedure byte_eqv
   module procedure short_eqv
   module procedure int_eqv
   module procedure long_eqv
end interface

!  declare specific functions implementing the .neqv. operator

public :: operator( .neqv.)                                          ! operator name

interface operator( .neqv.)
   module procedure byte_neqv
   module procedure short_neqv
   module procedure int_neqv
   module procedure long_neqv
end interface

!  declare specific functions implementing the .xor. operator

public :: operator( .xor.)                                           ! operator name

interface operator( .xor.)
   module procedure byte_xor
   module procedure short_xor
   module procedure int_xor
   module procedure long_xor
end interface

! **********************************************************************

!  private data

! **********************************************************************

!  mask, maskl, maskr data for byte_k

integer( kind= byte_k), dimension( bit_size( 0_byte_k) ), save :: &
      byte_left_mask, byte_right_mask

data &
    byte_left_mask/ z'80', z'c0', z'e0', z'f0', &
                    z'f8', z'fc', z'fe', z'ff'/

data &
   byte_right_mask/ z'01', z'03', z'07', z'0f', &
                    z'1f', z'3f', z'7f', z'ff'/

!  mask, maskl, maskr data for short_k

integer( kind= short_k), dimension( bit_size( 0_short_k) ), save :: &
      short_left_mask, short_right_mask

data &
   short_left_mask/ z'8000', z'c000', z'e000', z'f000', &
                    z'f800', z'fc00', z'fe00', z'ff00', &
                    z'ff80', z'ffc0', z'ffe0', z'fff0', &
                    z'fff8', z'fffc', z'fffe', z'ffff'/

data &
  short_right_mask/ z'0001', z'0003', z'0007', z'000f', &
                    z'001f', z'003f', z'007f', z'00ff', &
                    z'01ff', z'03ff', z'07ff', z'0fff', &
                    z'1fff', z'3fff', z'7fff', z'ffff'/

!  mask, maskl, maskr data for int_k

integer( kind= int_k), dimension( bit_size( 0_int_k) ), save :: &
      int_left_mask, int_right_mask

data &
     int_left_mask/ z'80000000', z'c0000000', z'e0000000', z'f0000000', &
                    z'f8000000', z'fc000000', z'fe000000', z'ff000000', &
                    z'ff800000', z'ffc00000', z'ffe00000', z'fff00000', &
                    z'fff80000', z'fffc0000', z'fffe0000', z'ffff0000', &
                    z'ffff8000', z'ffffc000', z'ffffe000', z'fffff000', &
                    z'fffff800', z'fffffc00', z'fffffe00', z'ffffff00', &
                    z'ffffff80', z'ffffffc0', z'ffffffe0', z'fffffff0', &
                    z'fffffff8', z'fffffffc', z'fffffffe', z'ffffffff'/

data &
    int_right_mask/ z'00000001', z'00000003', z'00000007', z'0000000f', &
                    z'0000001f', z'0000003f', z'0000007f', z'000000ff', &
                    z'000001ff', z'000003ff', z'000007ff', z'00000fff', &
                    z'00001fff', z'00003fff', z'00007fff', z'0000ffff', &
                    z'0001ffff', z'0003ffff', z'0007ffff', z'000fffff', &
                    z'001fffff', z'003fffff', z'007fffff', z'00ffffff', &
                    z'01ffffff', z'03ffffff', z'07ffffff', z'0fffffff', &
                    z'1fffffff', z'3fffffff', z'7fffffff', z'ffffffff'/

!  mask, maskl, maskr dada for long_k

integer( kind= long_k), dimension( bit_size( 0_long_k) ), save :: &
      long_left_mask, long_right_mask

data &
   long_left_mask/ z'8000000000000000', z'c000000000000000', z'e000000000000000', z'f000000000000000', &
                   z'f800000000000000', z'fc00000000000000', z'fe00000000000000', z'ff00000000000000', &
                   z'ff80000000000000', z'ffc0000000000000', z'ffe0000000000000', z'fff0000000000000', &
                   z'fff8000000000000', z'fffc000000000000', z'fffe000000000000', z'ffff000000000000', &
                   z'ffff800000000000', z'ffffc00000000000', z'ffffe00000000000', z'fffff00000000000', &
                   z'fffff80000000000', z'fffffc0000000000', z'fffffe0000000000', z'ffffff0000000000', &
                   z'ffffff8000000000', z'ffffffc000000000', z'ffffffe000000000', z'fffffff000000000', &
                   z'fffffff800000000', z'fffffffc00000000', z'fffffffe00000000', z'ffffffff00000000', &
                   z'ffffffff80000000', z'ffffffffc0000000', z'ffffffffe0000000', z'fffffffff0000000', &
                   z'fffffffff8000000', z'fffffffffc000000', z'fffffffffe000000', z'ffffffffff000000', &
                   z'ffffffffff800000', z'ffffffffffc00000', z'ffffffffffe00000', z'fffffffffff00000', &
                   z'fffffffffff80000', z'fffffffffffc0000', z'fffffffffffe0000', z'ffffffffffff0000', &
                   z'ffffffffffff8000', z'ffffffffffffc000', z'ffffffffffffe000', z'fffffffffffff000', &
                   z'fffffffffffff800', z'fffffffffffffc00', z'fffffffffffffe00', z'ffffffffffffff00', &
                   z'ffffffffffffff80', z'ffffffffffffffc0', z'ffffffffffffffe0', z'fffffffffffffff0', &
                   z'fffffffffffffff8', z'fffffffffffffffc', z'fffffffffffffffe', z'ffffffffffffffff'/


data &
   long_right_mask/ z'0000000000000001', z'0000000000000003', z'0000000000000007', z'000000000000000f', &
                    z'000000000000001f', z'000000000000003f', z'000000000000007f', z'00000000000000ff', &
                    z'00000000000001ff', z'00000000000003ff', z'00000000000007ff', z'0000000000000fff', &
                    z'0000000000001fff', z'0000000000003fff', z'0000000000007fff', z'000000000000ffff', &
                    z'000000000001ffff', z'000000000003ffff', z'000000000007ffff', z'00000000000fffff', &
                    z'00000000001fffff', z'00000000003fffff', z'00000000007fffff', z'0000000000ffffff', &
                    z'0000000001ffffff', z'0000000003ffffff', z'0000000007ffffff', z'000000000fffffff', &
                    z'000000001fffffff', z'000000003fffffff', z'000000007fffffff', z'00000000ffffffff', &
                    z'00000001ffffffff', z'00000003ffffffff', z'00000007ffffffff', z'0000000fffffffff', &
                    z'0000001fffffffff', z'0000003fffffffff', z'0000007fffffffff', z'000000ffffffffff', &
                    z'000001ffffffffff', z'000003ffffffffff', z'000007ffffffffff', z'00000fffffffffff', &
                    z'00001fffffffffff', z'00003fffffffffff', z'00007fffffffffff', z'0000ffffffffffff', &
                    z'0001ffffffffffff', z'0003ffffffffffff', z'0007ffffffffffff', z'000fffffffffffff', &
                    z'001fffffffffffff', z'003fffffffffffff', z'007fffffffffffff', z'00ffffffffffffff', &
                    z'01ffffffffffffff', z'03ffffffffffffff', z'07ffffffffffffff', z'0fffffffffffffff', &
                    z'1fffffffffffffff', z'3fffffffffffffff', z'7fffffffffffffff', z'ffffffffffffffff'/

! **********************************************************************

!  masks for leadz()

integer( kind= byte_k), save :: byte_lead_p4; data byte_lead_p4/ z'f0'/
integer( kind= byte_k), save :: byte_lead_p2; data byte_lead_p2/ z'cc'/
integer( kind= byte_k), save :: byte_lead_p1; data byte_lead_p1/ z'aa'/

integer( kind= short_k), save :: short_lead_p8; data short_lead_p8/ z'ff00'/
integer( kind= short_k), save :: short_lead_p4; data short_lead_p4/ z'f0f0'/
integer( kind= short_k), save :: short_lead_p2; data short_lead_p2/ z'cccc'/
integer( kind= short_k), save :: short_lead_p1; data short_lead_p1/ z'aaaa'/

integer( kind= int_k), save :: int_lead_p16; data int_lead_p16/ z'ffff0000'/
integer( kind= int_k), save :: int_lead_p8; data int_lead_p8/ z'ff00ff00'/
integer( kind= int_k), save :: int_lead_p4; data int_lead_p4/ z'f0f0f0f0'/
integer( kind= int_k), save :: int_lead_p2; data int_lead_p2/ z'cccccccc'/
integer( kind= int_k), save :: int_lead_p1; data int_lead_p1/ z'aaaaaaaa'/

integer( kind= long_k), save :: long_lead_p32; data long_lead_p32/ z'ffffffff00000000'/
integer( kind= long_k), save :: long_lead_p16; data long_lead_p16/ z'ffff0000ffff0000'/
integer( kind= long_k), save :: long_lead_p8; data long_lead_p8/ z'ff00ff00ff00ff00'/
integer( kind= long_k), save :: long_lead_p4; data long_lead_p4/ z'f0f0f0f0f0f0f0f0'/
integer( kind= long_k), save :: long_lead_p2; data long_lead_p2/ z'cccccccccccccccc'/
integer( kind= long_k), save :: long_lead_p1; data long_lead_p1/ z'aaaaaaaaaaaaaaaa'/

! **********************************************************************

!  masks for lastz()

integer( kind= byte_k), save :: byte_last_p4; data byte_last_p4/ z'0f'/
integer( kind= byte_k), save :: byte_last_p2; data byte_last_p2/ z'33'/
integer( kind= byte_k), save :: byte_last_p1; data byte_last_p1/ z'55'/

integer( kind= short_k), save :: short_last_p8; data short_last_p8/ z'00ff'/
integer( kind= short_k), save :: short_last_p4; data short_last_p4/ z'0f0f'/
integer( kind= short_k), save :: short_last_p2; data short_last_p2/ z'3333'/
integer( kind= short_k), save :: short_last_p1; data short_last_p1/ z'5555'/

integer( kind= int_k), save :: int_last_p16; data int_last_p16/ z'0000ffff'/
integer( kind= int_k), save :: int_last_p8; data int_last_p8/ z'00ff00ff'/
integer( kind= int_k), save :: int_last_p4; data int_last_p4/ z'0f0f0f0f'/
integer( kind= int_k), save :: int_last_p2; data int_last_p2/ z'33333333'/
integer( kind= int_k), save :: int_last_p1; data int_last_p1/ z'55555555'/

integer( kind= long_k), save :: long_last_p32; data long_last_p32/ z'00000000ffffffff'/
integer( kind= long_k), save :: long_last_p16; data long_last_p16/ z'0000ffff0000ffff'/
integer( kind= long_k), save :: long_last_p8; data long_last_p8/ z'00ff00ff00ff00ff'/
integer( kind= long_k), save :: long_last_p4; data long_last_p4/ z'0f0f0f0f0f0f0f0f'/
integer( kind= long_k), save :: long_last_p2; data long_last_p2/ z'3333333333333333'/
integer( kind= long_k), save :: long_last_p1; data long_last_p1/ z'5555555555555555'/

! **********************************************************************

!  masks for popcnt()/poppar()

integer( kind= byte_k), save :: byte_p1; data byte_p1/ z'11'/
integer( kind= byte_k), save :: byte_p2; data byte_p2/ z'22'/
integer( kind= byte_k), save :: byte_p4; data byte_p4/ z'44'/
integer( kind= byte_k), save :: byte_p8; data byte_p8/ z'88'/

integer( kind= byte_k), save :: byte_hi_nibble; data byte_hi_nibble/ z'f0'/
integer( kind= byte_k), save :: byte_lo_nibble; data byte_lo_nibble/ z'0f'/

integer( kind= byte_k), save :: byte_low_bit; data byte_low_bit/ z'01'/

integer( kind= short_k), save :: short_p1; data short_p1/ z'1111'/
integer( kind= short_k), save :: short_p2; data short_p2/ z'2222'/
integer( kind= short_k), save :: short_p4; data short_p4/ z'4444'/
integer( kind= short_k), save :: short_p8; data short_p8/ z'8888'/

integer( kind= short_k), save :: short_hi_nibble; data short_hi_nibble/ z'f0f0'/
integer( kind= short_k), save :: short_lo_nibble; data short_lo_nibble/ z'0f0f'/

integer( kind= short_k), save :: short_low_byte; data short_low_byte/ z'00ff'/

integer( kind= short_k), save :: short_low_bit; data short_low_bit/ z'0001'/

integer( kind= int_k), save :: int_p1; data int_p1/ z'11111111'/
integer( kind= int_k), save :: int_p2; data int_p2/ z'22222222'/
integer( kind= int_k), save :: int_p4; data int_p4/ z'44444444'/
integer( kind= int_k), save :: int_p8; data int_p8/ z'88888888'/

integer( kind= int_k), save :: int_hi_nibble; data int_hi_nibble/ z'f0f0f0f0'/
integer( kind= int_k), save :: int_lo_nibble; data int_lo_nibble/ z'0f0f0f0f'/

integer( kind= int_k), save :: int_low_byte; data int_low_byte/ z'000000ff'/

integer( kind= int_k), save :: int_low_bit; data int_low_bit/ z'00000001'/

integer( kind= long_k), save :: long_p1; data long_p1/ z'1111111111111111'/
integer( kind= long_k), save :: long_p2; data long_p2/ z'2222222222222222'/
integer( kind= long_k), save :: long_p4; data long_p4/ z'4444444444444444'/
integer( kind= long_k), save :: long_p8; data long_p8/ z'8888888888888888'/

integer( kind= long_k), save :: long_hi_nibble; data long_hi_nibble/ z'f0f0f0f0f0f0f0f0'/
integer( kind= long_k), save :: long_lo_nibble; data long_lo_nibble/ z'0f0f0f0f0f0f0f0f'/

integer( kind= long_k), save :: long_low_byte; data long_low_byte/ z'00000000000000ff'/

integer( kind= long_k), save :: long_low_bit; data long_low_bit/ z'0000000000000001'/

! **********************************************************************

!  module procedures

! **********************************************************************

contains                                                   ! bit_functions

! **********************************************************************

!  csmg(): conditional scalar merge for integer kinds

! **********************************************************************

!  byte_csmg(): csmg() for kind byte

elemental integer( kind= byte_k) function byte_csmg( i, j, k)

integer( kind= byte_k), intent( in) :: i, j, k

!  byte_csmg()

continue                                                             ! csmg()

   byte_csmg = ior( iand( i, k), iand( j, not( k)) )

return                                                               ! csmg()

!  byte_csmg()

end function byte_csmg

! **********************************************************************

!  short_csmg(): csmg() for kind short

elemental integer( kind= short_k) function short_csmg( i, j, k)

integer( kind= short_k), intent( in) :: i, j, k

!  short_csmg()

continue                                                             ! csmg()

   short_csmg = ior( iand( i, k), iand( j, not( k)) )

return                                                               ! csmg()

!  short_csmg()

end function short_csmg

! **********************************************************************

!  int_csmg(): csmg() for kind int

elemental integer( kind= int_k) function int_csmg( i, j, k)

integer( kind= int_k), intent( in) :: i, j, k

!  int_csmg()

continue                                                             ! csmg()

   int_csmg = ior( iand( i, k), iand( j, not( k)) )

return                                                               ! csmg()

!  int_csmg()

end function int_csmg

! **********************************************************************

!  long_csmg(): csmg() for kind long

elemental integer( kind= long_k) function long_csmg( i, j, k)

integer( kind= long_k), intent( in) :: i, j, k

!  long_csmg()

continue                                                             ! csmg()

   long_csmg = ior( iand( i, k), iand( j, not( k)) )

return                                                               ! csmg()

!  long_csmg()

end function long_csmg

! **********************************************************************

!  compl(): bit-wise complement

! **********************************************************************

!  byte_compl(): compl() for kind byte

elemental integer( kind= byte_k) function byte_compl( i)

integer( kind= byte_k), intent( in) :: i

!  byte_compl()

continue                                                             ! compl()

   byte_compl = not( i)

return                                                               ! compl()

!  byte_compl()

end function byte_compl

! **********************************************************************

!  short_compl(): compl() for kind short

elemental integer( kind= short_k) function short_compl( i)

integer( kind= short_k), intent( in) :: i

!  short_compl()

continue                                                             ! compl()

   short_compl = not( i)

return                                                               ! compl()

!  short_compl()

end function short_compl

! **********************************************************************

!  int_compl(): compl() for kind int

elemental integer( kind= int_k) function int_compl( i)

integer( kind= int_k), intent( in) :: i

!  int_compl()

continue                                                             ! compl()

   int_compl = not( i)

return                                                               ! compl()

!  int_compl()

end function int_compl

! **********************************************************************

!  long_compl(): compl() for kind long

elemental integer( kind= long_k) function long_compl( i)

integer( kind= long_k), intent( in) :: i

!  long_compl()

continue                                                             ! compl()

   long_compl = not( i)

return                                                               ! compl()

!  long_compl()

end function long_compl

! **********************************************************************

!  bit counts: leadz(), lastz(), popcnt(), poppar()

! **********************************************************************

!  leadz( b)

elemental integer( kind= byte_k) function byte_leadz( b)

integer( kind= byte_k), intent( in) :: b

!  scratch data and masks

   integer( kind= byte_k) :: test, at_least

!  byte_leadz()

continue                                                             ! leadz()

   test = b

   if( test == 0_byte_k )then                                        ! catch end case
      byte_leadz = bit_size( b)
      return                                                         ! leadz()
   end if

   if( iand( byte_lead_p4, test) == 0_byte_k )then
      at_least = 4                                                   ! top half all zero
   else
      at_least = 0
      test = iand( byte_lead_p4, test)
   end if

   if( iand( byte_lead_p2, test) == 0_byte_k )then
      at_least = at_least + 2                                        ! top quarter all zero
   else
      test = iand( byte_lead_p2, test)
   end if

   if( iand( byte_lead_p1, test) == 0_byte_k )then
      at_least = at_least + 1                                        ! top bit (eighth) zero
   end if

   byte_leadz = at_least

return                                                               ! leadz()

!  byte_leadz()

end function byte_leadz

! **********************************************************************

!  leadz( b)

elemental integer( kind= short_k) function short_leadz( b)

integer( kind= short_k), intent( in) :: b

!  scratch data

   integer( kind= short_k) :: test, at_least

!  short_leadz()

continue                                                             ! leadz()

   test = b

   if( test == 0_short_k )then                                       ! catch end case
      short_leadz = bit_size( b)
      return                                                         ! leadz()
   end if

   if( iand( short_lead_p8, test) == 0_short_k )then
      at_least = 8                                                   ! top half all zero
   else
      at_least = 0
      test = iand( short_lead_p8, test)
   end if

   if( iand( short_lead_p4, test) == 0_short_k )then
      at_least = at_least + 4                                        ! top quarter all zero
   else
      test = iand( short_lead_p4, test)
   end if

   if( iand( short_lead_p2, test) == 0_short_k )then
      at_least = at_least + 2                                        ! top eighth all zero
   else
      test = iand( short_lead_p2, test)
   end if

   if( iand( short_lead_p1, test) == 0_short_k )then
      at_least = at_least + 1                                        ! top bit (sixteenth) zero
   end if

   short_leadz = at_least

return                                                               ! leadz()

!  short_leadz()

end function short_leadz

! **********************************************************************

!  leadz( b)

elemental integer( kind= int_k) function int_leadz( b)

integer( kind= int_k), intent( in) :: b

!  scratch data

   integer( kind= int_k) :: test, at_least

!  int_leadz()

continue                                                             ! leadz()

   test = b

   if( test == 0_int_k )then                                         ! catch end case
      int_leadz = bit_size( b)
      return                                                         ! leadz()
   end if

   if( iand( int_lead_p16, test) == 0_int_k )then
      at_least = 16                                                  ! top half all zero
   else
      at_least = 0
      test = iand( int_lead_p16, test)
   end if

   if( iand( int_lead_p8, test) == 0_int_k )then
      at_least = at_least + 8                                        ! top quarter all zero
   else
      test = iand( int_lead_p8, test)
   end if

   if( iand( int_lead_p4, test) == 0_int_k )then
      at_least = at_least + 4                                        ! top eighth all zero
   else
      test = iand( int_lead_p4, test)
   end if

   if( iand( int_lead_p2, test) == 0_int_k )then
      at_least = at_least + 2                                        ! top sixteenth all zero
   else
      test = iand( int_lead_p2, test)
   end if

   if( iand( int_lead_p1, test) == 0_int_k )then
      at_least = at_least + 1                                        ! top bit (thirtysecond) zero
   end if

   int_leadz = at_least

return                                                               ! leadz()

!  int_leadz()

end function int_leadz

! **********************************************************************

!  leadz( b)

elemental integer( kind= long_k) function long_leadz( b)

integer( kind= long_k), intent( in) :: b

!  scratch data and masks

   integer( kind= long_k) :: test, at_least

!  long_leadz()

continue                                                             ! leadz()

   test = b

   if( test == 0_long_k )then                                        ! catch end case
      long_leadz = bit_size( b)
      return                                                         ! leadz()
   end if

   if( iand( long_lead_p32, test) == 0_long_k )then
      at_least = 32                                                  ! top half all zero
   else
      at_least = 0
      test = iand( long_lead_p32, test)
   end if

   if( iand( long_lead_p16, test) == 0_long_k )then
      at_least = at_least + 16                                       ! top quarter all zero
   else
      test = iand( long_lead_p16, test)
   end if

   if( iand( long_lead_p8, test) == 0_long_k )then
      at_least = at_least + 8                                        ! top eighth all zero
   else
      test = iand( long_lead_p8, test)
   end if

   if( iand( long_lead_p4, test) == 0_long_k )then
      at_least = at_least + 4                                        ! top sixteenth all zero
   else
      test = iand( long_lead_p4, test)
   end if

   if( iand( long_lead_p2, test) == 0_long_k )then
      at_least = at_least + 2                                        ! top thritysecond all zero
   else
      test = iand( long_lead_p2, test)
   end if

   if( iand( long_lead_p1, test) == 0_long_k )then
      at_least = at_least + 1                                        ! top bit (sixtyfourth) zero
   end if

   long_leadz = at_least

return                                                               ! leadz()

!  long_leadz()

end function long_leadz

! **********************************************************************

!  lastz( b)

elemental integer( kind= byte_k) function byte_lastz( b)

integer( kind= byte_k), intent( in) :: b

!  scratch data and masks

   integer( kind= byte_k) :: test, at_least

!  byte_lastz()

continue                                                             ! lastz()

   test = b                                                          ! operate on integer

   if( test == 0_byte_k )then                                        ! catch end case now
      byte_lastz = bit_size( b)
      return
   end if

   if( iand( byte_last_p4, test) == 0_byte_k )then
      at_least = 4                                                   ! bottom half all zero
   else
      at_least = 0
      test = iand( byte_last_p4, test)
   end if

   if( iand( byte_last_p2, test) == 0_byte_k )then
      at_least = at_least + 2                                        ! bottom quarter all zero
   else
      test = iand( byte_last_p2, test)
   end if

   if( iand( byte_last_p1, test) == 0_byte_k )then
      at_least = at_least + 1                                        ! bottom bit zero
   end if

   byte_lastz = at_least

return                                                               ! lastz()

!  byte_lastz()

end function byte_lastz

! **********************************************************************

!  lastz( b)

elemental integer( kind= short_k) function short_lastz( b)

integer( kind= short_k), intent( in) :: b

!  scratch data and masks

   integer( kind= short_k) :: test, at_least

!  short_lastz()

continue                                                             ! lastz()

   test = b                                                          ! operate on integer

   if( test == 0_short_k )then                                       ! catch end case now
      short_lastz = bit_size( b)
      return
   end if

   if( iand( short_last_p8, test) == 0_short_k )then
      at_least = 8                                                   ! bottom half all zero
   else
      at_least = 0
      test = iand( short_last_p8, test)
   end if

   if( iand( short_last_p4, test) == 0_short_k )then
      at_least = at_least + 4                                        ! bottom quarter all zero
   else
      test = iand( short_last_p4, test)
   end if

   if( iand( short_last_p2, test) == 0_short_k )then
      at_least = at_least + 2                                        ! bottom eighth all zero
   else
      test = iand( short_last_p2, test)
   end if

   if( iand( short_last_p1, test) == 0_short_k )then
      at_least = at_least + 1                                        ! bottom bit zero
   end if

   short_lastz = at_least

return                                                               ! lastz()

!  short_lastz()

end function short_lastz

! **********************************************************************

!  lastz( b)

elemental integer( kind= int_k) function int_lastz( b)

integer( kind= int_k), intent( in) :: b

!  scratch data and masks

   integer( kind= int_k) :: test, at_least

!  int_lastz()

continue                                                   ! lastz()

   test = b                                                          ! operate on integer

   if( test == 0_int_k )then                                         ! catch end case now
      int_lastz = bit_size( b)
      return
   end if

   if( iand( int_last_p16, test) == 0_int_k )then
      at_least = 16                                                  ! bottom half all zero
   else
      at_least = 0
      test = iand( int_last_p16, test)
   end if

   if( iand( int_last_p8, test) == 0_int_k )then
      at_least = at_least + 8                                        ! bottom quarter all zero
   else
      test = iand( int_last_p8, test)
   end if

   if( iand( int_last_p4, test) == 0_int_k )then
      at_least = at_least + 4                                        ! bottom eighth all zero
   else
      test = iand( int_last_p4, test)
   end if

   if( iand( int_last_p2, test) == 0_int_k )then
      at_least = at_least + 2                                        ! bottom sixteenth all zero
   else
      test = iand( int_last_p2, test)
   end if

   if( iand( int_last_p1, test) == 0_int_k )then
      at_least = at_least + 1                                        ! bottom bit zero
   end if

   int_lastz = at_least

return                                                               ! lastz()

!  int_lastz()

end function int_lastz

! **********************************************************************

!  lastz( b)

elemental integer( kind= long_k) function long_lastz( b)

integer( kind= long_k), intent( in) :: b

!  scratch data and masks

   integer( kind= long_k) :: test, at_least

!  long_lastz()

continue                                                             ! lastz()

   test = b                                                          ! operate on integer

   if( test == 0_long_k )then                                        ! catch end case now
      long_lastz = bit_size( b)
      return
   end if

   if( iand( long_last_p32, test) == 0_long_k )then
      at_least = 32                                                  ! bottom half all zero
   else
      at_least = 0
      test = iand( long_last_p32, test)
   end if

   if( iand( long_last_p16, test) == 0_long_k )then
      at_least = at_least + 16                                       ! bottom half all zero
   else
      test = iand( long_last_p16, test)
   end if

   if( iand( long_last_p8, test) == 0_long_k )then
      at_least = at_least + 8                                        ! bottom quarter all zero
   else
      test = iand( long_last_p8, test)
   end if

   if( iand( long_last_p4, test) == 0_long_k )then
      at_least = at_least + 4                                        ! bottom eighth all zero
   else
      test = iand( long_last_p4, test)
   end if

   if( iand( long_last_p2, test) == 0_long_k )then
      at_least = at_least + 2                                        ! bottom sixteenth all zero
   else
      test = iand( long_last_p2, test)
   end if

   if( iand( long_last_p1, test) == 0_long_k )then
      at_least = at_least + 1                                        ! bottom bit zero
   end if

   long_lastz = at_least

return                                                               ! lastz()

!  long_lastz()

end function long_lastz

! **********************************************************************

!  popcnt( b)

elemental integer( kind= byte_k) function byte_popcnt( b)

integer( kind= byte_k), intent( in) :: b

!  scratch data

   integer( kind= byte_k) :: test, t1, t2, t4, t8

!  byte_popcnt()

continue                                                             ! popcnt()

   test = b                                                          ! operate on integer

   t1 = iand( test, byte_p1)                                         ! 1 bit from each nibble
   t2 = iand( test, byte_p2)
   t4 = iand( test, byte_p4)
   t8 = iand( test, byte_p8)

   test = t1 + ishft( t2, -1) + ishft( t4, -2) + ishft( t8, -3)

! each nibble now contains [ 0, 1, 2, 3]

   t1 = iand( test, byte_hi_nibble)
   t2 = iand( test, byte_lo_nibble)

! add each of 4 high nibbles with each of 4 low nibbles

   test = iand( ishft( t1, -4) + t2, byte_lo_nibble)

! return popcnt

   byte_popcnt = test

return                                                               ! popcnt()

!  byte_popcnt()

end function byte_popcnt

! **********************************************************************

!  popcnt( b)

elemental integer( kind= short_k) function short_popcnt( b)

integer( kind= short_k), intent( in) :: b

!  scratch data

   integer( kind= short_k) :: test, t1, t2, t4, t8

!  short_popcnt()

continue                                                             ! popcnt()

   test = b                                                          ! operate on integer

   t1 = iand( test, short_p1)                                        ! 1 bit from each nibble
   t2 = iand( test, short_p2)
   t4 = iand( test, short_p4)
   t8 = iand( test, short_p8)

   test = t1 + ishft( t2, -1) + ishft( t4, -2) + ishft( t8, -3)

! each nibble now contains [ 0, 1, 2, 3]

   t1 = iand( test, short_hi_nibble)
   t2 = iand( test, short_lo_nibble)

! add each of 4 high nibbles with each of 4 low nibbles

   test = iand( ishft( t1, -4) + t2, short_lo_nibble)

! add each of 2 bytes & mask off low byte

   test = test + ishft( test, -8)

   short_popcnt = iand( test, short_low_byte)

return                                                               ! popcnt()

!  short_popcnt()

end function short_popcnt

! **********************************************************************

!  popcnt( b)

elemental integer( kind= int_k) function int_popcnt( b)

integer( kind= int_k), intent( in) :: b

!  scratch data and masks

   integer( kind= int_k) :: test, t1, t2, t4, t8

!  int_popcnt()

continue                                                             ! popcnt()

   test = b                                                          ! operate on integer

   t1 = iand( test, int_p1)                                          ! 1 bit from each nibble
   t2 = iand( test, int_p2)
   t4 = iand( test, int_p4)
   t8 = iand( test, int_p8)

   test = t1 + ishft( t2, -1) + ishft( t4, -2) + ishft( t8, -3)

! each nibble now contains [ 0, 1, 2, 3]

   t1 = iand( test, int_hi_nibble)
   t2 = iand( test, int_lo_nibble)

! add each of 4 high nibbles with each of 4 low nibbles

   test = iand( ishft( t1, -4) + t2, int_lo_nibble)

! add each of 4 bytes & mask off low byte

   test = test + ishft( test, -8) + ishft( test, -16) + ishft( test, -24)

   int_popcnt = iand( test, int_low_byte)

return                                                               ! popcnt()

!  int_popcnt()

end function int_popcnt

! **********************************************************************

!  popcnt( b)

elemental integer( kind= long_k) function long_popcnt( b)

integer( kind= long_k), intent( in) :: b

!  scratch data and masks

   integer( kind= long_k) :: test, t1, t2, t4, t8

!  long_popcnt()

continue                                                             ! popcnt()

   test = b                                                          ! operate on integer

   t1 = iand( test, long_p1)                                         ! 1 bit from each nibble
   t2 = iand( test, long_p2)
   t4 = iand( test, long_p4)
   t8 = iand( test, long_p8)

   test = t1 + ishft( t2, -1) + ishft( t4, -2) + ishft( t8, -3)

! each nibble now contains [ 0, 1, 2, 3]

   t1 = iand( test, long_hi_nibble)
   t2 = iand( test, long_lo_nibble)

! add each of 4 high nibbles with each of 4 low nibbles

   test = iand( ishft( t1, -4) + t2, long_lo_nibble)

! add each of 4 bytes & mask off low byte

   test = test + ishft( test, -8) + ishft( test, -16) + ishft( test, -24) &
               + ishft( test, -32) + ishft( test, -40) + ishft( test, -48) + ishft( test, -56)

   long_popcnt = iand( test, long_low_byte)

return                                                               ! popcnt()

!  long_popcnt()

end function long_popcnt

! **********************************************************************

!  poppar( b)

elemental integer( kind= byte_k) function byte_poppar( b)

integer( kind= byte_k), intent( in) :: b

!  local data

   integer( kind= byte_k) :: test, t1, t2, t4, t8

!  byte_poppar()

continue                                                             ! poppar()

   test = b

   t1 = iand( test, byte_p1)
   t2 = iand( test, byte_p2)
   t4 = iand( test, byte_p4)
   t8 = iand( test, byte_p8)

   test = t1 + ishft( t2, -1) + ishft( t4, -2) + ishft( t8, -3)

   t1 = iand( test, byte_hi_nibble)
   t2 = iand( test, byte_lo_nibble)
   test = iand( ishft( t1, -4) + t2, byte_lo_nibble)

   byte_poppar = iand( test, byte_low_bit)

return                                                               ! poppar()

!  byte_poppar()

end function byte_poppar

! **********************************************************************

!  poppar( b)

elemental integer( kind= short_k) function short_poppar( b)

integer( kind= short_k), intent( in) :: b

!  local data

   integer( kind= short_k) :: test, t1, t2, t4, t8

!  short_poppar()

continue                                                             ! poppar()

   test = b

   t1 = iand( test, short_p1)
   t2 = iand( test, short_p2)
   t4 = iand( test, short_p4)
   t8 = iand( test, short_p8)

   test = t1 + ishft( t2, -1) + ishft( t4, -2) + ishft( t8, -3)

   t1 = iand( test, short_hi_nibble)
   t2 = iand( test, short_lo_nibble)
   test = iand( ishft( t1, -4) + t2, short_lo_nibble)
   test = test + ishft( test, -8)

   short_poppar = iand( test, short_low_bit)

return                                                               ! poppar()

!  short_poppar()

end function short_poppar

! **********************************************************************

!  poppar( b)

elemental integer( kind= int_k) function int_poppar( b)

integer( kind= int_k), intent( in) :: b

!  local data

   integer( kind= int_k) :: test, t1, t2, t4, t8

!  int_poppar()

continue                                                             ! poppar()

   test = b

   t1 = iand( test, int_p1)
   t2 = iand( test, int_p2)
   t4 = iand( test, int_p4)
   t8 = iand( test, int_p8)

   test = t1 + ishft( t2, -1) + ishft( t4, -2) + ishft( t8, -3)

   t1 = iand( test, int_hi_nibble)
   t2 = iand( test, int_lo_nibble)
   test = iand( ishft( t1, -4) + t2, int_lo_nibble)
   test = test + ishft( test, -8) + ishft( test, -16) + ishft( test, -24)

   int_poppar = iand( test, int_low_bit)

return                                                               ! poppar()

!  int_poppar()

end function int_poppar

! **********************************************************************

!  poppar( b)

elemental integer( kind= long_k) function long_poppar( b)

integer( kind= long_k), intent( in) :: b

!  local data

   integer( kind= long_k) :: test, t1, t2, t4, t8

!  long_poppar()

continue                                                             ! poppar()

   test = b

   t1 = iand( test, long_p1)
   t2 = iand( test, long_p2)
   t4 = iand( test, long_p4)
   t8 = iand( test, long_p8)

   test = t1 + ishft( t2, -1) + ishft( t4, -2) + ishft( t8, -3)

   t1 = iand( test, long_hi_nibble)
   t2 = iand( test, long_lo_nibble)
   test = iand( ishft( t1, -4) + t2, long_lo_nibble)
   test = test + ishft( test, -8) + ishft( test, -16) + ishft( test, -24)

   long_poppar = iand( test, long_low_bit)

return                                                               ! poppar()

!  long_poppar()

end function long_poppar

! **********************************************************************

!  .ilen. i, ilen( i)

! **********************************************************************

!  byte_ilen()

elemental integer( kind= byte_k) function byte_ilen( i)

integer( kind= byte_k), intent( in) :: i

!  byte_ilen()

continue                                                             ! .ilen. i, ilen()

   byte_ilen = bit_size( i) - leadz( abs( i))

return                                                               ! .ilen i, ilen()

!  byte_ilen()

end function byte_ilen

! **********************************************************************

!  short_ilen()

elemental integer( kind= short_k) function short_ilen( i)

integer( kind= short_k), intent( in) :: i

!  short_ilen()

continue                                                             ! .ilen. i, ilen()

   short_ilen = bit_size( i) - leadz( abs( i))

return                                                               ! .ilen i, ilen()

!  short_ilen()

end function short_ilen

! **********************************************************************

!  int_ilen()

elemental integer( kind= int_k) function int_ilen( i)

integer( kind= int_k), intent( in) :: i

!  int_ilen()

continue                                                             ! .ilen. i, ilen()

   int_ilen = bit_size( i) - leadz( abs( i))

return                                                               ! .ilen i, ilen()

!  int_ilen()

end function int_ilen

! **********************************************************************

!  long_ilen()

elemental integer( kind= long_k) function long_ilen( i)

integer( kind= long_k), intent( in) :: i

!  long_ilen()

continue                                                             ! .ilen. i, ilen()

   long_ilen = bit_size( i) - leadz( abs( i))

return                                                               ! .ilen i, ilen()

!  long_ilen()

end function long_ilen

! **********************************************************************

!  i .hamd. i, hamd( i, i)

! **********************************************************************

!  byte_hamd()

elemental integer( kind= byte_k) function byte_hamd( i, j)

integer( kind= byte_k), intent( in) :: i, j

!  byte_hamd()

continue                                                             ! i .hamd. i, hamd()

   byte_hamd = popcnt( ieor( i, j))

return                                                               ! i .hamd i, hamd()

!  byte_hamd()

end function byte_hamd

! **********************************************************************

!  short_hamd()

elemental integer( kind= short_k) function short_hamd( i, j)

integer( kind= short_k), intent( in) :: i, j

!  short_hamd()

continue                                                             ! i .hamd. i, hamd()

   short_hamd = popcnt( ieor( i, j))

return                                                               ! i .hamd i, hamd()

!  short_hamd()

end function short_hamd

! **********************************************************************

!  int_hamd()

elemental integer( kind= int_k) function int_hamd( i, j)

integer( kind= int_k), intent( in) :: i, j

!  int_hamd()

continue                                                             ! i .hamd. i, hamd()

   int_hamd = popcnt( ieor( i, j))

return                                                               ! i .hamd i, hamd()

!  int_hamd()

end function int_hamd

! **********************************************************************

!  long_hamd()

elemental integer( kind= long_k) function long_hamd( i, j)

integer( kind= long_k), intent( in) :: i, j

!  long_hamd()

continue                                                             ! i .hamd. i, hamd()

   long_hamd = popcnt( ieor( i, j))

return                                                               ! i .hamd i, hamd()

!  long_hamd()

end function long_hamd

! **********************************************************************

!  masks: mask(), maskl(), maskr()

! **********************************************************************

!  mask( i)

elemental integer( kind= byte_k) function byte_mask( i)

integer( kind= byte_k), intent( in) :: i

!  byte_mask() local

   integer, parameter :: bs = bit_size( i)                           ! bits in kind i

!  byte_mask()

continue                                                             ! mask()

   bits: select case( i)

   case( 1: bs) bits                                                 ! [ 1, 8] ==> 00... 0011... 11

      byte_mask = byte_right_mask( i)

   case( -bs: -1) bits                                               ! [ -8, -1] ==> 11... 1100... 00

      byte_mask = byte_left_mask( abs( i))

   case default bits                                                 ! otherwise 00... 00

      byte_mask = 0_byte_k

   end select bits

return                                                               ! mask()

!  byte_mask()

end function byte_mask

! **********************************************************************

!  mask( i)

elemental integer( kind= short_k) function short_mask( i)

integer( kind= short_k), intent( in) :: i

!  short_mask() local

   integer, parameter :: bs = bit_size( i)                           ! bits in kind i

!  short_mask()

continue                                                             ! mask()

   bits: select case( i)

   case( 1: bs) bits                                                 ! [ 1, 8] ==> 00... 0011... 11

      short_mask = short_right_mask( i)

   case( -bs: -1) bits                                               ! [ -8, -1] ==> 11... 1100... 00

      short_mask = short_left_mask( abs( i))

   case default bits                                                 ! otherwise 00... 00

      short_mask = 0_short_k

   end select bits

return                                                               ! mask()

!  short_mask()

end function short_mask

! **********************************************************************

!  mask( i)

elemental integer( kind= int_k) function int_mask( i)

integer( kind= int_k), intent( in) :: i

!  int_mask() local

   integer, parameter :: bs = bit_size( i)                           ! bits in kind i

!  int_mask()

continue                                                             ! mask()

   bits: select case( i)

   case( 1: bs) bits                                                 ! [ 1, 8] ==> 00... 0011... 11

      int_mask = int_right_mask( i)

   case( -bs: -1) bits                                               ! [ -8, -1] ==> 11... 1100... 00

      int_mask = int_left_mask( abs( i))

   case default bits                                                 ! otherwise 00... 00

      int_mask = 0_int_k

   end select bits

return                                                               ! mask()

!  int_mask()

end function int_mask

! **********************************************************************

!  mask( i)

elemental integer( kind= long_k) function long_mask( i)

integer( kind= long_k), intent( in) :: i

!  long_mask() local

   integer, parameter :: bs = bit_size( i)                           ! bits in kind i

!  long_mask()

continue                                                             ! mask()

   bits: select case( i)

   case( 1: bs) bits                                                 ! [ 1, 8] ==> 00... 0011... 11

      long_mask = long_right_mask( i)

   case( -bs: -1) bits                                               ! [ -8, -1] ==> 11... 1100... 00

      long_mask = long_left_mask( abs( i))

   case default bits                                                 ! otherwise 00... 00

      long_mask = 0_long_k

   end select bits

return                                                               ! mask()

!  long_mask()

end function long_mask

! **********************************************************************

!  maskl( i)

elemental integer( kind= byte_k) function byte_maskl( i)

integer( kind= byte_k), intent( in) :: i

!  byte_maskl() local

   integer, parameter :: bs = bit_size( i)                           ! bits in kind i

!  byte_maskl()

continue                                                             ! maskl()

   bits: select case( i)

   case( 1: bs) bits                                                 ! [ 1, 8] ==> 11... 1100... 00

      byte_maskl = byte_left_mask( i)

   case default bits                                                 ! otherwise 00... 00

      byte_maskl = 0_byte_k

   end select bits

return                                                               ! maskl()

!  byte_maskl()

end function byte_maskl

! **********************************************************************

!  maskl( i)

elemental integer( kind= short_k) function short_maskl( i)

integer( kind= short_k), intent( in) :: i

!  short_maskl() local

   integer, parameter :: bs = bit_size( i)                           ! bits in kind i

!  short_maskl()

continue                                                             ! maskl()

   bits: select case( i)

   case( 1: bs) bits                                                 ! [ 1, 8] ==> 11... 1100... 00

      short_maskl = short_left_mask( i)

   case default bits                                                 ! otherwise 00... 00

      short_maskl = 0_short_k

   end select bits

return                                                               ! maskl()

!  short_maskl()

end function short_maskl

! **********************************************************************

!  maskl( i)

elemental integer( kind= int_k) function int_maskl( i)

integer( kind= int_k), intent( in) :: i

!  int_maskl() local

   integer, parameter :: bs = bit_size( i)                           ! bits in kind i

!  int_maskl()

continue                                                             ! maskl()

   bits: select case( i)

   case( 1: bs) bits                                                 ! [ 1, 8] ==> 11... 1100... 00

      int_maskl = int_left_mask( i)

   case default bits                                                 ! otherwise 00... 00

      int_maskl = 0_int_k

   end select bits

return                                                               ! maskl()

!  int_maskl()

end function int_maskl

! **********************************************************************

!  maskl( i)

elemental integer( kind= long_k) function long_maskl( i)

integer( kind= long_k), intent( in) :: i

!  long_maskl() local

   integer, parameter :: bs = bit_size( i)                           ! bits in kind i

!  long_maskl()

continue                                                             ! maskl()

   bits: select case( i)

   case( 1: bs) bits                                                 ! [ 1, 8] ==> 11... 1100... 00

      long_maskl = long_left_mask( i)

   case default bits                                                 ! otherwise 00... 00

      long_maskl = 0_long_k

   end select bits

return                                                               ! maskl()

!  long_maskl()

end function long_maskl

! **********************************************************************

!  maskr( i)

elemental integer( kind= byte_k) function byte_maskr( i)

integer( kind= byte_k), intent( in) :: i

!  byte_maskr() local

   integer, parameter :: bs = bit_size( i)                           ! bits in kind i

!  byte_maskr()

continue                                                             ! maskr()

   bits: select case( i)

   case( 1: bs) bits                                                 ! [ 1, 8] ==> 00... 0011... 11

      byte_maskr = byte_right_mask( i)

   case default bits                                                 ! otherwise 00... 00

      byte_maskr = 0_byte_k

   end select bits

return                                                               ! maskr()

!  byte_maskr()

end function byte_maskr

! **********************************************************************

!  maskr( i)

elemental integer( kind= short_k) function short_maskr( i)

integer( kind= short_k), intent( in) :: i

!  short_maskr() local

   integer, parameter :: bs = bit_size( i)                           ! bits in kind i

!  short_maskr()

continue                                                             ! maskr()

   bits: select case( i)

   case( 1: bs) bits                                                 ! [ 1, 8] ==> 00... 0011... 11

      short_maskr = short_right_mask( i)

   case default bits                                                 ! otherwise 00... 00

      short_maskr = 0_short_k

   end select bits

return                                                               ! maskr()

!  short_maskr()

end function short_maskr

! **********************************************************************

!  maskr( i)

elemental integer( kind= int_k) function int_maskr( i)

integer( kind= int_k), intent( in) :: i

!  int_maskr() local

   integer, parameter :: bs = bit_size( i)                           ! bits in kind i

!  int_maskr()

continue                                                             ! maskr()

   bits: select case( i)

   case( 1: bs) bits                                                 ! [ 1, 8] ==> 00... 0011... 11

      int_maskr = int_right_mask( i)

   case default bits                                                 ! otherwise 00... 00

      int_maskr = 0_int_k

   end select bits

return                                                               ! maskr()

!  int_maskr()

end function int_maskr

! **********************************************************************

!  maskr( i)

elemental integer( kind= long_k) function long_maskr( i)

integer( kind= long_k), intent( in) :: i

!  long_maskr() local

   integer, parameter :: bs = bit_size( i)                           ! bits in kind i

!  long_maskr()

continue                                                             ! maskr()

   bits: select case( i)

   case( 1: bs) bits                                                 ! [ 1, 8] ==> 00... 0011... 11

      long_maskr = long_right_mask( i)

   case default bits                                                 ! otherwise 00... 00

      long_maskr = 0_long_k

   end select bits

return                                                               ! maskr()

!  long_maskr()

end function long_maskr

! **********************************************************************

!  specific functions implementing shifts as binary operators

! **********************************************************************

!  shift( i, j)

elemental integer( kind= byte_k) function byte_shift( i, j)

integer( kind= byte_k), intent( in) :: i, j

!  byte_shift()

continue                                                             ! .shift.

   byte_shift = ishft( i, j)

return                                                               ! .shift.

!  byte_shift()

end function byte_shift

! **********************************************************************

!  shift( i, j)

elemental integer( kind= short_k) function short_shift( i, j)

integer( kind= short_k), intent( in) :: i, j

!  short_shift()

continue                                                             ! .shift.

   short_shift = ishft( i, j)

return                                                               ! .shift.

!  short_shift()

end function short_shift

! **********************************************************************

!  shift( i, j)

elemental integer( kind= int_k) function int_shift( i, j)

integer( kind= int_k), intent( in) :: i, j

!  int_shift()

continue                                                             ! .shift.

   int_shift = ishft( i, j)

return                                                               ! .shift.

!  int_shift()

end function int_shift

! **********************************************************************

!  shift( i, j)

elemental integer( kind= long_k) function long_shift( i, j)

integer( kind= long_k), intent( in) :: i, j

!  long_shift()

continue                                                             ! .shift.

   long_shift = ishft( i, j)

return                                                               ! .shift.

!  long_shift()

end function long_shift

! **********************************************************************

!  rotate( i, j)

elemental integer( kind= byte_k) function byte_rotate( i, j)

integer( kind= byte_k), intent( in) :: i, j

!  byte_rotate()

continue                                                             ! .rotate.

   byte_rotate = ishftc( i, j)

return                                                               ! .rotate.

!  byte_rotate()

end function byte_rotate

! **********************************************************************

!  rotate( i, j)

elemental integer( kind= short_k) function short_rotate( i, j)

integer( kind= short_k), intent( in) :: i, j

!  short_rotate()

continue                                                             ! .rotate.

   short_rotate = ishftc( i, j)

return                                                               ! .rotate.

!  short_rotate()

end function short_rotate

! **********************************************************************

!  rotate( i, j)

elemental integer( kind= int_k) function int_rotate( i, j)

integer( kind= int_k), intent( in) :: i, j

!  int_rotate()

continue                                                             ! .rotate.

   int_rotate = ishftc( i, j)

return                                                               ! .rotate.

!  int_rotate()

end function int_rotate

! **********************************************************************

!  rotate( i, j)

elemental integer( kind= long_k) function long_rotate( i, j)

integer( kind= long_k), intent( in) :: i, j

!  long_rotate()

continue                                                             ! .rotate.

   long_rotate = ishftc( i, j)

return                                                               ! .rotate.

!  long_rotate()

end function long_rotate

! **********************************************************************

!  double word shifts: dshftl(), dshftr(), dshftc()

! **********************************************************************

!  dshftl( bl, br, i)

elemental integer( kind= byte_k) function byte_dshftl( bl, br, i)

integer( kind= byte_k), intent( in) :: bl, br
integer( kind= byte_k), intent( in) :: i

!  byte_dshftl() local

   integer( kind= int_k) :: btl, btr

!  byte_dshftl()

continue                                                             ! dshftl()

!  trap out endcase

   if( i < 0 )then

      byte_dshftl = 0_byte_k

      return                                                         ! dshftl()

   elseif( i == 0 )then

      byte_dshftl = bl

      return                                                         ! dshftl()

   end if

   if( i < bit_size( i) )then                                        ! if shift within one word

      btl = ishft( bl, i)
      btr = ishft( br, i - bit_size( i))

      byte_dshftl = ior( btl, btr)

   elseif( i == bit_size( i) )then                                   ! shift is exactly one word

      byte_dshftl = br

   else                                                              ! if shift out of range

      byte_dshftl = 0_byte_k

   end if

return                                                               ! dshftl()

!  byte_dshftl()

end function byte_dshftl

! **********************************************************************

!  dshftl( bl, br, i)

elemental integer( kind= short_k) function short_dshftl( bl, br, i)

integer( kind= short_k), intent( in) :: bl, br
integer( kind= short_k), intent( in) :: i

!  short_dshftl() local

   integer( kind= int_k) :: btl, btr

!  short_dshftl()

continue                                                             ! dshftl()

!  trap out endcase

   if( i < 0 )then

      short_dshftl = 0_short_k

      return                                                         ! dshftl()

   elseif( i == 0 )then

      short_dshftl = bl

      return                                                         ! dshftl()

   end if

   if( i < bit_size( i) )then                                        ! if shift within one word

      btl = ishft( bl, i)
      btr = ishft( br, i - bit_size( i))

      short_dshftl = ior( btl, btr)

   elseif( i == bit_size( i) )then                                   ! shift is exactly one word

      short_dshftl = br

   else                                                              ! if shift out of range

      short_dshftl = 0_short_k

   end if

return                                                               ! dshftl()

!  short_dshftl()

end function short_dshftl

! **********************************************************************

!  dshftl( bl, br, i)

elemental integer( kind= int_k) function int_dshftl( bl, br, i)

integer( kind= int_k), intent( in) :: bl, br
integer( kind= int_k), intent( in) :: i

!  int_dshftl() local

   integer( kind= int_k) :: btl, btr

!  int_dshftl()

continue                                                             ! dshftl()

!  trap out endcase

   if( i < 0 )then

      int_dshftl = 0_int_k

      return                                                         ! dshftl()

   elseif( i == 0 )then

      int_dshftl = bl

      return                                                         ! dshftl()

   end if

   if( i < bit_size( i) )then                                        ! if shift within one word

      btl = ishft( bl, i)
      btr = ishft( br, i - bit_size( i))

      int_dshftl = ior( btl, btr)

   elseif( i == bit_size( i) )then                                   ! shift is exactly one word

      int_dshftl = br

   else                                                              ! if shift out of range

      int_dshftl = 0_int_k

   end if

return                                                               ! dshftl()

!  int_dshftl()

end function int_dshftl

! **********************************************************************

!  dshftl( bl, br, i)

elemental integer( kind= long_k) function long_dshftl( bl, br, i)

integer( kind= long_k), intent( in) :: bl, br
integer( kind= long_k), intent( in) :: i

!  long_dshftl() local

   integer( kind= int_k) :: btl, btr

!  long_dshftl()

continue                                                             ! dshftl()

!  trap out endcase

   if( i < 0 )then

      long_dshftl = 0_long_k

      return                                                         ! dshftl()

   elseif( i == 0 )then

      long_dshftl = bl

      return                                                         ! dshftl()

   end if

   if( i < bit_size( i) )then                                        ! if shift within one word

      btl = ishft( bl, i)
      btr = ishft( br, i - bit_size( i))

      long_dshftl = ior( btl, btr)

   elseif( i == bit_size( i) )then                                   ! shift is exactly one word

      long_dshftl = br

   else                                                              ! if shift out of range

      long_dshftl = 0_long_k

   end if

return                                                               ! dshftl()

!  long_dshftl()

end function long_dshftl

! **********************************************************************

!  dshftr( bl, br, i)

elemental integer( kind= byte_k) function byte_dshftr( bl, br, i)

integer( kind= byte_k), intent( in) :: bl, br
integer( kind= byte_k), intent( in) :: i

!  byte_dshftr() local

   integer( kind= byte_k) :: btl, btr

!  byte_dshftr()

continue                                                             ! dshftr()

!  trap out endcase

   if( i < 0 )then

      byte_dshftr = 0_byte_k

      return                                                         ! dshftr()

   elseif( i == 0 )then

      byte_dshftr = br

      return                                                         ! dshftr()

   end if

   if( i < bit_size( i) )then                                        ! if shift within one word

      btl = ishft( bl, bit_size( i) - i)
      btr = ishft( br, -i)

      byte_dshftr = ior( btl, btr)

   elseif( i == bit_size( i) )then                                   ! shift is exactly one word

      byte_dshftr = bl

   else                                                              ! if shift out of range

      byte_dshftr = 0_byte_k

   end if

return                                                               ! dshftr()

!  byte_dshftr()

end function byte_dshftr

! **********************************************************************

!  dshftr( bl, br, i)

elemental integer( kind= short_k) function short_dshftr( bl, br, i)

integer( kind= short_k), intent( in) :: bl, br
integer( kind= short_k), intent( in) :: i

!  short_dshftr() local

   integer( kind= short_k) :: btl, btr

!  short_dshftr()

continue                                                             ! dshftr()

!  trap out endcase

   if( i < 0 )then

      short_dshftr = 0_short_k

      return                                                         ! dshftr()

   elseif( i == 0 )then

      short_dshftr = br

      return                                                         ! dshftr()

   end if

   if( i < bit_size( i) )then                                        ! if shift within one word

      btl = ishft( bl, bit_size( i) - i)
      btr = ishft( br, -i)

      short_dshftr = ior( btl, btr)

   elseif( i == bit_size( i) )then                                   ! shift is exactly one word

      short_dshftr = bl

   else                                                              ! if shift out of range

      short_dshftr = 0_short_k

   end if

return                                                               ! dshftr()

!  short_dshftr()

end function short_dshftr

! **********************************************************************

!  dshftr( bl, br, i)

elemental integer( kind= int_k) function int_dshftr( bl, br, i)

integer( kind= int_k), intent( in) :: bl, br
integer( kind= int_k), intent( in) :: i

!  int_dshftr() local

   integer( kind= int_k) :: btl, btr

!  int_dshftr()

continue                                                             ! dshftr()

!  trap out endcase

   if( i < 0 )then

      int_dshftr = 0_int_k

      return                                                         ! dshftr()

   elseif( i == 0 )then

      int_dshftr = br

      return                                                         ! dshftr()

   end if

   if( i < bit_size( i) )then                                        ! if shift within one word

      btl = ishft( bl, bit_size( i) - i)
      btr = ishft( br, -i)

      int_dshftr = ior( btl, btr)

   elseif( i == bit_size( i) )then                                   ! shift is exactly one word

      int_dshftr = bl

   else                                                              ! if shift out of range

      int_dshftr = 0_int_k

   end if

return                                                               ! dshftr()

!  int_dshftr()

end function int_dshftr

! **********************************************************************

!  dshftr( bl, br, i)

elemental integer( kind= long_k) function long_dshftr( bl, br, i)

integer( kind= long_k), intent( in) :: bl, br
integer( kind= long_k), intent( in) :: i

!  long_dshftr() local

   integer( kind= long_k) :: btl, btr

!  long_dshftr()

continue                                                             ! dshftr()

!  trap out endcase

   if( i < 0 )then

      long_dshftr = 0_long_k

      return                                                         ! dshftr()

   elseif( i == 0 )then

      long_dshftr = br

      return                                                         ! dshftr()

   end if

   if( i < bit_size( i) )then                                        ! if shift within one word

      btl = ishft( bl, bit_size( i) - i)
      btr = ishft( br, -i)

      long_dshftr = ior( btl, btr)

   elseif( i == bit_size( i) )then                                   ! shift is exactly one word

      long_dshftr = bl

   else                                                              ! if shift out of range

      long_dshftr = 0_long_k

   end if

return                                                               ! dshftr()

!  long_dshftr()

end function long_dshftr

! **********************************************************************

!  dshftc( bl, br, i)

elemental subroutine byte_dshftc( bl, br, i)

integer( kind= byte_k), intent( inout) :: bl, br
integer( kind= byte_k), intent( in) :: i

!  byte_dshftc() local

   integer( kind= byte_k) :: btl, btr, carryl, carryr, ia

!  byte_dshftc()

continue                                                             ! dshftc()

   ia = abs( i)

   if( ia > bit_size( i) ) return

   if( ia == bit_size( i) )then                                      ! if exactly swapping words

      btl = br
      btr = bl

      bl = btl
      br = btr

   elseif( i<bit_size( i) .and. i>=1 )then                           ! if i+ shift left

      carryl = ishft( bl, i - bit_size( i))
      carryr = ishft( br, i - bit_size( i))

      btl = ishft( bl, i)
      btr = ishft( br, i)

      bl = ior( btl, carryr)
      br = ior( btr, carryl)

   elseif( ia<bit_size( i) .and. ia>=1 )then                         ! if i- shift right

      carryl = ishft( bl, bit_size( i) + i)
      carryr = ishft( br, bit_size( i) + i)

      btl = ishft( bl, i)
      btr = ishft( br, i)

      bl = ior( btl, carryr)
      br = ior( btr, carryl)

   end if                                                             ! else do nothing

return                                                               ! dshftc()

!  byte_dshftc()

end subroutine byte_dshftc

! **********************************************************************

!  dshftc( bl, br, i)

elemental subroutine short_dshftc( bl, br, i)

integer( kind= short_k), intent( inout) :: bl, br
integer( kind= short_k), intent( in) :: i

!  short_dshftc() local

   integer( kind= short_k) :: btl, btr, carryl, carryr, ia

!  short_dshftc()

continue                                                             ! dshftc()

   ia = abs( i)

   if( ia > bit_size( i) ) return

   if( ia == bit_size( i) )then                                      ! if exactly swapping words

      btl = br
      btr = bl

      bl = btl
      br = btr

   elseif( i<bit_size( i) .and. i>=1 )then                           ! if i+ shift left

      carryl = ishft( bl, i - bit_size( i))
      carryr = ishft( br, i - bit_size( i))

      btl = ishft( bl, i)
      btr = ishft( br, i)

      bl = ior( btl, carryr)
      br = ior( btr, carryl)

   elseif( ia<bit_size( i) .and. ia>=1 )then                         ! if i- shift right

      carryl = ishft( bl, bit_size( i) + i)
      carryr = ishft( br, bit_size( i) + i)

      btl = ishft( bl, i)
      btr = ishft( br, i)

      bl = ior( btl, carryr)
      br = ior( btr, carryl)

   end if                                                             ! else do nothing

return                                                               ! dshftc()

!  short_dshftc()

end subroutine short_dshftc

! **********************************************************************

!  dshftc( bl, br, i)

elemental subroutine int_dshftc( bl, br, i)

integer( kind= int_k), intent( inout) :: bl, br
integer( kind= int_k), intent( in) :: i

!  int_dshftc() local

   integer( kind= int_k) :: btl, btr, carryl, carryr, ia

!  int_dshftc()

continue                                                             ! dshftc()

   ia = abs( i)

   if( ia > bit_size( i) ) return

   if( ia == bit_size( i) )then                                      ! if exactly swapping words

      btl = br
      btr = bl

      bl = btl
      br = btr

   elseif( i<bit_size( i) .and. i>=1 )then                           ! if i+ shift left

      carryl = ishft( bl, i - bit_size( i))
      carryr = ishft( br, i - bit_size( i))

      btl = ishft( bl, i)
      btr = ishft( br, i)

      bl = ior( btl, carryr)
      br = ior( btr, carryl)

   elseif( ia<bit_size( i) .and. ia>=1 )then                         ! if i- shift right

      carryl = ishft( bl, bit_size( i) + i)
      carryr = ishft( br, bit_size( i) + i)

      btl = ishft( bl, i)
      btr = ishft( br, i)

      bl = ior( btl, carryr)
      br = ior( btr, carryl)

   end if                                                             ! else do nothing

return                                                               ! dshftc()

!  int_dshftc()

end subroutine int_dshftc

! **********************************************************************

!  dshftc( bl, br, i)

elemental subroutine long_dshftc( bl, br, i)

integer( kind= long_k), intent( inout) :: bl, br
integer( kind= long_k), intent( in) :: i

!  long_dshftc() local

   integer( kind= long_k) :: btl, btr, carryl, carryr, ia

!  long_dshftc()

continue                                                             ! dshftc()

   ia = abs( i)

   if( ia > bit_size( i) ) return

   if( ia == bit_size( i) )then                                      ! if exactly swapping words

      btl = br
      btr = bl

      bl = btl
      br = btr

   elseif( i<bit_size( i) .and. i>=1 )then                           ! if i+ shift left

      carryl = ishft( bl, i - bit_size( i))
      carryr = ishft( br, i - bit_size( i))

      btl = ishft( bl, i)
      btr = ishft( br, i)

      bl = ior( btl, carryr)
      br = ior( btr, carryl)

   elseif( ia<bit_size( i) .and. ia>=1 )then                         ! if i- shift right

      carryl = ishft( bl, bit_size( i) + i)
      carryr = ishft( br, bit_size( i) + i)

      btl = ishft( bl, i)
      btr = ishft( br, i)

      bl = ior( btl, carryr)
      br = ior( btr, carryl)

   end if                                                             ! else do nothing

return                                                               ! dshftc()

!  long_dshftc()

end subroutine long_dshftc

! **********************************************************************

!  bit-wise unary operator- .not. i

! **********************************************************************

!  byte_not()- .not. for kind byte

elemental integer( kind= byte_k) function byte_not( i)

integer( kind= byte_k), intent( in) :: i

!  byte_not()

continue                                                             ! .not. i

   byte_not = not( i)

return                                                               ! .not. i

!  byte_not()

end function byte_not

! **********************************************************************

!  short_not()- .not. for kind short

elemental integer( kind= short_k) function short_not( i)

integer( kind= short_k), intent( in) :: i

!  short_not()

continue                                                             ! .not. i

   short_not = not( i)

return                                                               ! .not. i

!  short_not()

end function short_not

! **********************************************************************

!  int_not()- .not. for kind int

elemental integer( kind= int_k) function int_not( i)

integer( kind= int_k), intent( in) :: i

!  int_not()

continue                                                             ! .not. i

   int_not = not( i)

return                                                               ! .not. i

!  int_not()

end function int_not

! **********************************************************************

!  long_not()- .not. for kind long

elemental integer( kind= long_k) function long_not( i)

integer( kind= long_k), intent( in) :: i

!  long_not()

continue                                                             ! .not. i

   long_not = not( i)

return                                                               ! .not. i

!  long_not()

end function long_not

! **********************************************************************

!  bit-wise binary operator-  i1 .and. i2

! **********************************************************************

!  byte_and()- .and. for kind byte

elemental integer( kind= byte_k) function byte_and( i1, i2)

integer( kind= byte_k), intent( in) :: i1, i2

!  byte_and()

continue                                                             ! i .and. i

   byte_and = iand( i1, i2)

return                                                               ! i .and. i

!  byte_and()

end function byte_and

! **********************************************************************

!  short_and()- .and. for kind short

elemental integer( kind= short_k) function short_and( i1, i2)

integer( kind= short_k), intent( in) :: i1, i2

!  short_and()

continue                                                             ! i .and. i

   short_and = iand( i1, i2)

return                                                               ! i .and. i

!  short_and()

end function short_and

! **********************************************************************

!  int_and()- .and. for kind int

elemental integer( kind= int_k) function int_and( i1, i2)

integer( kind= int_k), intent( in) :: i1, i2

!  int_and()

continue                                                             ! i .and. i

   int_and = iand( i1, i2)

return                                                               ! i .and. i

!  int_and()

end function int_and

! **********************************************************************

!  long_and()- .and. for kind long

elemental integer( kind= long_k) function long_and( i1, i2)

integer( kind= long_k), intent( in) :: i1, i2

!  long_and()

continue                                                             ! i .and. i

   long_and = iand( i1, i2)

return                                                               ! i .and. i

!  long_and()

end function long_and

! **********************************************************************

!  bit-wise binary operator-  i1 .or. i2

! **********************************************************************

!  byte_or()- .or. for kind byte

elemental integer( kind= byte_k) function byte_or( i1, i2)

integer( kind= byte_k), intent( in) :: i1, i2

!  byte_or()

continue                                                             ! i .or. i

   byte_or = ior( i1, i2)

return                                                               ! i .or. i

!  byte_or()

end function byte_or

! **********************************************************************

!  short_or()- .or. for kind short

elemental integer( kind= short_k) function short_or( i1, i2)

integer( kind= short_k), intent( in) :: i1, i2

!  short_or()

continue                                                             ! i .or. i

   short_or = ior( i1, i2)

return                                                               ! i .or. i

!  short_or()

end function short_or

! **********************************************************************

!  int_or()- .or. for kind int

elemental integer( kind= int_k) function int_or( i1, i2)

integer( kind= int_k), intent( in) :: i1, i2

!  int_or()

continue                                                             ! i .or. i

   int_or = ior( i1, i2)

return                                                               ! i .or. i

!  int_or()

end function int_or

! **********************************************************************

!  long_or()- .or. for kind long

elemental integer( kind= long_k) function long_or( i1, i2)

integer( kind= long_k), intent( in) :: i1, i2

!  long_or()

continue                                                             ! i .or. i

   long_or = ior( i1, i2)

return                                                               ! i .or. i

!  long_or()

end function long_or

! **********************************************************************

!  bit-wise binary operator-  i1 .eor. i2

! **********************************************************************

!  byte_eor()- .eor. for kind byte

elemental integer( kind= byte_k) function byte_eor( i1, i2)

integer( kind= byte_k), intent( in) :: i1, i2

!  byte_eor()

continue                                                             ! i .eor. i

   byte_eor = ieor( i1, i2)

return                                                               ! i .eor. i

!  byte_eor()

end function byte_eor

! **********************************************************************

!  short_eor()- .eor. for kind short

elemental integer( kind= short_k) function short_eor( i1, i2)

integer( kind= short_k), intent( in) :: i1, i2

!  short_eor()

continue                                                             ! i .eor. i

   short_eor = ieor( i1, i2)

return                                                               ! i .eor. i

!  short_eor()

end function short_eor

! **********************************************************************

!  int_eor()- .eor. for kind int

elemental integer( kind= int_k) function int_eor( i1, i2)

integer( kind= int_k), intent( in) :: i1, i2

!  int_eor()

continue                                                             ! i .eor. i

   int_eor = ieor( i1, i2)

return                                                               ! i .eor. i

!  int_eor()

end function int_eor

! **********************************************************************

!  long_eor()- .eor. for kind long

elemental integer( kind= long_k) function long_eor( i1, i2)

integer( kind= long_k), intent( in) :: i1, i2

!  long_eor()

continue                                                             ! i .eor. i

   long_eor = ieor( i1, i2)

return                                                               ! i .eor. i

!  long_eor()

end function long_eor

! **********************************************************************

!  bit-wise binary operator-  i1 .eqv. i2

! **********************************************************************

!  byte_eqv()- .eqv. for kind byte

elemental integer( kind= byte_k) function byte_eqv( b1, b2)

integer( kind= byte_k), intent( in) :: b1, b2

!  byte_eqv()

continue                                                             ! i .eqv. i

   byte_eqv = not( ieor( b1, b2) )

return                                                               ! i .eqv. i

!  byte_eqv()

end function byte_eqv

! **********************************************************************

!  short_eqv()- .eqv. for kind short

elemental integer( kind= short_k) function short_eqv( b1, b2)

integer( kind= short_k), intent( in) :: b1, b2

!  short_eqv()

continue                                                             ! i .eqv. i

   short_eqv = not( ieor( b1, b2) )

return                                                               ! i .eqv. i

!  short_eqv()

end function short_eqv

! **********************************************************************

!  int_eqv()- .eqv. for kind int

elemental integer( kind= int_k) function int_eqv( b1, b2)

integer( kind= int_k), intent( in) :: b1, b2

!  int_eqv()

continue                                                             ! i .eqv. i

   int_eqv = not( ieor( b1, b2) )

return                                                               ! i .eqv. i

!  int_eqv()

end function int_eqv

! **********************************************************************

!  long_eqv()- .eqv. for kind long

elemental integer( kind= long_k) function long_eqv( b1, b2)

integer( kind= long_k), intent( in) :: b1, b2

!  long_eqv()

continue                                                             ! i .eqv. i

   long_eqv = not( ieor( b1, b2) )

return                                                               ! i .eqv. i

!  long_eqv()

end function long_eqv

! **********************************************************************

!  bit-wise binary operator-  i1 .neqv. i2

! **********************************************************************

!  byte_neqv()- .neqv. for kind byte

elemental integer( kind= byte_k) function byte_neqv( b1, b2)

integer( kind= byte_k), intent( in) :: b1, b2

!  byte_neqv()

continue                                                             ! i .neqv. i

   byte_neqv = ieor( b1, b2)

return                                                               ! i .neqv. i

!  byte_neqv()

end function byte_neqv

! **********************************************************************

!  short_neqv()- .neqv. for kind short

elemental integer( kind= short_k) function short_neqv( b1, b2)

integer( kind= short_k), intent( in) :: b1, b2

!  short_neqv()

continue                                                             ! i .neqv. i

   short_neqv = ieor( b1, b2)

return                                                               ! i .neqv. i

!  short_neqv()

end function short_neqv

! **********************************************************************

!  int_neqv()- .neqv. for kind int

elemental integer( kind= int_k) function int_neqv( b1, b2)

integer( kind= int_k), intent( in) :: b1, b2

!  int_neqv()

continue                                                             ! i .neqv. i

   int_neqv = ieor( b1, b2)

return                                                               ! i .neqv. i

!  int_neqv()

end function int_neqv

! **********************************************************************

!  long_neqv()- .neqv. for kind long

elemental integer( kind= long_k) function long_neqv( b1, b2)

integer( kind= long_k), intent( in) :: b1, b2

!  long_neqv()

continue                                                             ! i .neqv. i

   long_neqv = ieor( b1, b2)

return                                                               ! i .neqv. i

!  long_neqv()

end function long_neqv

! **********************************************************************

!  bit-wise binary operator:  i1 .xor. i2

! **********************************************************************

!  byte_xor(): xor() for kind byte

elemental integer( kind= byte_k) function byte_xor( i1, i2)

integer( kind= byte_k), intent( in) :: i1, i2

!  byte_xor()

continue                                                             ! i .xor. i

   byte_xor = ieor( i1, i2)

return                                                               ! i .xor. i

!  byte_xor()

end function byte_xor

! **********************************************************************

!  short_xor(): xor() for kind short

elemental integer( kind= short_k) function short_xor( i1, i2)

integer( kind= short_k), intent( in) :: i1, i2

!  short_xor()

continue                                                             ! i .xor. i

   short_xor = ieor( i1, i2)

return                                                               ! i .xor. i

!  short_xor()

end function short_xor

! **********************************************************************

!  int_xor(): xor() for kind int

elemental integer( kind= int_k) function int_xor( i1, i2)

integer( kind= int_k), intent( in) :: i1, i2

!  int_xor()

continue                                                             ! i .xor. i

   int_xor = ieor( i1, i2)

return                                                               ! i .xor. i

!  int_xor()

end function int_xor

! **********************************************************************

!  long_xor(): xor() for kind long

elemental integer( kind= long_k) function long_xor( i1, i2)

integer( kind= long_k), intent( in) :: i1, i2

!  long_xor()

continue                                                             ! i .xor. i

   long_xor = ieor( i1, i2)

return                                                               ! i .xor. i

!  long_xor()

end function long_xor

! **********************************************************************

!  bit_functions

! **********************************************************************
! $Id: bitfunc.fpp 1.3 2003/10/03 19:28:00Z Dan Release $

end module bit_functions                                             ! eof

