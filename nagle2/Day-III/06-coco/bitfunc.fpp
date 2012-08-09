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

?? ! *******************************************************************

?? ! preprocessor definitions

?? include 'coco.inc'

?? ! *******************************************************************

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
?? if( byte_k )then
   module procedure byte_csmg
?? end if
?? if( short_k )then
   module procedure short_csmg
?? end if
?? if( int_k )then
   module procedure int_csmg
?? end if
?? if( long_k )then
   module procedure long_csmg
?? end if
end interface

!  declare specific functions implementing the compl() function

public :: compl                                                      ! generic name

interface compl
?? if( byte_k )then
   module procedure byte_compl
?? end if
?? if( short_k )then
   module procedure short_compl
?? end if
?? if( int_k )then
   module procedure int_compl
?? end if
?? if( long_k )then
   module procedure long_compl
?? end if
end interface

! **********************************************************************

!  declare specific functions supporting generic function leadz()

public :: leadz                                                      ! generic name

interface leadz
?? if( byte_k )then
   module procedure byte_leadz
?? end if
?? if( short_k )then
   module procedure short_leadz
?? end if
?? if( int_k )then
   module procedure int_leadz
?? end if
?? if( long_k )then
   module procedure long_leadz
?? end if
end interface

! **********************************************************************

!  declare specific functions supporting generic function lastz()

public :: lastz                                                      ! generic name

interface lastz
?? if( byte_k )then
   module procedure byte_lastz
?? end if
?? if( short_k )then
   module procedure short_lastz
?? end if
?? if( int_k )then
   module procedure int_lastz
?? end if
?? if( long_k )then
   module procedure long_lastz
?? end if
end interface

! **********************************************************************

!  declare specific functions supporting generic function popcnt()

public :: popcnt                                                     ! generic name

interface popcnt
?? if( byte_k )then
   module procedure byte_popcnt
?? end if
?? if( short_k )then
   module procedure short_popcnt
?? end if
?? if( int_k )then
   module procedure int_popcnt
?? end if
?? if( long_k )then
   module procedure long_popcnt
?? end if
end interface

! **********************************************************************

!  declare specific functions supporting generic function poppar()

public :: poppar                                                     ! generic name

interface poppar
?? if( byte_k )then
   module procedure byte_poppar
?? end if
?? if( short_k )then
   module procedure short_poppar
?? end if
?? if( int_k )then
   module procedure int_poppar
?? end if
?? if( long_k )then
   module procedure long_poppar
?? end if
end interface

! **********************************************************************

!  bit length of an integer .ilen. ilen()

! **********************************************************************

!  declare specific functions supporting .ilen.

public :: operator( .ilen.)                                          ! operator name

interface operator( .ilen.)
?? if( byte_k )then
   module procedure byte_ilen
?? end if
?? if( short_k )then
   module procedure short_ilen
?? end if
?? if( int_k )then
   module procedure int_ilen
?? end if
?? if( long_k )then
   module procedure long_ilen
?? end if
end interface

!  declare specific functions supporting generic ilen()

public :: ilen                                                       ! generic name

interface ilen
?? if( byte_k )then
   module procedure byte_ilen
?? end if
?? if( short_k )then
   module procedure short_ilen
?? end if
?? if( int_k )then
   module procedure int_ilen
?? end if
?? if( long_k )then
   module procedure long_ilen
?? end if
end interface

! **********************************************************************

!  hamming distance .hamd. hamd()

! **********************************************************************

!  declare specific functions supporting .hamd.

public :: operator( .hamd.)                                          ! operator name

interface operator( .hamd.)
?? if( byte_k )then
   module procedure byte_hamd
?? end if
?? if( short_k )then
   module procedure short_hamd
?? end if
?? if( int_k )then
   module procedure int_hamd
?? end if
?? if( long_k )then
   module procedure long_hamd
?? end if
end interface

!  declare specific functions supporting generic hamd()

public :: hamd                                                       ! generic name

interface hamd
?? if( byte_k )then
   module procedure byte_hamd
?? end if
?? if( short_k )then
   module procedure short_hamd
?? end if
?? if( int_k )then
   module procedure int_hamd
?? end if
?? if( long_k )then
   module procedure long_hamd
?? end if
end interface

! **********************************************************************

!  mask functions mask(), maskl(), maskr()

! **********************************************************************

!  declare specific functions supporting generic function mask()

public :: mask                                                       ! generic name

interface mask
?? if( byte_k )then
   module procedure byte_mask
?? end if
?? if( short_k )then
   module procedure short_mask
?? end if
?? if( int_k )then
   module procedure int_mask
?? end if
?? if( long_k )then
   module procedure long_mask
?? end if
end interface

! **********************************************************************

!  declare specific functions supporting generic function maskl()

public :: maskl                                                      ! generic name

interface maskl
?? if( byte_k )then
   module procedure byte_maskl
?? end if
?? if( short_k )then
   module procedure short_maskl
?? end if
?? if( int_k )then
   module procedure int_maskl
?? end if
?? if( long_k )then
   module procedure long_maskl
?? end if
end interface

! **********************************************************************

!  declare specific functions supporting generic function maskr()

public :: maskr                                                      ! generic name

interface maskr
?? if( byte_k )then
   module procedure byte_maskr
?? end if
?? if( short_k )then
   module procedure short_maskr
?? end if
?? if( int_k )then
   module procedure int_maskr
?? end if
?? if( long_k )then
   module procedure long_maskr
?? end if
end interface

! **********************************************************************

!  shifts as binary operators .shift. .rotate.

! **********************************************************************

!  declare specific functions supporting .shift.

public :: operator( .shift.)                                         ! operator name

interface operator( .shift.)
?? if( byte_k )then
   module procedure byte_shift
?? end if
?? if( short_k )then
   module procedure short_shift
?? end if
?? if( int_k )then
   module procedure int_shift
?? end if
?? if( long_k )then
   module procedure long_shift
?? end if
end interface

! **********************************************************************

!  declare specific functions supporting .rotate.

public :: operator( .rotate.)                                        ! operator name

interface operator( .rotate.)
?? if( byte_k )then
   module procedure byte_rotate
?? end if
?? if( short_k )then
   module procedure short_rotate
?? end if
?? if( int_k )then
   module procedure int_rotate
?? end if
?? if( long_k )then
   module procedure long_rotate
?? end if
end interface

! **********************************************************************

!  two word shift functions dshftl(), dshftr(), dshftc()

! **********************************************************************

!  declare specific functions supporting generic function dshftl()

public :: dshftl                                                     ! generic name

interface dshftl
?? if( byte_k )then
   module procedure byte_dshftl
?? end if
?? if( short_k )then
   module procedure short_dshftl
?? end if
?? if( int_k )then
   module procedure int_dshftl
?? end if
?? if( long_k )then
   module procedure long_dshftl
?? end if
end interface

! **********************************************************************

!  declare specific functions supporting generic function dshftr()

public :: dshftr                                                     ! generic name

interface dshftr
?? if( byte_k )then
   module procedure byte_dshftr
?? end if
?? if( short_k )then
   module procedure short_dshftr
?? end if
?? if( int_k )then
   module procedure int_dshftr
?? end if
?? if( long_k )then
   module procedure long_dshftr
?? end if
end interface

! **********************************************************************

!  declare specific functions supporting generic function dshftc()

public :: dshftc                                                     ! generic name

interface dshftc
?? if( byte_k )then
   module procedure byte_dshftc
?? end if
?? if( short_k )then
   module procedure short_dshftc
?? end if
?? if( int_k )then
   module procedure int_dshftc
?? end if
?? if( long_k )then
   module procedure long_dshftc
?? end if
end interface

! **********************************************************************

!  unary operator: .not.

! **********************************************************************

!  declare specific functions implementing the .not. operator

public :: operator( .not.)                                           ! operator name

interface operator( .not.)
?? if( byte_k )then
   module procedure byte_not
?? end if
?? if( short_k )then
   module procedure short_not
?? end if
?? if( int_k )then
   module procedure int_not
?? end if
?? if( long_k )then
   module procedure long_not
?? end if
end interface

! **********************************************************************

!  binary operators: .and., .or., .eor., .xor., .eqv., .neqv., .xor.

! **********************************************************************

!  declare specific functions implementing the .and. operator

public :: operator( .and.)                                           ! operator name

interface operator( .and.)
?? if( byte_k )then
   module procedure byte_and
?? end if
?? if( short_k )then
   module procedure short_and
?? end if
?? if( int_k )then
   module procedure int_and
?? end if
?? if( long_k )then
   module procedure long_and
?? end if
end interface

!  declare specific functions implementing the .or. operator

public :: operator( .or.)                                            ! operator name

interface operator( .or.)
?? if( byte_k )then
   module procedure byte_or
?? end if
?? if( short_k )then
   module procedure short_or
?? end if
?? if( int_k )then
   module procedure int_or
?? end if
?? if( long_k )then
   module procedure long_or
?? end if
end interface

!  declare specific functions implementing the .eor. operator

public :: operator( .eor.)                                           ! operator name

interface operator( .eor.)
?? if( byte_k )then
   module procedure byte_eor
?? end if
?? if( short_k )then
   module procedure short_eor
?? end if
?? if( int_k )then
   module procedure int_eor
?? end if
?? if( long_k )then
   module procedure long_eor
?? end if
end interface

!  declare specific functions implementing the .eqv. operator

public :: operator( .eqv.)                                           ! operator name

interface operator( .eqv.)
?? if( byte_k )then
   module procedure byte_eqv
?? end if
?? if( short_k )then
   module procedure short_eqv
?? end if
?? if( int_k )then
   module procedure int_eqv
?? end if
?? if( long_k )then
   module procedure long_eqv
?? end if
end interface

!  declare specific functions implementing the .neqv. operator

public :: operator( .neqv.)                                          ! operator name

interface operator( .neqv.)
?? if( byte_k )then
   module procedure byte_neqv
?? end if
?? if( short_k )then
   module procedure short_neqv
?? end if
?? if( int_k )then
   module procedure int_neqv
?? end if
?? if( long_k )then
   module procedure long_neqv
?? end if
end interface

!  declare specific functions implementing the .xor. operator

public :: operator( .xor.)                                           ! operator name

interface operator( .xor.)
?? if( byte_k )then
   module procedure byte_xor
?? end if
?? if( short_k )then
   module procedure short_xor
?? end if
?? if( int_k )then
   module procedure int_xor
?? end if
?? if( long_k )then
   module procedure long_xor
?? end if
end interface

! **********************************************************************

!  private data

! **********************************************************************

?? if( byte_k )then
!  mask, maskl, maskr data for byte_k

integer( kind= byte_k), dimension( bit_size( 0_byte_k) ), save :: &
      byte_left_mask, byte_right_mask

data &
    byte_left_mask/ z'80', z'c0', z'e0', z'f0', &
                    z'f8', z'fc', z'fe', z'ff'/

data &
   byte_right_mask/ z'01', z'03', z'07', z'0f', &
                    z'1f', z'3f', z'7f', z'ff'/

?? end if
?? if( short_k )then
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

?? end if
?? if( int_k )then
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

?? end if
?? if( long_k )then
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

?? end if
! **********************************************************************

!  masks for leadz()

?? if( byte_k )then
integer( kind= byte_k), save :: byte_lead_p4; data byte_lead_p4/ z'f0'/
integer( kind= byte_k), save :: byte_lead_p2; data byte_lead_p2/ z'cc'/
integer( kind= byte_k), save :: byte_lead_p1; data byte_lead_p1/ z'aa'/

?? end if
?? if( short_k )then
integer( kind= short_k), save :: short_lead_p8; data short_lead_p8/ z'ff00'/
integer( kind= short_k), save :: short_lead_p4; data short_lead_p4/ z'f0f0'/
integer( kind= short_k), save :: short_lead_p2; data short_lead_p2/ z'cccc'/
integer( kind= short_k), save :: short_lead_p1; data short_lead_p1/ z'aaaa'/

?? end if
?? if( int_k )then
integer( kind= int_k), save :: int_lead_p16; data int_lead_p16/ z'ffff0000'/
integer( kind= int_k), save :: int_lead_p8; data int_lead_p8/ z'ff00ff00'/
integer( kind= int_k), save :: int_lead_p4; data int_lead_p4/ z'f0f0f0f0'/
integer( kind= int_k), save :: int_lead_p2; data int_lead_p2/ z'cccccccc'/
integer( kind= int_k), save :: int_lead_p1; data int_lead_p1/ z'aaaaaaaa'/

?? end if
?? if( long_k )then
integer( kind= long_k), save :: long_lead_p32; data long_lead_p32/ z'ffffffff00000000'/
integer( kind= long_k), save :: long_lead_p16; data long_lead_p16/ z'ffff0000ffff0000'/
integer( kind= long_k), save :: long_lead_p8; data long_lead_p8/ z'ff00ff00ff00ff00'/
integer( kind= long_k), save :: long_lead_p4; data long_lead_p4/ z'f0f0f0f0f0f0f0f0'/
integer( kind= long_k), save :: long_lead_p2; data long_lead_p2/ z'cccccccccccccccc'/
integer( kind= long_k), save :: long_lead_p1; data long_lead_p1/ z'aaaaaaaaaaaaaaaa'/
?? end if

! **********************************************************************

!  masks for lastz()

?? if( byte_k )then
integer( kind= byte_k), save :: byte_last_p4; data byte_last_p4/ z'0f'/
integer( kind= byte_k), save :: byte_last_p2; data byte_last_p2/ z'33'/
integer( kind= byte_k), save :: byte_last_p1; data byte_last_p1/ z'55'/

?? end if
?? if( short_k )then
integer( kind= short_k), save :: short_last_p8; data short_last_p8/ z'00ff'/
integer( kind= short_k), save :: short_last_p4; data short_last_p4/ z'0f0f'/
integer( kind= short_k), save :: short_last_p2; data short_last_p2/ z'3333'/
integer( kind= short_k), save :: short_last_p1; data short_last_p1/ z'5555'/

?? end if
?? if( int_k )then
integer( kind= int_k), save :: int_last_p16; data int_last_p16/ z'0000ffff'/
integer( kind= int_k), save :: int_last_p8; data int_last_p8/ z'00ff00ff'/
integer( kind= int_k), save :: int_last_p4; data int_last_p4/ z'0f0f0f0f'/
integer( kind= int_k), save :: int_last_p2; data int_last_p2/ z'33333333'/
integer( kind= int_k), save :: int_last_p1; data int_last_p1/ z'55555555'/

?? end if
?? if( long_k )then
integer( kind= long_k), save :: long_last_p32; data long_last_p32/ z'00000000ffffffff'/
integer( kind= long_k), save :: long_last_p16; data long_last_p16/ z'0000ffff0000ffff'/
integer( kind= long_k), save :: long_last_p8; data long_last_p8/ z'00ff00ff00ff00ff'/
integer( kind= long_k), save :: long_last_p4; data long_last_p4/ z'0f0f0f0f0f0f0f0f'/
integer( kind= long_k), save :: long_last_p2; data long_last_p2/ z'3333333333333333'/
integer( kind= long_k), save :: long_last_p1; data long_last_p1/ z'5555555555555555'/

?? end if
! **********************************************************************

!  masks for popcnt()/poppar()

?? if( byte_k )then
integer( kind= byte_k), save :: byte_p1; data byte_p1/ z'11'/
integer( kind= byte_k), save :: byte_p2; data byte_p2/ z'22'/
integer( kind= byte_k), save :: byte_p4; data byte_p4/ z'44'/
integer( kind= byte_k), save :: byte_p8; data byte_p8/ z'88'/

integer( kind= byte_k), save :: byte_hi_nibble; data byte_hi_nibble/ z'f0'/
integer( kind= byte_k), save :: byte_lo_nibble; data byte_lo_nibble/ z'0f'/

integer( kind= byte_k), save :: byte_low_bit; data byte_low_bit/ z'01'/

?? end if
?? if( short_k )then
integer( kind= short_k), save :: short_p1; data short_p1/ z'1111'/
integer( kind= short_k), save :: short_p2; data short_p2/ z'2222'/
integer( kind= short_k), save :: short_p4; data short_p4/ z'4444'/
integer( kind= short_k), save :: short_p8; data short_p8/ z'8888'/

integer( kind= short_k), save :: short_hi_nibble; data short_hi_nibble/ z'f0f0'/
integer( kind= short_k), save :: short_lo_nibble; data short_lo_nibble/ z'0f0f'/

integer( kind= short_k), save :: short_low_byte; data short_low_byte/ z'00ff'/

integer( kind= short_k), save :: short_low_bit; data short_low_bit/ z'0001'/

?? end if
?? if( int_k )then
integer( kind= int_k), save :: int_p1; data int_p1/ z'11111111'/
integer( kind= int_k), save :: int_p2; data int_p2/ z'22222222'/
integer( kind= int_k), save :: int_p4; data int_p4/ z'44444444'/
integer( kind= int_k), save :: int_p8; data int_p8/ z'88888888'/

integer( kind= int_k), save :: int_hi_nibble; data int_hi_nibble/ z'f0f0f0f0'/
integer( kind= int_k), save :: int_lo_nibble; data int_lo_nibble/ z'0f0f0f0f'/

integer( kind= int_k), save :: int_low_byte; data int_low_byte/ z'000000ff'/

integer( kind= int_k), save :: int_low_bit; data int_low_bit/ z'00000001'/

?? end if
?? if( long_k )then
integer( kind= long_k), save :: long_p1; data long_p1/ z'1111111111111111'/
integer( kind= long_k), save :: long_p2; data long_p2/ z'2222222222222222'/
integer( kind= long_k), save :: long_p4; data long_p4/ z'4444444444444444'/
integer( kind= long_k), save :: long_p8; data long_p8/ z'8888888888888888'/

integer( kind= long_k), save :: long_hi_nibble; data long_hi_nibble/ z'f0f0f0f0f0f0f0f0'/
integer( kind= long_k), save :: long_lo_nibble; data long_lo_nibble/ z'0f0f0f0f0f0f0f0f'/

integer( kind= long_k), save :: long_low_byte; data long_low_byte/ z'00000000000000ff'/

integer( kind= long_k), save :: long_low_bit; data long_low_bit/ z'0000000000000001'/

?? end if
! **********************************************************************

!  module procedures

! **********************************************************************

contains                                                   ! bit_functions

! **********************************************************************

!  csmg(): conditional scalar merge for integer kinds

?? text :: csmg( kind)
! **********************************************************************

!  ?kind?_csmg(): csmg() for kind ?kind?

elemental integer( kind= ?kind?_k) function ?kind?_csmg( i, j, k)

integer( kind= ?kind?_k), intent( in) :: i, j, k

!  ?kind?_csmg()

continue                                                             ! csmg()

   ?kind?_csmg = ior( iand( i, k), iand( j, not( k)) )

return                                                               ! csmg()

!  ?kind?_csmg()

end function ?kind?_csmg

?? end text csmg
?? if( byte_k )then
?? copy :: csmg( byte)
?? end if
?? if( short_k )then
?? copy :: csmg( short)
?? end if
?? if( int_k )then
?? copy :: csmg( int)
?? end if
?? if( long_k )then
?? copy :: csmg( long)
?? end if
! **********************************************************************

!  compl(): bit-wise complement

?? text :: compl( kind)
! **********************************************************************

!  ?kind?_compl(): compl() for kind ?kind?

elemental integer( kind= ?kind?_k) function ?kind?_compl( i)

integer( kind= ?kind?_k), intent( in) :: i

!  ?kind?_compl()

continue                                                             ! compl()

   ?kind?_compl = not( i)

return                                                               ! compl()

!  ?kind?_compl()

end function ?kind?_compl

?? end text compl
?? if( byte_k )then
?? copy :: compl( byte)
?? end if
?? if( short_k )then
?? copy :: compl( short)
?? end if
?? if( int_k )then
?? copy :: compl( int)
?? end if
?? if( long_k )then
?? copy :: compl( long)
?? end if
! **********************************************************************

!  bit counts: leadz(), lastz(), popcnt(), poppar()

?? if( byte_k )then
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

?? end if
?? if( short_k )then
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

?? end if
?? if( int_k )then
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

?? end if
?? if( long_k )then
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

?? end if
?? if( byte_k )then
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

?? end if
?? if( short_k )then
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

?? end if
?? if( int_k )then
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

?? end if
?? if( long_k )then
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

?? end if
?? if( byte_k )then
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

?? end if
?? if( short_k )then
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

?? end if
?? if( int_k )then
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

?? end if
?? if( long_k )then
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

?? end if
?? if( byte_k )then
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

?? end if
?? if( short_k )then
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

?? end if
?? if( int_k )then
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

?? end if
?? if( long_k )then
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

?? end if
! **********************************************************************

!  .ilen. i, ilen( i)

?? text :: ilen( kind)
! **********************************************************************

!  ?kind?_ilen()

elemental integer( kind= ?kind?_k) function ?kind?_ilen( i)

integer( kind= ?kind?_k), intent( in) :: i

!  ?kind?_ilen()

continue                                                             ! .ilen. i, ilen()

   ?kind?_ilen = bit_size( i) - leadz( abs( i))

return                                                               ! .ilen i, ilen()

!  ?kind?_ilen()

end function ?kind?_ilen

?? end text ilen
?? if( byte_k )then
?? copy :: ilen( byte)
?? end if
?? if( short_k )then
?? copy :: ilen( short)
?? end if
?? if( int_k )then
?? copy :: ilen( int)
?? end if
?? if( long_k )then
?? copy :: ilen( long)
?? end if
! **********************************************************************

!  i .hamd. i, hamd( i, i)

?? text :: hamd( kind)
! **********************************************************************

!  ?kind?_hamd()

elemental integer( kind= ?kind?_k) function ?kind?_hamd( i, j)

integer( kind= ?kind?_k), intent( in) :: i, j

!  ?kind?_hamd()

continue                                                             ! i .hamd. i, hamd()

   ?kind?_hamd = popcnt( ieor( i, j))

return                                                               ! i .hamd i, hamd()

!  ?kind?_hamd()

end function ?kind?_hamd

?? end text hamd
?? if( byte_k )then
?? copy :: hamd( byte)
?? end if
?? if( short_k )then
?? copy :: hamd( short)
?? end if
?? if( int_k )then
?? copy :: hamd( int)
?? end if
?? if( long_k )then
?? copy :: hamd( long)
?? end if
! **********************************************************************

!  masks: mask(), maskl(), maskr()

?? text :: mask( kind)
! **********************************************************************

!  mask( i)

elemental integer( kind= ?kind?_k) function ?kind?_mask( i)

integer( kind= ?kind?_k), intent( in) :: i

!  ?kind?_mask() local

   integer, parameter :: bs = bit_size( i)                           ! bits in kind i

!  ?kind?_mask()

continue                                                             ! mask()

   bits: select case( i)

   case( 1: bs) bits                                                 ! [ 1, 8] ==> 00... 0011... 11

      ?kind?_mask = ?kind?_right_mask( i)

   case( -bs: -1) bits                                               ! [ -8, -1] ==> 11... 1100... 00

      ?kind?_mask = ?kind?_left_mask( abs( i))

   case default bits                                                 ! otherwise 00... 00

      ?kind?_mask = 0_?kind?_k

   end select bits

return                                                               ! mask()

!  ?kind?_mask()

end function ?kind?_mask

?? end text mask
?? if( byte_k )then
?? copy :: mask( byte)
?? end if
?? if( short_k )then
?? copy :: mask( short)
?? end if
?? if( int_k )then
?? copy :: mask( int)
?? end if
?? if( long_k )then
?? copy :: mask( long)
?? end if
?? text :: maskl( kind)
! **********************************************************************

!  maskl( i)

elemental integer( kind= ?kind?_k) function ?kind?_maskl( i)

integer( kind= ?kind?_k), intent( in) :: i

!  ?kind?_maskl() local

   integer, parameter :: bs = bit_size( i)                           ! bits in kind i

!  ?kind?_maskl()

continue                                                             ! maskl()

   bits: select case( i)

   case( 1: bs) bits                                                 ! [ 1, 8] ==> 11... 1100... 00

      ?kind?_maskl = ?kind?_left_mask( i)

   case default bits                                                 ! otherwise 00... 00

      ?kind?_maskl = 0_?kind?_k

   end select bits

return                                                               ! maskl()

!  ?kind?_maskl()

end function ?kind?_maskl

?? end text maskl
?? if( byte_k )then
?? copy :: maskl( byte)
?? end if
?? if( short_k )then
?? copy :: maskl( short)
?? end if
?? if( int_k )then
?? copy :: maskl( int)
?? end if
?? if( long_k )then
?? copy :: maskl( long)
?? end if
?? text :: maskr( kind)
! **********************************************************************

!  maskr( i)

elemental integer( kind= ?kind?_k) function ?kind?_maskr( i)

integer( kind= ?kind?_k), intent( in) :: i

!  ?kind?_maskr() local

   integer, parameter :: bs = bit_size( i)                           ! bits in kind i

!  ?kind?_maskr()

continue                                                             ! maskr()

   bits: select case( i)

   case( 1: bs) bits                                                 ! [ 1, 8] ==> 00... 0011... 11

      ?kind?_maskr = ?kind?_right_mask( i)

   case default bits                                                 ! otherwise 00... 00

      ?kind?_maskr = 0_?kind?_k

   end select bits

return                                                               ! maskr()

!  ?kind?_maskr()

end function ?kind?_maskr

?? end text maskr
?? if( byte_k )then
?? copy :: maskr( byte)
?? end if
?? if( short_k )then
?? copy :: maskr( short)
?? end if
?? if( int_k )then
?? copy :: maskr( int)
?? end if
?? if( long_k )then
?? copy :: maskr( long)
?? end if
! **********************************************************************

!  specific functions implementing shifts as binary operators

?? text :: shift( kind)
! **********************************************************************

!  shift( i, j)

elemental integer( kind= ?kind?_k) function ?kind?_shift( i, j)

integer( kind= ?kind?_k), intent( in) :: i, j

!  ?kind?_shift()

continue                                                             ! .shift.

   ?kind?_shift = ishft( i, j)

return                                                               ! .shift.

!  ?kind?_shift()

end function ?kind?_shift

?? end text shift
?? if( byte_k )then
?? copy :: shift( byte)
?? end if
?? if( short_k )then
?? copy :: shift( short)
?? end if
?? if( int_k )then
?? copy :: shift( int)
?? end if
?? if( long_k )then
?? copy :: shift( long)
?? end if
?? text :: rotate( kind)
! **********************************************************************

!  rotate( i, j)

elemental integer( kind= ?kind?_k) function ?kind?_rotate( i, j)

integer( kind= ?kind?_k), intent( in) :: i, j

!  ?kind?_rotate()

continue                                                             ! .rotate.

   ?kind?_rotate = ishftc( i, j)

return                                                               ! .rotate.

!  ?kind?_rotate()

end function ?kind?_rotate

?? end text rotate
?? if( byte_k )then
?? copy :: rotate( byte)
?? end if
?? if( short_k )then
?? copy :: rotate( short)
?? end if
?? if( int_k )then
?? copy :: rotate( int)
?? end if
?? if( long_k )then
?? copy :: rotate( long)
?? end if
! **********************************************************************

!  double word shifts: dshftl(), dshftr(), dshftc()

?? text :: dshftl( kind)
! **********************************************************************

!  dshftl( bl, br, i)

elemental integer( kind= ?kind?_k) function ?kind?_dshftl( bl, br, i)

integer( kind= ?kind?_k), intent( in) :: bl, br
integer( kind= ?kind?_k), intent( in) :: i

!  ?kind?_dshftl() local

   integer( kind= int_k) :: btl, btr

!  ?kind?_dshftl()

continue                                                             ! dshftl()

!  trap out endcase

   if( i < 0 )then

      ?kind?_dshftl = 0_?kind?_k

      return                                                         ! dshftl()

   elseif( i == 0 )then

      ?kind?_dshftl = bl

      return                                                         ! dshftl()

   end if

   if( i < bit_size( i) )then                                        ! if shift within one word

      btl = ishft( bl, i)
      btr = ishft( br, i - bit_size( i))

      ?kind?_dshftl = ior( btl, btr)

   elseif( i == bit_size( i) )then                                   ! shift is exactly one word

      ?kind?_dshftl = br

   else                                                              ! if shift out of range

      ?kind?_dshftl = 0_?kind?_k

   end if

return                                                               ! dshftl()

!  ?kind?_dshftl()

end function ?kind?_dshftl

?? end text dshftl
?? if( byte_k )then
?? copy :: dshftl( byte)
?? end if
?? if( short_k )then
?? copy :: dshftl( short)
?? end if
?? if( int_k )then
?? copy :: dshftl( int)
?? end if
?? if( long_k )then
?? copy :: dshftl( long)
?? end if
?? text :: dshftr( kind)
! **********************************************************************

!  dshftr( bl, br, i)

elemental integer( kind= ?kind?_k) function ?kind?_dshftr( bl, br, i)

integer( kind= ?kind?_k), intent( in) :: bl, br
integer( kind= ?kind?_k), intent( in) :: i

!  ?kind?_dshftr() local

   integer( kind= ?kind?_k) :: btl, btr

!  ?kind?_dshftr()

continue                                                             ! dshftr()

!  trap out endcase

   if( i < 0 )then

      ?kind?_dshftr = 0_?kind?_k

      return                                                         ! dshftr()

   elseif( i == 0 )then

      ?kind?_dshftr = br

      return                                                         ! dshftr()

   end if

   if( i < bit_size( i) )then                                        ! if shift within one word

      btl = ishft( bl, bit_size( i) - i)
      btr = ishft( br, -i)

      ?kind?_dshftr = ior( btl, btr)

   elseif( i == bit_size( i) )then                                   ! shift is exactly one word

      ?kind?_dshftr = bl

   else                                                              ! if shift out of range

      ?kind?_dshftr = 0_?kind?_k

   end if

return                                                               ! dshftr()

!  ?kind?_dshftr()

end function ?kind?_dshftr

?? end text dshftr
?? if( byte_k )then
?? copy :: dshftr( byte)
?? end if
?? if( short_k )then
?? copy :: dshftr( short)
?? end if
?? if( int_k )then
?? copy :: dshftr( int)
?? end if
?? if( long_k )then
?? copy :: dshftr( long)
?? end if
?? text :: dshftc( kind)
! **********************************************************************

!  dshftc( bl, br, i)

elemental subroutine ?kind?_dshftc( bl, br, i)

integer( kind= ?kind?_k), intent( inout) :: bl, br
integer( kind= ?kind?_k), intent( in) :: i

!  ?kind?_dshftc() local

   integer( kind= ?kind?_k) :: btl, btr, carryl, carryr, ia

!  ?kind?_dshftc()

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

!  ?kind?_dshftc()

end subroutine ?kind?_dshftc

?? end text dshftc
?? if( byte_k )then
?? copy :: dshftc( byte)
?? end if
?? if( short_k )then
?? copy :: dshftc( short)
?? end if
?? if( int_k )then
?? copy :: dshftc( int)
?? end if
?? if( long_k )then
?? copy :: dshftc( long)
?? end if
! **********************************************************************

!  bit-wise unary operator- .not. i

?? text :: not( kind)
! **********************************************************************

!  ?kind?_not()- .not. for kind ?kind?

elemental integer( kind= ?kind?_k) function ?kind?_not( i)

integer( kind= ?kind?_k), intent( in) :: i

!  ?kind?_not()

continue                                                             ! .not. i

   ?kind?_not = not( i)

return                                                               ! .not. i

!  ?kind?_not()

end function ?kind?_not

?? end text not
?? if( byte_k )then
?? copy :: not( byte)
?? end if
?? if( short_k )then
?? copy :: not( short)
?? end if
?? if( int_k )then
?? copy :: not( int)
?? end if
?? if( long_k )then
?? copy :: not( long)
?? end if
! **********************************************************************

!  bit-wise binary operator-  i1 .and. i2

?? text :: and( kind)
! **********************************************************************

!  ?kind?_and()- .and. for kind ?kind?

elemental integer( kind= ?kind?_k) function ?kind?_and( i1, i2)

integer( kind= ?kind?_k), intent( in) :: i1, i2

!  ?kind?_and()

continue                                                             ! i .and. i

   ?kind?_and = iand( i1, i2)

return                                                               ! i .and. i

!  ?kind?_and()

end function ?kind?_and

?? end text and
?? if( byte_k )then
?? copy :: and( byte)
?? end if
?? if( short_k )then
?? copy :: and( short)
?? end if
?? if( int_k )then
?? copy :: and( int)
?? end if
?? if( long_k )then
?? copy :: and( long)
?? end if
! **********************************************************************

!  bit-wise binary operator-  i1 .or. i2

?? text :: or( kind)
! **********************************************************************

!  ?kind?_or()- .or. for kind ?kind?

elemental integer( kind= ?kind?_k) function ?kind?_or( i1, i2)

integer( kind= ?kind?_k), intent( in) :: i1, i2

!  ?kind?_or()

continue                                                             ! i .or. i

   ?kind?_or = ior( i1, i2)

return                                                               ! i .or. i

!  ?kind?_or()

end function ?kind?_or

?? end text or
?? if( byte_k )then
?? copy :: or( byte)
?? end if
?? if( short_k )then
?? copy :: or( short)
?? end if
?? if( int_k )then
?? copy :: or( int)
?? end if
?? if( long_k )then
?? copy :: or( long)
?? end if
! **********************************************************************

!  bit-wise binary operator-  i1 .eor. i2

?? text :: eor( kind)
! **********************************************************************

!  ?kind?_eor()- .eor. for kind ?kind?

elemental integer( kind= ?kind?_k) function ?kind?_eor( i1, i2)

integer( kind= ?kind?_k), intent( in) :: i1, i2

!  ?kind?_eor()

continue                                                             ! i .eor. i

   ?kind?_eor = ieor( i1, i2)

return                                                               ! i .eor. i

!  ?kind?_eor()

end function ?kind?_eor

?? end text eor
?? if( byte_k )then
?? copy :: eor( byte)
?? end if
?? if( short_k )then
?? copy :: eor( short)
?? end if
?? if( int_k )then
?? copy :: eor( int)
?? end if
?? if( long_k )then
?? copy :: eor( long)
?? end if
! **********************************************************************

!  bit-wise binary operator-  i1 .eqv. i2

?? text :: eqv( kind)
! **********************************************************************

!  ?kind?_eqv()- .eqv. for kind ?kind?

elemental integer( kind= ?kind?_k) function ?kind?_eqv( b1, b2)

integer( kind= ?kind?_k), intent( in) :: b1, b2

!  ?kind?_eqv()

continue                                                             ! i .eqv. i

   ?kind?_eqv = not( ieor( b1, b2) )

return                                                               ! i .eqv. i

!  ?kind?_eqv()

end function ?kind?_eqv

?? end text eqv
?? if( byte_k )then
?? copy :: eqv( byte)
?? end if
?? if( short_k )then
?? copy :: eqv( short)
?? end if
?? if( int_k )then
?? copy :: eqv( int)
?? end if
?? if( long_k )then
?? copy :: eqv( long)
?? end if
! **********************************************************************

!  bit-wise binary operator-  i1 .neqv. i2

?? text :: neqv( kind)
! **********************************************************************

!  ?kind?_neqv()- .neqv. for kind ?kind?

elemental integer( kind= ?kind?_k) function ?kind?_neqv( b1, b2)

integer( kind= ?kind?_k), intent( in) :: b1, b2

!  ?kind?_neqv()

continue                                                             ! i .neqv. i

   ?kind?_neqv = ieor( b1, b2)

return                                                               ! i .neqv. i

!  ?kind?_neqv()

end function ?kind?_neqv

?? end text neqv
?? if( byte_k )then
?? copy :: neqv( byte)
?? end if
?? if( short_k )then
?? copy :: neqv( short)
?? end if
?? if( int_k )then
?? copy :: neqv( int)
?? end if
?? if( long_k )then
?? copy :: neqv( long)
?? end if
! **********************************************************************

!  bit-wise binary operator:  i1 .xor. i2

?? text :: xor( kind)
! **********************************************************************

!  ?kind?_xor(): xor() for kind ?kind?

elemental integer( kind= ?kind?_k) function ?kind?_xor( i1, i2)

integer( kind= ?kind?_k), intent( in) :: i1, i2

!  ?kind?_xor()

continue                                                             ! i .xor. i

   ?kind?_xor = ieor( i1, i2)

return                                                               ! i .xor. i

!  ?kind?_xor()

end function ?kind?_xor

?? end text xor
?? if( byte_k )then
?? copy :: xor( byte)
?? end if
?? if( short_k )then
?? copy :: xor( short)
?? end if
?? if( int_k )then
?? copy :: xor( int)
?? end if
?? if( long_k )then
?? copy :: xor( long)
?? end if
! **********************************************************************

!  bit_functions

! **********************************************************************
! $Id: bitfunc.fpp 1.3 2003/10/03 19:28:00Z Dan Release $

end module bit_functions                                             ! eof

