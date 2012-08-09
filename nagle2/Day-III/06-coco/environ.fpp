! bof
! **********************************************************************
! Fortran 95 module environment

! **********************************************************************
! Source Control Strings

! $Id: environ.fpp 1.4 2003/10/03 19:35:55Z Dan Release $

! **********************************************************************
!  copyright 2003 Purple Sage Computing Solutions, Inc.

! **********************************************************************
! some unix f77 subprograms

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
! use standard_types

! **********************************************************************

! environment constants

!   unknown_option= character returned by getopt meaning 'unknown option'
!   end_of_options= integer getopt return value meaning 'no more words'

! environment variables

!   optarg= character getopt returns option string
!   optind= integer pointing to next word for getopt to process

! environment library

!   abort( code)

!   assert( expression, string)

!   getopt( optstring) sets optarg, optind

!   lshift() left and
!   rshift() right shifts

!   cut( string, sep, nsubs, substrings, subslengths, stat)

!   swab() copy words swapping even and odd bytes

?? ! *******************************************************************

?? ! preprocessor definitions

?? include 'coco.inc'

?? ! *******************************************************************

! **********************************************************************

! environment

! **********************************************************************

module environment

! **********************************************************************

! use standard processor description

use standard_types

! **********************************************************************

! explicit declarations

implicit none                                                        ! no implicit typing

! **********************************************************************

! explicit export

private                                                              ! no implicit exporting

! **********************************************************************

!  RCS strings

! **********************************************************************

character( len= *), public, parameter :: environment_rcs_id = &
      '$Id: environ.fpp 1.4 2003/10/03 19:35:55Z Dan Release $'

! **********************************************************************

!  communication with getopt()

! ----------------------------------------------------------------------

!  getopt() 'no more arguments'

integer, public, parameter :: end_of_args = -1

!  getopt() 'not in optltrs'

character( len= 1), public, parameter :: unknown_option = '?'

!  getopt() out string

integer, public, parameter :: optarg_len = 1024

character( len= optarg_len), public, save :: optarg = unknown_option

!  getopt() out index

integer, public, save :: optind = 0

! **********************************************************************

!  library

! **********************************************************************

!  unix f77 shift routines

public :: lshift                                                     ! use generic

interface lshift                                                     ! generic name
?? if( byte_k )then
   module procedure byte_lshift
?? endif
?? if( short_k )then
   module procedure short_lshift
?? endif
?? if( int_k )then
   module procedure int_lshift
?? endif
?? if( long_k )then
   module procedure long_lshift
?? endif
end interface

public :: rshift                                                     ! use generic

interface rshift                                                     ! generic name
?? if( byte_k )then
   module procedure byte_rshift
?? endif
?? if( short_k )then
   module procedure short_rshift
?? endif
?? if( int_k )then
   module procedure int_rshift
?? endif
?? if( long_k )then
   module procedure long_rshift
?? endif
end interface

! **********************************************************************

!  module procedures

public :: abort
public :: assert

public :: getopt

public :: cut
public :: swab

! **********************************************************************

!  library

contains                                                             ! environ

! **********************************************************************

!  public abort: terminate program with nonzero exit status

subroutine abort( code)

integer, intent( in), optional :: code

!  local

   integer :: icode

!  abort() text

continue                                                             ! abort()

   default_code: if( present( code) )then                            ! if called with code

      icode = code

   else default_code                                                 ! else use default

      icode = 1

   endif default_code

   write( unit= error_unit, fmt= *) ' abort: ', icode                ! complain

stop 'abort'                                                         ! quit status code or 1

!  abort()

end subroutine abort

! **********************************************************************

!  public assert: verify assertion

subroutine assert( expression, string)

logical, intent( in) :: expression

character( len= *), intent( in) :: string

!  assert() text

continue                                                             ! assert()

   assertion_fails: if( .not. expression )then                       ! expression false

      write( unit= error_unit, fmt= *) ' assertion failed: ' // string

      stop 'assertion failed'                                        ! punt

   endif assertion_fails                                             ! expression false

return                                                               ! assert()

!  assert()

end subroutine assert

! **********************************************************************

!  public getopt: return next known option from command line or unknown

integer function getopt( optstring)

character( len= *), intent( in) :: optstring

!  getopt() local

   character( len= optarg_len) :: optword
   character( len= 1) :: firstchar, secndchar, thirdchar

!  character constants

   character( len= 1), parameter :: dash = '-'
   character( len= 1), parameter :: colon = ':'
   character( len= 1), parameter :: blank = ' '

!  integers

   integer :: iopt

!  external symbols

   integer, external :: iargc

   external :: getarg

! **********************************************************************

!  getopt() text

continue                                                             ! getopt()

!  initialize for next option

   optarg = blank                                                    ! reset

   check_inc: if( optind >= iargc() )then                            ! if no unread options

      optarg = unknown_option                                        ! complain
      getopt = end_of_args                                           ! and

      return                                                         ! quit

   endif check_inc                                                   ! if no unread options

!  get next option

   optind = optind + 1                                               ! next arg to get

   call getarg( optind, optword)                                     ! get next arg word

   firstchar = optword( 1: 1)                                        ! examine optword
   secndchar = optword( 2: 2)                                        ! character by
   thirdchar = optword( 3: 3)                                        ! character

!  if word is not -x

   not_an_option: if( firstchar /= dash )then                        ! if word is not an option

      optarg = optword                                               ! return word
      getopt = ichar( unknown_option)                                ! mark unrecognized

      return                                                         ! quit

!  if word is --

   elseif( secndchar == dash )then not_an_option                     ! if --

      optarg = optword                                               ! return --
      getopt = end_of_args                                           ! signal end of options

      return                                                         ! quit

   endif not_an_option

!  optword is -x (not --)

   iopt = index( optstring, secndchar)                               ! find optletter in string

   is_opt: if( iopt > substring_not_found )then                      ! if found in optstring

!  if this optltr must have another word

      opt_string: if( optstring( iopt+1: iopt+1) == colon )then

!  it can be separated by a blank

         next_word: if( thirdchar == blank )then                     ! in which case

            optind = optind + 1                                      ! increment index
            call getarg( optind, optarg)                             ! get next word

!  or not be separated by a blank

         else next_word

            optarg = optword( 3: )                                   ! option field is rest of word

         endif next_word

      endif opt_string

      getopt = ichar( secndchar)                                     ! option found

!  if this optltr must not have another word

   else is_opt                                                       ! if not found in optstring

      optarg = optword                                               ! return word and
      getopt = ichar( unknown_option)                                ! complain

   endif is_opt

return                                                               ! getopt()

!  getopt()

end function getopt

! **********************************************************************

!  lshift()/rshift()

?? text :: lshift( kind)
! **********************************************************************

!  ?kind_lshift(): lshift for kind ?kind?

elemental integer function ?kind?_lshift( i, j)

integer( kind= ?kind?_k), intent( in) :: i, j

!  ?kind?_lshift() text

continue                                                             ! lshift()

   ?kind?_lshift = ishft( i, abs( j))

return                                                               ! lshift()

!  ?kind?_lshift()

end function ?kind?_lshift

?? end text lshift
?? if( byte_k )then
?? copy :: lshift( byte)
?? endif
?? if( short_k )then
?? copy :: lshift( short)
?? endif
?? if( int_k )then
?? copy :: lshift( int)
?? endif
?? if( long_k )then
?? copy :: lshift( long)
?? endif
?? text :: rshift( kind)
! **********************************************************************

!  ?kind?_rshift(): rshift for kind ?kind?

elemental integer function ?kind?_rshift( i, j)

integer( kind= ?kind?_k), intent( in) :: i, j

!  ?kind?_rshift() text

continue                                                             ! rshift()

   ?kind?_rshift = ishft( i, -abs( j))

return                                                               ! rshift()

!  ?kind?_rshift()

end function ?kind?_rshift

?? end text rshift
?? if( byte_k )then
?? copy :: rshift( byte)
?? endif
?? if( short_k )then
?? copy :: rshift( short)
?? endif
?? if( int_k )then
?? copy :: rshift( int)
?? endif
?? if( long_k )then
?? copy :: rshift( long)
?? endif
! **********************************************************************

!  cut(): return input string broken at separators in output array

pure subroutine cut( string, sep, substrings, substring_len, number)

character( len= *), intent( in) :: string                            ! input string
character( len= 1), intent( in) :: sep                               ! separator

character( len= *), dimension( :), optional, intent( out) :: substrings
integer, dimension( *), optional, intent( out) :: substring_len

integer, optional, intent( out) :: number

!  cut() local

   integer :: index_sep                                              ! index of next separator
   integer :: next_substring                                         ! start of next substring
   integer :: number_substring                                       ! number of substring found

!  cut() text

continue                                                             ! cut()

!  initialize

   next_substring = 1                                                ! start of next substring
   number_substring = 0                                              ! number found so far

   index_sep = index( string( next_substring: ), sep)                ! index of first separator

!  loop while more separators in string

   more_subs: do while( index_sep > substring_not_found)             ! while more separators

      number_substring = number_substring + 1                        ! one more substring

      set_substring: if( present( substrings) )then                  ! next substring

         substrings( number_substring) = string( next_substring: next_substring + index_sep - 2)

      endif set_substring                                            ! next substring

      set_len: if( present( substring_len) )then                     ! next length

         substring_len( number_substring) = index_sep - 1

      endif set_len                                                  ! next length

      next_substring = next_substring + index_sep                    ! start of next substring

      index_sep = index( string( next_substring: ), sep)             ! index of next separator

   enddo more_subs                                                   ! while more separators

!  last substring

   number_substring = number_substring + 1                           ! one more substring

   last_substring: if( present( substrings) )then                    ! rest of string is last substring

      substrings( number_substring) = string( next_substring: )

   endif last_substring                                              ! rest of string is last substring

   len_arg: if( present( substring_len) )then

       substring_len( number_substring) = len_trim( string( next_substring: ))

    endif len_arg

!  return number of substring of requested

   number_arg: if( present( number) )then

      number = number_substring                                      ! count if requested

   endif number_arg

return                                                               ! cut()

!  cut()

end subroutine cut

! **********************************************************************

!  swab(): return input string broken at separators in output array

pure subroutine swab( a, b)

integer( kind= int_k), dimension( :), intent( in) :: a

integer( kind= int_k), dimension( :), intent( out) :: b

!  swab() local

   integer( kind= byte_k), dimension( csu_per_nsu) :: temp

   integer( kind= int_k) :: iword

!  swab() text

continue                                                             ! swab()

   copy: do iword = 1, min( size(a), size( b) )                      ! copy words

      temp = transfer( a( iword), temp)                              ! to byte array

      temp( 1: 2) = temp( 2: 1: -1)                                  ! swap low bytes
      temp( 3: 4) = temp( 4: 3: -1)                                  ! swap hi bytes

      b( iword) = transfer( temp, b( iword) )                        ! from byte array

   enddo copy                                                        ! copy words

return                                                               ! swab()

!  swab()

end subroutine swab

! **********************************************************************

!  environment

! $Id: environ.fpp 1.4 2003/10/03 19:35:55Z Dan Release $
! **********************************************************************

end module environment                                               ! eof
