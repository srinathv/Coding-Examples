! bof
! **********************************************************************
! Fortran 95 program aka

! **********************************************************************
! Source Control Strings

! $Source: D:/LF95/Utilities/rcs/Aka.f90 $
! $Revision: 1.2 $
! $State: Release $
! $Date: 1999/04/16 17:22:07 $

! **********************************************************************
!  Copyright 1999 Purple Sage Computing Solutions, Inc.

!   This program is free software; you can redistribute it and/or
!   modify it under the terms of the GNU General Public
!   License as published by the Free Software Foundation; either
!   version 2 of the License, or (at your option) any later version.

!   This library is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!   Library General Public License for more details.

!   You should have received a copy of the GNU General Public
!   License along with this library; if not, write to the Free
!   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

! To report bugs, suggest enhancements, etc. to the Authors,
! Contact:
!    Purple Sage Computing Solutions, Inc.
!                               send email to dnagle@erols.com
!                                   or fax to 703 471 0684 (USA)
!                                  or mail to 12142 Purple Sage Ct.
!                                             Reston, VA 20194-5621 USA

!  a user runs aka to see what a value expressed as a 4 byte type
!  looks like when the underlying bits are expressed as some other type.
!  this is a toy to demonstrate one possible use of type boolean,
!  in this case, as a hole in the type system.
!  aka is loosely modeled on the unix units program.

!  run aka as follows:
!  after the banners, select the 'I have' type: real, integer, logical or
!  character( len= 4).  then select the 'I want' type: same menu.
!  then enter the value, expressed as the 'I have' type.
!  aka prints the same bits expressed as the 'I want' type.

!  aka agrees if the 'I have' and the 'I want' type are the same,
!  and asks for the 'I have' type again, then the 'I want' type.

!  aka quits upon garbled input.

! **********************************************************************
! aka type conversion utility

! **********************************************************************

!  aka reads

!     stdin

!  aka writes

!     stdout

!  aka uses

!     standard_types

!     type_boolean

!  aka constants

!     <none>

!  aka types

!     <none>

!  aka data

!  aka library

!     <none>

! **********************************************************************

!  aka

! **********************************************************************

program aka

! **********************************************************************

!  aka uses programs

! **********************************************************************

use, intrinsic :: iso_fortran_env, only: numeric_storage_size, character_storage_size

use :: type_boolean

! **********************************************************************

!  aka RCS strings

! **********************************************************************

!  program source filename supplied by RCS

character( len= *), parameter :: &
      aka_rcs_source = '$Source: D:/LF95/Utilities/rcs/Aka.f90 $'

!  program revision supplied by RCS

character( len= *), parameter :: &
      aka_rcs_revision = '$Revision: 1.2 $'

!  program revision state supplied by RCS

character( len= *), parameter :: &
      aka_rcs_state = '$State: Release $'

!  program revision date supplied by RCS

character( len= *), parameter :: &
      aka_rcs_date = '$Date: 1999/04/16 17:22:07 $'

!  a Fortran 2003 version from iso_fortran_env

integer, parameter :: csu_per_nsu = numeric_storage_size / character_storage_size

! **********************************************************************

!  aka data

! **********************************************************************

!  buffers

   type( boolean_t) :: buffer

   real :: rbuff
   integer :: ibuff
   logical :: lbuff
   character( len= csu_per_nsu) :: cbuff

!  source and target formats

   character( len= 1) :: ihave, iwant

!  user quits

   character( len= 1), parameter :: iquit = 'q'

!  type codes

   character( len= 1), parameter :: isreal = 'r'
   character( len= 1), parameter :: isint = 'i'
   character( len= 1), parameter :: islog = 'l'
   character( len= 1), parameter :: ischar = 'c'
   character( len= 1), parameter :: isbits = 'b'

!  formats

   character( len= 12), parameter :: banner = '( 1x, a, a)'
   character( len= 8), parameter :: inbits = '( z8)'
   character( len= 12), parameter :: outbits = '( 1x, z8.8)'

   character( len= 8) :: bbuff    ! internal file

! **********************************************************************

!  aka text

! **********************************************************************

continue

! **********************************************************************

!  aka rcs banner

   write( *, *) aka_rcs_source
   write( *, *) aka_rcs_revision
   write( *, *) aka_rcs_state
   write( *, *) aka_rcs_date
   write( *, *)

! **********************************************************************

!  aka banner

   write( *, banner) isreal, ' - real'
   write( *, banner) isint, ' - integer'
   write( *, banner) islog, ' - logical'
   write( *, banner) ischar, ' - character'
   write( *, banner) isbits, ' - bits'

! **********************************************************************

!  top of main input loop

 1 continue

! ----------------------------------------------------------------------

!  solicit current interpretation

   write( *, *)
   write( *, *) 'you have: '
   read( *, *) ihave
   if( ihave .eq. iquit ) goto 3

! ----------------------------------------------------------------------

!  check for recognition

   select case( ihave)
      case( isreal)
         continue
      case( isint)
         continue
      case( islog)
         continue
      case( ischar)
         continue
      case( isbits)
         continue
      case default
         goto 2
   end select

! ----------------------------------------------------------------------

!  solicit desired interpretation

   write( *, *) 'you want: '
   read( *, *) iwant
   if( iwant .eq. iquit ) goto 3

! ----------------------------------------------------------------------

!  check for recognition

   select case( iwant)
      case( isreal)
         continue
      case( isint)
         continue
      case( islog)
         continue
      case( ischar)
         continue
      case( isbits)
         continue
      case default
         goto 2
   end select

! ----------------------------------------------------------------------

!  no work if have == want

   no_change: if( ihave == iwant )then
      write( *, *) 'yes'
      goto 1
   endif no_change

! ----------------------------------------------------------------------

!  read current interpretation

   select case( ihave)
      case( isreal)
         read( *, *) rbuff
         buffer = rbuff
      case( isint)
         read( *, *) ibuff
         buffer = ibuff
      case( islog)
         read( *, *) lbuff
         buffer = lbuff
      case( ischar)
         read( *, *) cbuff
         buffer = cbuff
      case( isbits)
         read( *, *) bbuff
         read( bbuff, inbits) ibuff
         buffer = ibuff
   end select

! ----------------------------------------------------------------------

!  print desired interpretation

   select case( iwant)
      case( isreal)
         rbuff = buffer
         write( *, *) rbuff
      case( isint)
         ibuff = buffer
         write( *, *) ibuff
      case( islog)
         lbuff = buffer
         write( *, *) lbuff
      case( ischar)
         cbuff = buffer
         write( *, *) cbuff
      case( isbits)
         ibuff = buffer
         write( *, outbits) ibuff
   end select

! ----------------------------------------------------------------------

!  do it again

   goto 1

! **********************************************************************

!  here from punt

 2 continue
   write( *, *) 'garbled input- aka quits'

! **********************************************************************

!  here to quit

 3 continue

stop 'normal exit in aka'

! **********************************************************************

!  aka

! $Source: D:/LF95/Utilities/rcs/Aka.f90 $
! **********************************************************************

end program
