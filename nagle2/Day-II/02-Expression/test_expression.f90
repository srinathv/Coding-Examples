! bof
! **********************************************************************
! Fortran 95 program test_expression

! ----------------------------------------------------------------------
! Source Control Strings

! $Id$

! ----------------------------------------------------------------------
!  Copyright 2012 Dan Nagle
!  All Rights Reserved

!   This program is free software; you can redistribute it and/or
!   modify it under the terms of the GNU General Public
!   License as published by the Free Software Foundation; either
!   version 2 of the License, or (at your option) any later version.

!   This program is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!   General Public License for more details.

!   You should have received a copy of the GNU General Public
!   License along with this program; if not, write to the Free
!   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

! To report bugs, suggest enhancements, or contact the Authors,
! Contact:
!    Dan Nagle
!                               send email to dannagle@verizon.net
!                                  or mail to 2820 Lafayette Dr.
!                                             Boulder, CO 80305 USA

! ----------------------------------------------------------------------
! test_expression describe the program

! ----------------------------------------------------------------------

!  test_expression uses

!     processor_dependencies- describes the processor

!  test_expression includes

!     <none>

!  test_expression reads files

!  test_expression writes files

!  test_expression constants

!  test_expression types

!  test_expression data

!  test_expression library

! **********************************************************************

!  test_expression

! ----------------------------------------------------------------------

program test_expression

! ----------------------------------------------------------------------

!  test_expression uses modules

! ----------------------------------------------------------------------

!  processor description

use, intrinsic :: iso_fortran_env, only: real64

!  derivative type

use :: expression

! ----------------------------------------------------------------------

!  all names are declared: require explicit declarations

implicit none

! ----------------------------------------------------------------------

!  test_expression RCS strings

! ----------------------------------------------------------------------

!  program source filename supplied by RCS

character( len= *), parameter :: test_expression_rcs_id = &
   '$Id$'

! ----------------------------------------------------------------------

!  test_expression constants

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------

!  test_expression types

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------

!  test_expression data

! ----------------------------------------------------------------------

type( integer_expression_t), target :: x, y

type( integer_expression_t), pointer :: r
type( integer_expression_t), pointer :: rsub

type( real_expression_t), target :: rx, ry

type( real_expression_t), pointer :: rr


! ----------------------------------------------------------------------

!  test_expression text

! ----------------------------------------------------------------------

continue

   x = integer_expression_t( 0, null(), null(), 0)
   y = integer_expression_t( 0, null(), null(), 0)

   r => x + y
   rsub => x - y

   call evaluate( r)

   write( unit= *, fmt= *) 'what the cat dragged in', r% value

   x% value = 42
   y% value = 99

   call evaluate( r)
   call evaluate( rsub)

   write( unit= *, fmt= *) 'what the cat dragged in', r% value
   write( unit= *, fmt= *) 'what the cat coughed out', rsub% value

   rx = real_expression_t( 0, null(), null(), 0.)
   ry = real_expression_t( 0, null(), null(), 0.)

   rr => rx + ry

   call evaluate( rr)

   write( unit= *, fmt= *) 'what the cat dragged in', rr% value

   rx% value = 42.
   ry% value = 99.

   call evaluate( rr)

   write( unit= *, fmt= *) 'what the cat dragged in', rr% value



stop 'normal exit in test_expression'

! ----------------------------------------------------------------------

!  test_expression library

! ----------------------------------------------------------------------

contains

! ----------------------------------------------------------------------

!  test_expression

! $Id$
! **********************************************************************
! eof
end program test_expression

