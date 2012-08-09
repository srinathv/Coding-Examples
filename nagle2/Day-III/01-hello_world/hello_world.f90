! bof
! **********************************************************************
! Fortran 95 program hello_world

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
! hello_world describe the program

! ----------------------------------------------------------------------

!  hello_world uses

!     processor_dependencies- describes the processor

!  hello_world includes

!     <none>

!  hello_world reads files

!  hello_world writes files

!  hello_world constants

!  hello_world types

!  hello_world data

!  hello_world library

! **********************************************************************

!  hello_world

! ----------------------------------------------------------------------

program hello_world

! ----------------------------------------------------------------------

!  hello_world uses modules

! ----------------------------------------------------------------------

!  processor description

use, intrinsic :: iso_fortran_env, only: output_unit

! ----------------------------------------------------------------------

!  all names are declared: require explicit declarations

implicit none

! ----------------------------------------------------------------------

!  hello_world RCS strings

! ----------------------------------------------------------------------

!  program source filename supplied by RCS

character( len= *), parameter :: hello_world_rcs_id = &
   '$Id$'

! ----------------------------------------------------------------------

!  hello_world constants

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------

!  hello_world types

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------

!  hello_world data

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------

!  hello_world text

! ----------------------------------------------------------------------

continue

   write( unit= output_unit, fmt= *) 'Image ', this_image(), ' of ', num_images(), ' says "Hello, world!"'

stop 'normal exit in hello_world'

! ----------------------------------------------------------------------

!  hello_world library

! ----------------------------------------------------------------------

contains

! ----------------------------------------------------------------------

!  hello_world

! $Id$
! **********************************************************************
! eof
end program hello_world

