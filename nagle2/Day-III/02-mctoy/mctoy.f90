! bof
! **********************************************************************
! Fortran 95 program mctoy

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
! mctoy describe the program

! ----------------------------------------------------------------------

!  mctoy uses

!     processor_dependencies- describes the processor

!  mctoy includes

!     <none>

!  mctoy reads files

!  mctoy writes files

!  mctoy constants

!  mctoy types

!  mctoy data

!  mctoy library

! **********************************************************************

!  mctoy

! ----------------------------------------------------------------------

program mctoy

! ----------------------------------------------------------------------

!  mctoy uses modules

! ----------------------------------------------------------------------

!  processor description

use, intrinsic :: iso_fortran_env, only: input_unit, output_unit, error_unit

! ----------------------------------------------------------------------

!  all names are declared: require explicit declarations

implicit none

! ----------------------------------------------------------------------

!  mctoy RCS strings

! ----------------------------------------------------------------------

!  program source filename supplied by RCS

character( len= *), parameter :: mctoy_rcs_id = &
   '$Id$'

! ----------------------------------------------------------------------

!  mctoy constants

! ----------------------------------------------------------------------

real, parameter :: one = 1.0

! ----------------------------------------------------------------------

!  mctoy types

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------

!  mctoy data

! ----------------------------------------------------------------------

   integer :: i

   integer, codimension[ *] :: num_trials

   integer :: quadrant

   real :: sign_x, sign_y

   real :: x, y

   real, codimension[ *] :: total

! ----------------------------------------------------------------------

!  mctoy text

! ----------------------------------------------------------------------

continue

   if( num_images() /= 4 )then

      write( unit= error_unit, fmt= *) 'mctoy must be run with four images'
      stop 'wrong number of images for mctoy'

   end if

   if( this_image() == 1 )then

      write( unit= output_unit, fmt= *) 'number of trials (per image)'
      read( unit= input_unit, fmt= *) num_trials

   end if

   do i = 2, num_images()

      num_trials[ i] = num_trials

   end do

   sync all

   quadrant = mod( this_image(), 4)

   if( quadrant == 0 )then

      sign_x = one
      sign_y = one

   else if( quadrant == 1 )then

      sign_x = -one
      sign_y = one

   else if( quadrant == 2 )then

      sign_x = -one
      sign_y = -one

   else if( quadrant == 3 )then

      sign_x = one
      sign_y = -one

   end if

   total = 0

   do i = 1, num_trials

      call random_number( harvest= x)
      call random_number( harvest= y)

      x = sign( x, sign_x)
      y = sign( y, sign_y)

      if( x*x + y*y < one ) total = total + 1

   end do

   sync all

   do i = 2, num_images()

      total = total + total[ i]

   end do

   if( this_image() == 1 )then

      write( unit= output_unit, fmt= *) 'buffon says pi = ', real( total) / real( num_trials)

   end if

stop 'normal exit in mctoy'

! ----------------------------------------------------------------------

!  mctoy library

! ----------------------------------------------------------------------

contains

! ----------------------------------------------------------------------

!  mctoy

! $Id$
! **********************************************************************
! eof
end program mctoy

