! bof
! **********************************************************************
! Fortran 95 program test_f_df

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
! test_f_df describe the program

! ----------------------------------------------------------------------

!  test_f_df uses

!     processor_dependencies- describes the processor

!  test_f_df includes

!     <none>

!  test_f_df reads files

!  test_f_df writes files

!  test_f_df constants

!  test_f_df types

!  test_f_df data

!  test_f_df library

! **********************************************************************

!  test_f_df

! ----------------------------------------------------------------------

program test_f_df

! ----------------------------------------------------------------------

!  test_f_df uses modules

! ----------------------------------------------------------------------

!  processor description

use, intrinsic :: iso_fortran_env, only: real64

!  derivative type

use :: f_df, only: f_df_t, sin, cos, operator( *), operator( -)

! ----------------------------------------------------------------------

!  all names are declared: require explicit declarations

implicit none

! ----------------------------------------------------------------------

!  test_f_df RCS strings

! ----------------------------------------------------------------------

!  program source filename supplied by RCS

character( len= *), parameter :: test_f_df_rcs_id = &
   '$Id$'

! ----------------------------------------------------------------------

!  test_f_df constants

! ----------------------------------------------------------------------

integer, parameter :: max_iter = 30

real( real64), parameter :: tolerance = 1.0e-10_real64

real( real64), parameter :: runaway = 1.0e10_real64

! ----------------------------------------------------------------------

!  test_f_df types

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------

!  test_f_df data

! ----------------------------------------------------------------------

type( f_df_t) :: x, x_new

type( f_df_t) :: fx

integer :: i

! ----------------------------------------------------------------------

!  test_f_df text

! ----------------------------------------------------------------------

continue

   x = f_df_t( 1.5_real64, 1.0_real64)

   do i = 1, max_iter

      fx = f( x)

      x_new = x - fx% f / fx% df

      if( abs( x_new% f - x% f) < tolerance ) exit

      if( abs( x_new% f) > runaway ) exit

      x = x_new

   end do

   write( unit= *, fmt= *) 'what the cat dragged in', i, x_new

stop 'normal exit in test_f_df'

! ----------------------------------------------------------------------

!  test_f_df library

! ----------------------------------------------------------------------

contains

! ----------------------------------------------------------------------

function f( x) result( fx)

type( f_df_t), intent( in) :: x

type( f_df_t) :: fx

continue

   fx = x * cos( x)

return

end function f

! ----------------------------------------------------------------------

!  test_f_df

! $Id$
! **********************************************************************
! eof
end program test_f_df

