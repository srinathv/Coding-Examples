! bof
! **********************************************************************
! Fortran 95 program mxv

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
! mxv describe the program

! ----------------------------------------------------------------------

!  mxv uses

!     processor_dependencies- describes the processor

!  mxv includes

!     <none>

!  mxv reads files

!  mxv writes files

!  mxv constants

!  mxv types

!  mxv data

!  mxv library

! **********************************************************************

!  mxv

! ----------------------------------------------------------------------

program mxv

! ----------------------------------------------------------------------

!  mxv uses modules

! ----------------------------------------------------------------------

!  processor description

use, intrinsic :: iso_fortran_env, only: input_unit

! ----------------------------------------------------------------------

!  all names are declared: require explicit declarations

implicit none

! ----------------------------------------------------------------------

!  mxv RCS strings

! ----------------------------------------------------------------------

!  program source filename supplied by RCS

character( len= *), parameter :: mxv_rcs_id = &
   '$Id$'

! ----------------------------------------------------------------------

!  mxv constants

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------

!  mxv types

! ----------------------------------------------------------------------

!  1-d segment_t

type :: segment_t

   integer :: lo
   integer :: hi

end type segment_t

! ----------------------------------------------------------------------

!  mxv data

! ----------------------------------------------------------------------

integer, codimension[ *] :: mat_size

real, dimension( :, :), codimension[ :], allocatable :: rows

real, dimension( :), codimension[ :], allocatable :: vector, prod

integer :: i

type( segment_t), dimension( :), allocatable :: batch

! ----------------------------------------------------------------------

!  mxv text

! ----------------------------------------------------------------------

continue

   if( this_image() == 1 )then

      read( unit= input_unit, fmt= *) mat_size

      do i = 2, num_images()

         mat_size[ i] = mat_size

      end do

   end if

   sync all

   allocate( vector( mat_size)[ *], prod( mat_size)[ *] )

   if( this_image() == 1 )then

      read( unit= input_unit, fmt= *) vector

   end if

   sync all

   if( this_image() == 1 )then

      do i = 2, num_images()

         vector[ i] = vector

      end do

   end if

   sync all

   allocate( batch( num_images()) )

   call segment( 1, mat_size, num_images(), batch)

   allocate( rows( mat_size, batch( 1)% lo: batch( 1)% hi)[ *] )

   if( this_image() == 1 )then

      do i = 1, mat_size

         read( unit= input_unit, fmt= *) rows( :, batch( i)% lo: batch( i)% hi)[ i]

      end do

   end if

   sync all

   do i = batch( this_image())% lo, batch( this_image())% hi

      prod( i) = dot_product( vector, rows( :, i))

   end do

   sync all

   if( this_image() == 1 )then

      do i = 1, this_image()

         write( unit= output_unit, fmt= *) rows[ i]

      end do

   end if

stop 'normal exit in mxv'

! ----------------------------------------------------------------------

!  mxv library

! ----------------------------------------------------------------------

contains

! ----------------------------------------------------------------------

!  segment() partition a (loop) range as evenly as possible

subroutine segment( low, high, nproc, subseg)

!  input ( low, high), to be divided nproc ways
!  output subseg=( ibgn, iend), ( 0, 0) ==> error

integer, intent( in) :: low, high, nproc

type( segment_t), intent( out), dimension( nproc) :: subseg

!  local data

   integer :: quot, rem, cnt

!  segment()

continue

!  get size of segment

   cnt = high - low + 1

!     detect nonsense

   if( low >= high .or. nproc < 1 )then

      subseg( 1) = segment_t( 0, 0)
      return

!     detect no-op

   elseif( nproc == 1 )then

      subseg( 1) = segment_t( low, high)
      return

!     detect 1-1

   elseif( nproc == cnt )then

      subseg = (/ ( segment_t( i, i), i = low, high) /)
      return

   endif

!  get remainder and quotient

   rem = mod( cnt, nproc)
   quot = cnt / nproc

!  if nonzero remainder, first rem segments will be one greater

   if( rem /= 0 ) quot = quot + 1

!  first range

   subseg( 1) = segment_t( low, low + quot - 1)

!  loop thru segments (if needed)

   do i = 2, nproc-1

!  set first

      subseg( i)% lo = subseg( i - 1)% lo + quot
      rem = rem - 1

!  after first rem segments, reset for rest of segments

      if( rem == 0 ) quot = quot - 1

!  set last

      subseg( i)% hi = subseg( i - 1)% hi + quot

   enddo

!  set up last segment

   subseg( nproc) = segment_t( subseg( nproc - 1)%  hi + 1, high)

!  having partitioned the original segment nproc ways as fairly as possible

return

!  segment()

end subroutine segment

! ----------------------------------------------------------------------

!  mxv

! $Id$
! **********************************************************************
! eof
end program mxv

