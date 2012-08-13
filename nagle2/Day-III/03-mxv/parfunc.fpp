! bof
! **********************************************************************
! Fortran 95 module parallel_functions

! **********************************************************************
! Source Control Strings

! $Source$
! $Revision$
! $State$
! $Date$

! **********************************************************************
! Copyright 2000 Purple Sage Computing Solutions, Inc.

! **********************************************************************
! routines useful with vector and parallel programs

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
!                               send email to dnagle@@erols.com
!                                   or fax to 703 471 0684 (USA)
!                                  or mail to 12142 Purple Sage Ct.
!                                             Reston, VA 20194-5621 USA

! **********************************************************************

!  parallel_functions routines useful in parallel computing

! **********************************************************************

!  parallel_functions types

!     type segment_t integers( [ lo, hi])
!     type tile_t segment_t x segment_t
!     type block_t tile_t x segment_t

!     type accumulator_t sum-by-magnitude accumulator

!  parallel_functions library

!     segment() divide a contiguous 1d range as evenly as possible
!     tile() divide a contiguous 2d range as evenly as possible
!     block() divide a contiguous 3d range as evenly as possible

!     smag_zero() zeros accumulator
!     smag_add() add to accumulator sorted by magnitude for reals
!     smag_sum() sum of accumulator sorted by magnitude for reals

!     permute() permute an (index) array (of integers) randomly

!     vector_index() an array of constant stride index values for integers
!     cycle_index() an array of indices divided cyclically nproc ways for integers
!     shuffle_index() an array of indices shuffled nproc ways for integers

!     prng_next()
!     prng_vector()
!     prng_restrt()
!     prng_chkpnt() parallel random number generator

!     cvmgt() vector merge
!     cvmgp()
!     cvmgm()
!     cvmgz()
!     cvmgn()

!     bit_size() for segment_t, tile_t, block_t, accumulator_t
!     swap() for segment_t, tile_t, block_t, accumulator_t

! **********************************************************************

!  parallel_functions

module parallel_functions

! **********************************************************************
! use standard parameterization of processor dependencies

use standard_types

! a basic set of utility and computational routines

use standard_functions

! **********************************************************************

!  RCS strings

! **********************************************************************

   character( len= *), parameter :: parallel_functions_rcs_source = &
      '$Source$'

   character( len= *), parameter :: parallel_functions_rcs_revision = &
      '$Revision$'

   character( len= *), parameter :: parallel_functions_rcs_state = &
      '$State$'

   character( len= *), parameter :: parallel_functions_rcs_date = &
      '$Date$'

! **********************************************************************

?? include 'coco.inc'

! **********************************************************************

!  parallel_functions types

! **********************************************************************

!  1-d segment_t

   type :: segment_t                             ! seg_i = [ low, high]

      integer :: i_lo                            ! low
      integer :: i_hi                            ! high

   end type segment_t

! **********************************************************************

!  2-d tile_t = segment_t x segment_t

   type :: tile_t                                ! ( seg_i, seg_j)

      integer :: i_lo                            ! low
      integer :: i_hi                            ! high

      integer :: j_lo                            ! j_lo
      integer :: j_hi                            ! j_hi

   end type tile_t

! **********************************************************************

!  3-d block_t = segment_t x segment_t x segment_t

   type :: block_t                               ! ( seg_i, seg_j, seg_k)

      integer :: i_lo                            ! low
      integer :: i_hi                            ! high

      integer :: j_lo                            ! j_lo
      integer :: j_hi                            ! j_hi

      integer :: k_lo                            ! k_lo
      integer :: k_hi                            ! k_hi

   end type block_t

! **********************************************************************

!  library

! **********************************************************************

!  declare interfaces for permute() function

   public :: permute

?? if( byte_k )then
   private :: byte_permute
?? endif
?? if( short_k )then
   private :: short_permute
?? endif
?? if( int_k )then
   private :: int_permute
?? endif
?? if( long_k )then
   private :: long_permute
?? endif

   interface permute
?? if( byte_k )then
      module procedure byte_permute
?? endif
?? if( short_k )then
      module procedure short_permute
?? endif
?? if( int_k )then
      module procedure int_permute
?? endif
?? if( long_k )then
      module procedure long_permute
?? endif
   end interface

!  declare interfaces for smag_sum() function

   public :: smag_sum

?? if( single_k )then
   private :: single_smag_sum
?? endif
?? if( double_k )then
   private :: double_smag_sum
?? endif
?? if( quad_k )then
   private :: quad_smag_sum
?? endif

   interface smag_sum
?? if( single_k )then
      module procedure single_smag_sum
?? endif
?? if( double_k )then
      module procedure double_smag_sum
?? endif
?? if( quad_k )then
      module procedure quad_smag_sum
?? endif
   end interface

!  declare interfaces for vector_index() function

   public :: vector_index

?? if( byte_k )then
   private :: byte_vector_index
?? endif
?? if( short_k )then
   private :: short_vector_index
?? endif
?? if( int_k )then
   private :: int_vector_index
?? endif
?? if( long_k )then
   private :: long_vector_index
?? endif

   interface vector_index
?? if( byte_k )then
      module procedure byte_vector_index
?? endif
?? if( short_k )then
      module procedure short_vector_index
?? endif
?? if( int_k )then
      module procedure int_vector_index
?? endif
?? if( long_k )then
      module procedure long_vector_index
?? endif
   end interface

!  declare interfaces for cyclic_index() function

   public :: cyclic_index

?? if( byte_k )then
   private :: byte_cyclic_index
?? endif
?? if( short_k )then
   private :: short_cyclic_index
?? endif
?? if( int_k )then
   private :: int_cyclic_index
?? endif
?? if( long_k )then
   private :: long_cyclic_index
?? endif

   interface cyclic_index
?? if( byte_k )then
      module procedure byte_cyclic_index
?? endif
?? if( short_k )then
      module procedure short_cyclic_index
?? endif
?? if( int_k )then
      module procedure int_cyclic_index
?? endif
?? if( long_k )then
      module procedure long_cyclic_index
?? endif
   end interface

!  declare interfaces for shuffle_index() function

   public :: shuffle_index

?? if( byte_k )then
   private :: byte_shuffle_index
?? endif
?? if( short_k )then
   private :: short_shuffle_index
?? endif
?? if( int_k )then
   private :: int_shuffle_index
?? endif
?? if( long_k )then
   private :: long_shuffle_index
?? endif

   interface shuffle_index
?? if( byte_k )then
      module procedure byte_shuffle_index
?? endif
?? if( short_k )then
      module procedure short_shuffle_index
?? endif
?? if( int_k )then
      module procedure int_shuffle_index
?? endif
?? if( long_k )then
      module procedure long_shuffle_index
?? endif
   end interface

! **********************************************************************

!  declare interfaces for cvmgt() functions

   public :: cvmgt

?? if( byte_k )then
   private :: byte_cvmgt
?? endif
?? if( short_k )then
   private :: short_cvmgt
?? endif
?? if( int_k )then
   private :: int_cvmgt
?? endif
?? if( long_k )then
   private :: long_cvmgt
?? endif
?? if( single_k )then
   private :: single_cvmgt
?? endif
?? if( double_k )then
   private :: double_cvmgt
?? endif
?? if( quad_k )then
   private :: quad_cvmgt
?? endif
?? if( l_byte_k )then
   private :: l_byte_cvmgt
?? endif
?? if( l_short_k )then
   private :: l_short_cvmgt
?? endif
?? if( l_int_k )then
   private :: l_int_cvmgt
?? endif
?? if( l_long_k )then
   private :: l_long_cvmgt
?? endif
?? if( single_k )then
   private :: single_complex_cvmgt
?? endif
?? if( double_k )then
   private :: double_complex_cvmgt
?? endif
?? if( quad_k )then
   private :: quad_complex_cvmgt
?? endif

   interface cvmgt
?? if( byte_k )then
      module procedure byte_cvmgt
?? endif
?? if( short_k )then
      module procedure short_cvmgt
?? endif
?? if( int_k )then
      module procedure int_cvmgt
?? endif
?? if( long_k )then
      module procedure long_cvmgt
?? endif
?? if( single_k )then
      module procedure single_cvmgt
?? endif
?? if( double_k )then
      module procedure double_cvmgt
?? endif
?? if( quad_k )then
      module procedure quad_cvmgt
?? endif
?? if( l_byte_k )then
      module procedure l_byte_cvmgt
?? endif
?? if( l_short_k )then
      module procedure l_short_cvmgt
?? endif
?? if( l_int_k )then
      module procedure l_int_cvmgt
?? endif
?? if( l_long_k )then
      module procedure l_long_cvmgt
?? endif
?? if( single_k )then
      module procedure single_complex_cvmgt
?? endif
?? if( double_k )then
      module procedure double_complex_cvmgt
?? endif
?? if( quad_k )then
      module procedure quad_complex_cvmgt
?? endif
   end interface

!  declare interfaces for cvmgp() functions

   public :: cvmgp

?? if( byte_k )then
   private :: byte_cvmgp
?? endif
?? if( short_k )then
   private :: short_cvmgp
?? endif
?? if( int_k )then
   private :: int_cvmgp
?? endif
?? if( long_k )then
   private :: long_cvmgp
?? endif
?? if( single_k )then
   private :: single_cvmgp
?? endif
?? if( double_k )then
   private :: double_cvmgp
?? endif
?? if( quad_k )then
   private :: quad_cvmgp
?? endif

   interface cvmgp
?? if( byte_k )then
      module procedure byte_cvmgp
?? endif
?? if( short_k )then
      module procedure short_cvmgp
?? endif
?? if( int_k )then
      module procedure int_cvmgp
?? endif
?? if( long_k )then
      module procedure long_cvmgp
?? endif
?? if( single_k )then
      module procedure single_cvmgp
?? endif
?? if( double_k )then
      module procedure double_cvmgp
?? endif
?? if( quad_k )then
      module procedure quad_cvmgp
?? endif
   end interface

!  declare interfaces for cvmgm() functions

   public :: cvmgm

?? if( byte_k )then
   private :: byte_cvmgm
?? endif
?? if( short_k )then
   private :: short_cvmgm
?? endif
?? if( int_k )then
   private :: int_cvmgm
?? endif
?? if( long_k )then
   private :: long_cvmgm
?? endif
?? if( single_k )then
   private :: single_cvmgm
?? endif
?? if( double_k )then
   private :: double_cvmgm
?? endif
?? if( quad_k )then
   private :: quad_cvmgm
?? endif

   interface cvmgm
?? if( byte_k )then
      module procedure byte_cvmgm
?? endif
?? if( short_k )then
      module procedure short_cvmgm
?? endif
?? if( int_k )then
      module procedure int_cvmgm
?? endif
?? if( long_k )then
      module procedure long_cvmgm
?? endif
?? if( single_k )then
      module procedure single_cvmgm
?? endif
?? if( double_k )then
      module procedure double_cvmgm
?? endif
?? if( quad_k )then
      module procedure quad_cvmgm
?? endif
   end interface

!  declare interfaces for cvmgz() functions

   public :: cvmgz

?? if( byte_k )then
   private :: byte_cvmgz
?? endif
?? if( short_k )then
   private :: short_cvmgz
?? endif
?? if( int_k )then
   private :: int_cvmgz
?? endif
?? if( long_k )then
   private :: long_cvmgz
?? endif
?? if( single_k )then
   private :: single_cvmgz
?? endif
?? if( double_k )then
   private :: double_cvmgz
?? endif
?? if( quad_k )then
   private :: quad_cvmgz
?? endif
?? if( single_k )then
   private :: single_complex_cvmgz
?? endif
?? if( double_k )then
   private :: double_complex_cvmgz
?? endif
?? if( quad_k )then
   private :: quad_complex_cvmgz
?? endif

   interface cvmgz
?? if( byte_k )then
      module procedure byte_cvmgz
?? endif
?? if( short_k )then
      module procedure short_cvmgz
?? endif
?? if( int_k )then
      module procedure int_cvmgz
?? endif
?? if( long_k )then
      module procedure long_cvmgz
?? endif
?? if( single_k )then
      module procedure single_cvmgz
?? endif
?? if( double_k )then
      module procedure double_cvmgz
?? endif
?? if( quad_k )then
      module procedure quad_cvmgz
?? endif
?? if( single_k )then
      module procedure single_complex_cvmgz
?? endif
?? if( double_k )then
      module procedure double_complex_cvmgz
?? endif
?? if( quad_k )then
      module procedure quad_complex_cvmgz
?? endif
   end interface

!  declare interfaces for cvmgn() functions

   public :: cvmgn

?? if( byte_k )then
   private :: byte_cvmgn
?? endif
?? if( short_k )then
   private :: short_cvmgn
?? endif
?? if( int_k )then
   private :: int_cvmgn
?? endif
?? if( long_k )then
   private :: long_cvmgn
?? endif
?? if( single_k )then
   private :: single_cvmgn
?? endif
?? if( double_k )then
   private :: double_cvmgn
?? endif
?? if( quad_k )then
   private :: quad_cvmgn
?? endif
?? if( single_k )then
   private :: single_complex_cvmgn
?? endif
?? if( double_k )then
   private :: double_complex_cvmgn
?? endif
?? if( quad_k )then
   private :: quad_complex_cvmgn
?? endif

   interface cvmgn
?? if( byte_k )then
      module procedure byte_cvmgn
?? endif
?? if( short_k )then
      module procedure short_cvmgn
?? endif
?? if( int_k )then
      module procedure int_cvmgn
?? endif
?? if( long_k )then
      module procedure long_cvmgn
?? endif
?? if( single_k )then
      module procedure single_cvmgn
?? endif
?? if( double_k )then
      module procedure double_cvmgn
?? endif
?? if( quad_k )then
      module procedure quad_cvmgn
?? endif
?? if( single_k )then
      module procedure single_complex_cvmgn
?? endif
?? if( double_k )then
      module procedure double_complex_cvmgn
?? endif
?? if( quad_k )then
      module procedure quad_complex_cvmgn
?? endif
   end interface

! **********************************************************************

!  module procedures

! **********************************************************************

contains                                                   ! parallel_functions

! **********************************************************************

!  segment() partition a (loop) range as evenly as possible

subroutine segment( low, high, nproc, subseg)

!  input ( low, high), to be divided nproc ways
!  output subseg=( ibgn, iend), ( 0, 0) ==> error

integer, intent( in) :: low, high, nproc

type( segment_t), intent( out), dimension( nproc) :: subseg

!  local data

   integer :: quot, rem, cnt

!  segment()

continue                                                   ! segment()

!  get size of segment

   cnt = high - low + 1                                    ! size of range

!     detect nonsense

   if( low >= high .or. nproc < 1 )then                    ! range or processor number

      subseg( 1) = segment_t( 0, 0)                        ! signal error
      return

!     detect no-op

   elseif( nproc == 1 )then                                ! one processor

      subseg( 1) = segment_t( low, high)                   ! only range is low..high
      return

!     detect 1-1

   elseif( nproc == cnt )then                              ! one processor, one loop index

      subseg = (/ ( subseg_t( i, i), i = low, high) /)
      return

   endif

!  get remainder and quotient

   rem = cnt .mod. nproc
   quot = cnt / nproc

!  if nonzero remainder, first rem segments will be one greater

   if( rem /= 0 ) quot = quot + 1                          ! divide evenly?

!  first range

   subseg( 1) = segment_t( low, low + quot - 1)

!  loop thru segments (if needed)

   do i = 2, nproc-1                                       ! if nproc > 2

!  set first

      subseg( i)%i_lo = subseg( i-1)%i_lo + quot
      rem = rem - 1                                        ! decrement counter

!  after first rem segments, reset for rest of segments

      if( rem == 0 ) quot = quot - 1                       ! rest are one smaller

!  set last

      subseg( i)%i_hi = subseg( i-1)%i_hi + quot

   enddo

!  set up last segment

   subseg( nproc) = segment_t( subseg( nproc-1)%i_hi + 1, high)

!  having partitioned the original segment nproc ways as fairly as possible

return                                                     ! segment()

!  segment()

end subroutine segment

! **********************************************************************

!  tile() partition a 2-d (loop nest) range as evenly as possible

subroutine tile( lo1, hi1, lo2, hi2, np1, np2, tiles)

integer, intent( in) :: lo1, hi1, lo2, hi2, np1, np2

type( tile_t), intent( out), dimension( np1, np2) :: tiles

!  segs along 1, segs along 2

   type( segment_t), dimension( np1) :: i_segs
   type( segment_t), dimension( np2) :: j_segs

!  tile()

continue                                                   ! tile()

   call segment( lo1, hi1, np1, i_segs)
   call segment( lo2, hi2, np2, j_segs)

   do i = 1, np1
      do j = 1, np2

         tiles( i, j) = tile_t( i_segs(i)%i_lo, i_segs(i)%i_hi, &
                                j_segs(j)%i_lo, j_segs(j)%i_hi)

      enddo
   enddo

return                                                     ! tile()

!  tile()

end subroutine tile

! **********************************************************************

!  block() partition a 3-d (loop nest) range as evenly as possible

subroutine block( lo1, hi1, lo2, hi2, lo3, hi3, np1, np2, np3, blks)

integer, intent( in) :: lo1, hi1, lo2, hi2, lo3, hi3, np1, np2, np3

type( block_t), intent( out), dimension( np1, np2, np3) :: blks

!  segs along 1, segs along 2, segs along 3

   type( segment_t), dimension( np1) :: i_segs
   type( segment_t), dimension( np2) :: j_segs
   type( segment_t), dimension( np3) :: k_segs

!  block()

continue                                                   ! block()

   call segment( lo1, hi1, np1, i_segs)
   call segment( lo2, hi2, np2, j_segs)
   call segment( lo3, hi3, np3, k_segs)

   do i = 1, np1
      do j = 1, np2
         do k = 1, np3

            blks( i, j) = block_t( i_segs(i)%i_lo, i_segs(i)%i_hi, &
                                   j_segs(j)%i_lo, j_segs(j)%i_hi, &
                                   k_segs(k)%i_lo, k_segs(k)%i_hi)

         enddo
      enddo
   enddo

return                                                     ! block()

!  block()

end subroutine block

! **********************************************************************

!  byte_permute(): permute for kind byte

! **********************************************************************

!  permute() permute an array of integers

?? if( byte_k )then
! **********************************************************************

!  byte_permute(): permute for kind byte

subroutine byte_permute( n, narr)

integer( kind= byte_k), intent( in) :: n
integer( kind= byte_k), dimension( n), intent( inout) :: narr

!  local data

   real( kind= single_k), dimension( n) :: ra

   integer( kind= int_k) :: i, ir, j, l

   real( kind= single_k) :: rra
   integer( kind= byte_k) :: rnarr

!  byte_permute()

continue                                                   ! permute()

!  set up array ra to hold random numbers

   call random_number( ra)

!  start sorting

   if( n < 2 ) return
   l = n/2 + 1
   ir = n

10 continue

      if( l > 1 )then
         l = l - 1

         rra = ra( l)
         rnarr = narr( l)

      else
         rra = ra( ir)
         rnarr = narr( ir)

         ra( ir) = ra( 1)
         narr( ir) = narr( 1)

         ir = ir - 1

         if( ir == 1 )then

            ra( 1) = rra
            narr( 1) = rnarr

            return
         endif
      endif

      i = l
      j = l + l

20    continue
         if( j <= ir )then

            if( j < ir )then
               if( ra( j) < ra( j+1) ) j = j + 1

            endif

            if( rra < ra( j) )then

               ra( i) = ra( j)
               narr( i) = narr( j)

               i = j
               j = j + j

            else
               j = ir + 1
            endif
      goto 20
         endif

      ra( i) = rra
      narr( i) = rnarr

   goto 10

!  byte_permute()

end subroutine byte_permute

?? endif
?? if( short_k )then
! **********************************************************************

!  short_permute(): permute for kind short

subroutine short_permute( n, narr)

integer( kind= short_k), intent( in) :: n
integer( kind= short_k), dimension( n), intent( inout) :: narr

!  local data

   real( kind= single_k), dimension( n) :: ra

   integer( kind= int_k) :: i, ir, j, l

   real( kind= single_k) :: rra
   integer( kind= short_k) :: rnarr

!  short_permute

continue                                                   ! permute()

!  set up array ra to hold random numbers

   call random_number( ra)

!  start sorting

   if( n < 2 ) return
   l = n/2 + 1
   ir = n

10 continue

      if( l > 1 )then
         l = l - 1

         rra = ra( l)
         rnarr = narr( l)

      else
         rra = ra( ir)
         rnarr = narr( ir)

         ra( ir) = ra( 1)
         narr( ir) = narr( 1)

         ir = ir - 1

         if( ir == 1 )then

            ra( 1) = rra
            narr( 1) = rnarr

            return
         endif
      endif

      i = l
      j = l + l

20    continue
         if( j <= ir )then

            if( j < ir )then
               if( ra( j) < ra( j+1) ) j = j + 1

            endif

            if( rra < ra( j) )then

               ra( i) = ra( j)
               narr( i) = narr( j)

               i = j
               j = j + j

            else
               j = ir + 1
            endif
      goto 20
         endif

      ra( i) = rra
      narr( i) = rnarr

   goto 10

!  short_permute()

end subroutine short_permute

?? endif
?? if( int_k )then
! **********************************************************************

!  int_permute(): permute for kind int

subroutine int_permute( n, narr)

integer( kind= int_k), intent( in) :: n
integer( kind= int_k), dimension( n), intent( inout) :: narr

!  local data

   real( kind= single_k), dimension( n) :: ra

   integer( kind= int_k) :: i, ir, j, l

   real( kind= single_k) :: rra
   integer( kind= int_k) :: rnarr

!  int_permute()

continue                                                   ! permute()

!  set up array ra to hold random numbers

   call random_number( ra)

!  start sorting

   if( n < 2 ) return
   l = n/2 + 1
   ir = n

10 continue

      if( l > 1 )then
         l = l - 1

         rra = ra( l)
         rnarr = narr( l)

      else
         rra = ra( ir)
         rnarr = narr( ir)

         ra( ir) = ra( 1)
         narr( ir) = narr( 1)

         ir = ir - 1

         if( ir == 1 )then

            ra( 1) = rra
            narr( 1) = rnarr

            return
         endif
      endif

      i = l
      j = l + l

20    continue
         if( j <= ir )then

            if( j < ir )then
               if( ra( j) < ra( j+1) ) j = j + 1

            endif

            if( rra < ra( j) )then

               ra( i) = ra( j)
               narr( i) = narr( j)

               i = j
               j = j + j

            else
               j = ir + 1
            endif
      goto 20
         endif

      ra( i) = rra
      narr( i) = rnarr

   goto 10

!  int_permute()

end subroutine int_permute

?? endif
?? if( long_k )then
! **********************************************************************

!  long_permute(): permute for kind int

subroutine long_permute( n, narr)

integer( kind= long_k), intent( in) :: n
integer( kind= long_k), dimension( n), intent( inout) :: narr

!  local data

   real( kind= single_k), dimension( n) :: ra

   integer( kind= long_k) :: i, ir, j, l

   real( kind= single_k) :: rra
   integer( kind= long_k) :: rnarr

!  long_permute()

continue                                                   ! permute()

!  set up array ra to hold random numbers

   call random_number( ra)

!  start sorting

   if( n < 2 ) return
   l = n/2 + 1
   ir = n

10 continue

      if( l > 1 )then
         l = l - 1

         rra = ra( l)
         rnarr = narr( l)

      else
         rra = ra( ir)
         rnarr = narr( ir)

         ra( ir) = ra( 1)
         narr( ir) = narr( 1)

         ir = ir - 1

         if( ir == 1 )then

            ra( 1) = rra
            narr( 1) = rnarr

            return
         endif
      endif

      i = l
      j = l + l

20    continue
         if( j <= ir )then

            if( j < ir )then
               if( ra( j) < ra( j+1) ) j = j + 1

            endif

            if( rra < ra( j) )then

               ra( i) = ra( j)
               narr( i) = narr( j)

               i = j
               j = j + j

            else
               j = ir + 1
            endif
      goto 20
         endif

      ra( i) = rra
      narr( i) = rnarr

   goto 10

!  long_permute()

end subroutine long_permute

?? endif
! **********************************************************************

!  smag_sum(): real summation sorted by magnitude

?? if( single_k )then
! **********************************************************************

!  single_smag_sum(): smag_sum for kind single

subroutine single_smag_sum( total, addend, delta)

real( kind= single_k), intent( out), optional :: total
real( kind= single_k), intent( in) :: addend
real( kind= single_k), intent( out), optional :: delta

!  local data

   real( kind= single_k), &
   dimension( minexponent( 1._single_k): maxexponent( 1._single_k)), &
   save :: accumulator = 0._single_k

   real( kind= single_k) :: o_sum

   real( kind= single_k), save :: last_sum = 0._single_k

!  single_smag_sum()

continue                                                   ! smag_sum()

!  first, update accumulator

   accumulator( exponent( addend) ) = &
   accumulator( exponent( addend) ) + addend

!  calculate ordered sum if total or delta are needed

   must_sum: if( present( total) .or. present( delta) )then

      o_sum = 0._single_k

!  compute sum in increasing exponent order

      ordered_sum: do i = minexponent( 1._single_k), maxexponent( 1._single_k)

         o_sum = o_sum + accumulator( i)

      end do ordered_sum

!  report total

      output_total: if( present( total) )then

         total = o_sum                                     ! return value requested

      endif output_total

!  report delta and save sum

      output_delta: if( present( delta) )then

         delta = o_sum - last_sum                          ! return value requested

         last_sum = o_sum                                  ! update for next time

      endif output_delta

   endif must_sum

return                                                     ! smag_sum()

!  single_smag_sum()

end subroutine single_smag_sum

?? endif
?? if( double_k )then
! **********************************************************************

!  double_smag_sum(): smag_sum for kind double

subroutine double_smag_sum( total, addend, delta)

real( kind= double_k), intent( out), optional :: total
real( kind= double_k), intent( in) :: addend
real( kind= double_k), intent( out), optional :: delta

!  local data

   real( kind= double_k), &
   dimension( minexponent( 1._double_k): maxexponent( 1._double_k)), &
   save :: accumulator = 0._double_k

   real( kind= double_k) :: o_sum

   real( kind= double_k), save :: last_sum

!  double_smag_sum()

continue                                                   ! smag_sum()

!  first, update accumulator

   accumulator( exponent( addend) ) = &
   accumulator( exponent( addend) ) + addend

!  calculate ordered sum if total or delta are needed

   must_sum: if( present( total) .or. present( delta) )then

      o_sum = 0._double_k

!  compute sum in increasing exponent order

      ordered_sum: do i = minexponent( 1._double_k), maxexponent( 1._double_k)

         o_sum = o_sum + accumulator( i)

      end do ordered_sum

!  report total

      output_total: if( present( total) )then

         total = o_sum                                     ! return value requested

      endif output_total

!  report delta and save sum

      output_delta: if( present( delta) )then

         delta = o_sum - last_sum                          ! return value requested

         last_sum = o_sum                                  ! update for next time

      endif output_delta

   endif must_sum

return                                                     ! smag_sum()

!  double_smag_sum()

end subroutine double_smag_sum

?? endif
?? if( quad_k )then
! **********************************************************************

!  quad_smag_sum(): smag_sum for kind double

subroutine quad_smag_sum( total, addend, delta)

real( kind= quad_k), intent( out), optional :: total
real( kind= quad_k), intent( in) :: addend
real( kind= quad_k), intent( out), optional :: delta

!  local data

   real( kind= quad_k), &
   dimension( minexponent( 1._quad_k): maxexponent( 1._quad_k)), &
   save :: accumulator = 0._quad_k

   real( kind= quad_k) :: o_sum

   real( kind= quad_k), save :: last_sum

!  quad_smag_sum()

continue                                                   ! smag_sum()

!  first, update accumulator

   accumulator( exponent( addend) ) = &
   accumulator( exponent( addend) ) + addend

!  calculate ordered sum if total or delta are needed

   must_sum: if( present( total) .or. present( delta) )then

      o_sum = 0._quad_k

!  compute sum in increasing exponent order

      ordered_sum: do i = minexponent( 1._quad_k), maxexponent( 1._quad_k)

         o_sum = o_sum + accumulator( i)

      end do ordered_sum

!  report total

      output_total: if( present( total) )then

         total = o_sum                                     ! return value requested

      endif output_total

!  report delta and save sum

      output_delta: if( present( delta) )then

         delta = o_sum - last_sum                          ! return value requested

         last_sum = o_sum                                  ! update for next time

      endif output_delta

   endif must_sum

return                                                     ! smag_sum()

!  quad_smag_sum()

end subroutine quad_smag_sum

?? endif
! **********************************************************************

!  vector_index(): return array of constant stride index values

?? if( byte_k )then
! **********************************************************************

!  byte_vector_index(): vector_index() for kind byte

subroutine byte_vector_index( indx, b1, b2, b3)

integer( kind= byte_k), dimension( *), intent( out) :: indx

integer( kind= byte_k), intent( in) :: b1, b2
integer( kind= byte_k), intent( in), optional :: b3

!  vector_index()

continue                                                   ! vector_index()

   if( present( b3) )then                                  ! if have stride, use it
      do i = b1, b2, b3
         indx( i) = i
      enddo

   else                                                    ! no stride provided, use 1
      do i = b1, b2
         indx( i) = i
      enddo

   endif

return                                                     ! vector_index()

!  byte_vector_index()

end subroutine byte_vector_index

?? endif
?? if( short_k )then
! **********************************************************************

!  short_vector_index(): vector_index() for kind short

subroutine short_vector_index( indx, b1, b2, b3)

integer( kind= short_k), dimension( *), intent( out) :: indx

integer( kind= short_k), intent( in) :: b1, b2
integer( kind= short_k), intent( in), optional :: b3

!  short_vector_index()

continue                                                   ! vector_index()

   if( present( b3) )then                                  ! if have stride, use it
      do i = b1, b2, b3
         indx( i) = i
      enddo

   else                                                    ! no stride provided, use 1
      do i = b1, b2
         indx( i) = i
      enddo

   endif

return                                                     ! vector_index()

!  short_vector_index()

end subroutine short_vector_index

?? endif
?? if( int_k )then
! **********************************************************************

!  int_vector_index(): vector_index() for kind int

subroutine int_vector_index( indx, b1, b2, b3)

integer( kind= int_k), dimension( *), intent( out) :: indx

integer( kind= int_k), intent( in) :: b1, b2
integer( kind= int_k), intent( in), optional :: b3

!  int_vector_index()

continue                                                   ! vector_index()

   if( present( b3) )then                                  ! if have stride, use it
      do i = b1, b2, b3
         indx( i) = i
      enddo

   else                                                    ! no stride provided, use 1
      do i = b1, b2
         indx( i) = i
      enddo

   endif

return                                                     ! vector_index()

!  int_vector_index()

end subroutine int_vector_index

?? endif
?? if( long_k )then
! **********************************************************************

!  long_vector_index(): vector_index() for kind int

subroutine long_vector_index( indx, b1, b2, b3)

integer( kind= long_k), dimension( *), intent( out) :: indx

integer( kind= long_k), intent( in) :: b1, b2
integer( kind= long_k), intent( in), optional :: b3

!  long_vector_index()

continue                                                   ! vector_index()

   if( present( b3) )then                                  ! if have stride, use it
      do i = b1, b2, b3
         indx( i) = i
      enddo

   else                                                    ! no stride provided, use 1
      do i = b1, b2
         indx( i) = i
      enddo

   endif

return                                                     ! vector_index()

!  long_vector_index()

end subroutine long_vector_index

?? endif
! **********************************************************************

!  cyclic_index(): return array of constant stride index values

?? if( byte_k )then
! **********************************************************************

!  byte_cyclic_index(): cyclic_index() for kind byte

subroutine byte_cyclic_index( mindx, m, nproc, sindx, n)

integer( kind= byte_k), intent( in) :: m, n, nproc

integer( kind= byte_k), dimension( m), intent( in) :: mindx

integer( kind= byte_k), dimension( n, nproc), intent( out) :: sindx

!  byte_cycle_index()

continue                                                   ! cycle_index()

   i = 0                                                   ! initialize master index

block:   do j = 1, n                                       ! block per proc

      proc: do k = 1, nproc                                ! processors

               i = i + 1                                   ! increment master index
               sindx( j, k) = mindx( i)                    ! copy to subindex

               if( i >= m ) exit block                     ! quit when too many

            enddo proc                                     ! processors

         enddo block                                       ! block per processor

return                                                     ! cycle_index()

!  byte_cyclic_index()

end subroutine byte_cyclic_index

?? endif
?? if( short_k )then
! **********************************************************************

!  short_cyclic_index(): cyclic_index() for kind short

subroutine short_cyclic_index( mindx, m, nproc, sindx, n)

integer( kind= short_k), intent( in) :: m, n, nproc

integer( kind= short_k), dimension( m), intent( in) :: mindx

integer( kind= short_k), dimension( n, nproc), intent( out) :: sindx

!  short_cycle_index()

continue                                                   ! cycle_index()

   i = 0                                                   ! initialize master index

block:   do j = 1, n                                       ! block per proc

      proc: do k = 1, nproc                                ! processors

               i = i + 1                                   ! increment master index
               sindx( j, k) = mindx( i)                    ! copy to subindex

               if( i >= m ) exit block                     ! quit when too many

            enddo proc                                     ! processors

         enddo block                                       ! block per processor

return                                                     ! cycle_index()

!  short_cyclic_index()

end subroutine short_cyclic_index

?? endif
?? if( int_k )then
! **********************************************************************

!  int_cyclic_index(): cyclic_index() for kind int

subroutine int_cyclic_index( mindx, m, nproc, sindx, n)

integer( kind= int_k), intent( in) :: m, n, nproc

integer( kind= int_k), dimension( m), intent( in) :: mindx

integer( kind= int_k), dimension( n, nproc), intent( out) :: sindx

!  int_cycle_index()

continue                                                   ! cycle_index()

   i = 0                                                   ! initialize master index

block:   do j = 1, n                                       ! block per proc

      proc: do k = 1, nproc                                ! processors

               i = i + 1                                   ! increment master index
               sindx( j, k) = mindx( i)                    ! copy to subindex

               if( i >= m ) exit block                     ! quit when too many

            enddo proc                                     ! processors

         enddo block                                       ! block per processor

return                                                     ! cycle_index()

!  int_cyclic_index()

end subroutine int_cyclic_index

?? endif
?? if( long_k )then
! **********************************************************************

!  long_cyclic_index(): cyclic_index() for kind int

subroutine long_cyclic_index( mindx, m, nproc, sindx, n)

integer( kind= long_k), intent( in) :: m, n, nproc

integer( kind= long_k), dimension( m), intent( in) :: mindx

integer( kind= long_k), dimension( n, nproc), intent( out) :: sindx

!  long_cycle_index()

continue                                                   ! cycle_index()

   i = 0                                                   ! initialize master index

block:   do j = 1, n                                       ! block per proc

      proc: do k = 1, nproc                                ! processors

               i = i + 1                                   ! increment master index
               sindx( j, k) = mindx( i)                    ! copy to subindex

               if( i >= m ) exit block                     ! quit when too many

            enddo proc                                     ! processors

         enddo block                                       ! block per processor

return                                                     ! cycle_index()

!  long_cyclic_index()

end subroutine long_cyclic_index

?? endif
! **********************************************************************

!  shuffle_index(): return array of constant stride index values

?? if( byte_k )then
! **********************************************************************

!  byte_shuffle_index(): shuffle_index() for kind byte

subroutine byte_shuffle_index( mindx, m, nproc, sindx, n)

integer( kind= byte_k), intent( in) :: m, n, nproc

integer( kind= byte_k), dimension( m), intent( in) :: mindx

integer( kind= byte_k), dimension( n, nproc), intent( out) :: sindx

!  byte_cycle_index()

continue                                                   ! cycle_index()

   i = 0                                                   ! initialize master index

block:   do j = 1, n                                       ! block per proc

      proc: do k = 1, nproc                                ! processors

               i = i + 1                                   ! increment master index
               sindx( j, k) = mindx( i)                    ! copy to subindex

               if( i >= m ) exit block                     ! quit when too many

            enddo proc                                     ! processors

         enddo block                                       ! block per processor

return                                                     ! cycle_index()

!  byte_shuffle_index()

end subroutine byte_shuffle_index

?? endif
?? if( short_k )then
! **********************************************************************

!  short_shuffle_index(): shuffle_index() for kind short

subroutine short_shuffle_index( mindx, m, nproc, sindx, n)

integer( kind= short_k), intent( in) :: m, n, nproc

integer( kind= short_k), dimension( m), intent( in) :: mindx

integer( kind= short_k), dimension( n, nproc), intent( out) :: sindx

!  short_cycle_index()

continue                                                   ! cycle_index()

   i = 0                                                   ! initialize master index

block:   do j = 1, n                                       ! block per proc

      proc: do k = 1, nproc                                ! processors

               i = i + 1                                   ! increment master index
               sindx( j, k) = mindx( i)                    ! copy to subindex

               if( i >= m ) exit block                     ! quit when too many

            enddo proc                                     ! processors

         enddo block                                       ! block per processor

return                                                     ! cycle_index()

!  short_shuffle_index()

end subroutine short_shuffle_index

?? endif
?? if( int_k )then
! **********************************************************************

!  int_shuffle_index(): shuffle_index() for kind int

subroutine int_shuffle_index( mindx, m, nproc, sindx, n)

integer( kind= int_k), intent( in) :: m, n, nproc

integer( kind= int_k), dimension( m), intent( in) :: mindx

integer( kind= int_k), dimension( n, nproc), intent( out) :: sindx

!  int_cycle_index()

continue                                                   ! cycle_index()

   i = 0                                                   ! initialize master index

block:   do j = 1, n                                       ! block per proc

      proc: do k = 1, nproc                                ! processors

               i = i + 1                                   ! increment master index
               sindx( j, k) = mindx( i)                    ! copy to subindex

               if( i >= m ) exit block                     ! quit when too many

            enddo proc                                     ! processors

         enddo block                                       ! block per processor

return                                                     ! cycle_index()

!  int_shuffle_index()

end subroutine int_shuffle_index

?? endif
?? if( long_k )then
! **********************************************************************

!  long_shuffle_index(): shuffle_index() for kind int

subroutine long_shuffle_index( mindx, m, nproc, sindx, n)

integer( kind= long_k), intent( in) :: m, n, nproc

integer( kind= long_k), dimension( m), intent( in) :: mindx

integer( kind= long_k), dimension( n, nproc), intent( out) :: sindx

!  long_cycle_index()

continue                                                   ! cycle_index()

   i = 0                                                   ! initialize master index

block:   do j = 1, n                                       ! block per proc

      proc: do k = 1, nproc                                ! processors

               i = i + 1                                   ! increment master index
               sindx( j, k) = mindx( i)                    ! copy to subindex

               if( i >= m ) exit block                     ! quit when too many

            enddo proc                                     ! processors

         enddo block                                       ! block per processor

return                                                     ! cycle_index()

!  long_shuffle_index()

end subroutine long_shuffle_index

?? endif
! **********************************************************************

!  vector merges: merge two vectors into one by a third

! **********************************************************************

!  cvmgt(): first vector if third is true, second otherwise

?? if( byte_k )then
! **********************************************************************

!  byte_cvmgt(): cvmgt() for kind byte

elemental integer( kind= byte_k) function byte_cvmgt( b1, b2, l)

integer( kind= byte_k), intent( in) :: b1, b2

logical, intent( in) :: l

!  byte_cvmgt()

continue                                                   ! cvmgt()

   if( l )then
      byte_cvmgt = b1

   else
      byte_cvmgt = b2
   endif

return                                                     ! cvmgt()

!  byte_cvmgt()

end function byte_cvmgt

?? endif
?? if( short_k )then
! **********************************************************************

!  short_cvmgt(): cvmgt() for kind short

elemental integer( kind= short_k) function short_cvmgt( b1, b2, l)

integer( kind= short_k), intent( in) :: b1, b2

logical, intent( in) :: l

!  short_cvmgt()

continue                                                   ! cvmgt()

   if( l )then
      short_cvmgt = b1

   else
      short_cvmgt = b2
   endif

return                                                     ! cvmgt()

!  short_cvmgt()

end function short_cvmgt

?? endif
?? if( int_k )then
! **********************************************************************

!  int_cvmgt(): cvmgt() for kind int

elemental integer( kind= int_k) function int_cvmgt( b1, b2, l)

integer( kind= int_k), intent( in) :: b1, b2

logical, intent( in) :: l

!  int_cvmgt()

continue                                                   ! cvmgt()

   if( l )then
      int_cvmgt = b1

   else
      int_cvmgt = b2
   endif

return                                                     ! cvmgt()

!  int_cvmgt()

end function int_cvmgt

?? endif
?? if( long_k )then
! **********************************************************************

!  long_cvmgt(): cvmgt() for kind int

elemental integer( kind= long_k) function long_cvmgt( b1, b2, l)

integer( kind= long_k), intent( in) :: b1, b2

logical, intent( in) :: l

!  long_cvmgt()

continue                                                   ! cvmgt()

   if( l )then
      long_cvmgt = b1

   else
      long_cvmgt = b2
   endif

return                                                     ! cvmgt()

!  long_cvmgt()

end function long_cvmgt

?? endif
?? if( single_k )then
! **********************************************************************

!  single_cvmgt(): cvmgt() for kind single

elemental real( kind= single_k) function single_cvmgt( b1, b2, l)

real( kind= single_k), intent( in) :: b1, b2

logical, intent( in) :: l

!  single_cvmgt()

continue                                                   ! cvmgt()

   if( l )then
      single_cvmgt = b1

   else
      single_cvmgt = b2
   endif

return                                                     ! cvmgt()

!  single_cvmgt()

end function single_cvmgt

?? endif
?? if( double_k )then
! **********************************************************************

!  double_cvmgt(): cvmgt() for kind double

elemental real( kind= double_k) function double_cvmgt( b1, b2, l)

real( kind= double_k), intent( in) :: b1, b2

logical, intent( in) :: l

!  double_cvmgt()

continue                                                   ! cvmgt()

   if( l )then
      double_cvmgt = b1

   else
      double_cvmgt = b2
   endif

return                                                     ! cvmgt()

!  double_cvmgt()

end function double_cvmgt

?? endif
?? if( quad_k )then
! **********************************************************************

!  quad_cvmgt(): cvmgt() for kind double

elemental real( kind= quad_k) function quad_cvmgt( b1, b2, l)

real( kind= quad_k), intent( in) :: b1, b2

logical, intent( in) :: l

!  quad_cvmgt()

continue                                                   ! cvmgt()

   if( l )then
      quad_cvmgt = b1

   else
      quad_cvmgt = b2
   endif

return                                                     ! cvmgt()

!  quad_cvmgt()

end function quad_cvmgt

?? endif
?? if( l_byte_k )then
! **********************************************************************

!  l_byte_cvmgt(): cvmgt() for kind space

elemental logical( kind= l_byte_k) function l_byte_cvmgt( b1, b2, l)

logical( kind= l_byte_k), intent( in) :: b1, b2

logical, intent( in) :: l

!  l_byte_cvmgt()

continue                                                   ! cvmgt()

   if( l )then
      space_cvmgt = b1

   else
      space_cvmgt = b2
   endif

return                                                     ! cvmgt()

!  l_byte_cvmgt()

end function l_byte_cvmgt

?? endif
?? if( l_short_k )then
! **********************************************************************

!  l_short_cvmgt(): cvmgt() for kind space

elemental logical( kind= l_short_k) function l_short_cvmgt( b1, b2, l)

logical( kind= l_short_k), intent( in) :: b1, b2

logical, intent( in) :: l

!  l_short_cvmgt()

continue                                                   ! cvmgt()

   if( l )then
      space_cvmgt = b1

   else
      space_cvmgt = b2
   endif

return                                                     ! cvmgt()

!  l_short_cvmgt()

end function l_short_cvmgt

?? endif
?? if( l_int_k )then
! **********************************************************************

!  l_int_cvmgt(): cvmgt() for kind space

elemental logical( kind= l_int_k) function l_int_cvmgt( b1, b2, l)

logical( kind= l_int_k), intent( in) :: b1, b2

logical, intent( in) :: l

!  l_int_cvmgt()

continue                                                   ! cvmgt()

   if( l )then
      space_cvmgt = b1

   else
      space_cvmgt = b2
   endif

return                                                     ! cvmgt()

!  l_int_cvmgt()

end function l_int_cvmgt

?? endif
?? if( l_long_k )then
! **********************************************************************

!  l_long_cvmgt(): cvmgt() for kind l_long

elemental logical( kind= l_long_k) function l_long_cvmgt( b1, b2, l)

logical( kind= l_long_k), intent( in) :: b1, b2

logical( kind= l_int_k), intent( in) :: l

!  long_cvmgt()

continue                                                   ! cvmgt()

   if( l )then
      l_long_cvmgt = b1

   else
      l_long_cvmgt = b2
   endif

return                                                     ! cvmgt()

!  l_long_cvmgt()

end function l_long_cvmgt

?? endif
?? if( single_k )then
! **********************************************************************

!  single_complex_cvmgt(): cvmgt() for kind single_complex

elemental complex( kind= single_k) function single_complex_cvmgt( b1, b2, l)

complex( kind= single_k), intent( in) :: b1, b2

logical, intent( in) :: l

!  single_complex_cvmgt()

continue                                                   ! cvmgt()

   if( l )then
      single_complex_cvmgt = b1

   else
      single_complex_cvmgt = b2
   endif

return                                                     ! cvmgt()

!  single_complex_cvmgt()

end function single_complex_cvmgt

?? endif
?? if( double_k )then
! **********************************************************************

!  double_complex_cvmgt(): cvmgt() for kind double_complex

elemental complex( kind= double_k) function double_complex_cvmgt( b1, b2, l)

complex( kind= double_k), intent( in) :: b1, b2

logical, intent( in) :: l

!  double_complex_cvmgt()

continue                                                   ! cvmgt()

   if( l )then
      double_complex_cvmgt = b1

   else
      double_complex_cvmgt = b2
   endif

return                                                     ! cvmgt()

!  double_complex_cvmgt()

end function double_complex_cvmgt

?? endif
?? if( quad_k )then
! **********************************************************************

!  quad_complex_cvmgt(): cvmgt() for kind quad_complex

elemental complex( kind= quad_k) function quad_complex_cvmgt( b1, b2, l)

complex( kind= quad_k), intent( in) :: b1, b2

logical, intent( in) :: l

!  quad_complex_cvmgt()

continue                                                   ! cvmgt()

   if( l )then
      quad_complex_cvmgt = b1

   else
      quad_complex_cvmgt = b2
   endif

return                                                     ! cvmgt()

!  quad_complex_cvmgt()

end function quad_complex_cvmgt

?? endif
! **********************************************************************

!  cvmgp(): first vector if third is true, second otherwise

?? if( byte_k )then
! **********************************************************************

!  byte_cvmgp(): cvmgp() for kind byte

elemental integer( kind= byte_k) function byte_cvmgp( b1, b2, b3)

integer( kind= byte_k), intent( in) :: b1, b2, b3

!  byte_cvmgp()

continue                                                   ! cvmgp()

   if( b3 >= 0_byte_k )then
      byte_cvmgp = b1

   else
      byte_cvmgp = b2
   endif

return                                                     ! cvmgp()

!  byte_cvmgp()

end function byte_cvmgp

?? endif
?? if( short_k )then
! **********************************************************************

!  short_cvmgp(): cvmgp() for kind short

elemental integer( kind= short_k) function short_cvmgp( b1, b2, b3)

integer( kind= short_k), intent( in) :: b1, b2, b3

!  short_cvmgp()

continue                                                   ! cvmgp()

   if( b3 >= 0_short_k )then
      short_cvmgp = b1

   else
      short_cvmgp = b2
   endif

return                                                     ! cvmgp()

!  short_cvmgp()

end function short_cvmgp

?? endif
?? if( int_k )then
! **********************************************************************

!  int_cvmgp(): cvmgp() for kind int

elemental integer( kind= int_k) function int_cvmgp( b1, b2, b3)

integer( kind= int_k), intent( in) :: b1, b2, b3

!  int_cvmgp()

continue                                                   ! cvmgp()

   if( b3 >= 0_int_k )then
      int_cvmgp = b1

   else
      int_cvmgp = b2
   endif

return                                                     ! cvmgp()

!  int_cvmgp()

end function int_cvmgp

?? endif
?? if( long_k )then
! **********************************************************************

!  long_cvmgp(): cvmgp() for kind int

elemental integer( kind= long_k) function long_cvmgp( b1, b2, b3)

integer( kind= long_k), intent( in) :: b1, b2, b3

!  long_cvmgp()

continue                                                   ! cvmgp()

   if( b3 >= 0_long_k )then
      long_cvmgp = b1

   else
      long_cvmgp = b2
   endif

return                                                     ! cvmgp()

!  long_cvmgp()

end function long_cvmgp

?? endif
?? if( single_k )then
! **********************************************************************

!  single_cvmgp(): cvmgp() for kind single

elemental real( kind= single_k) function single_cvmgp( b1, b2, b3)

real( kind= single_k), intent( in) :: b1, b2, b3

!  single_cvmgp()

continue                                                   ! cvmgp()

   if( b3 >= 0._single_k )then
      single_cvmgp = b1

   else
      single_cvmgp = b2
   endif

return                                                     ! cvmgp()

!  single_cvmgp()

end function single_cvmgp

?? endif
?? if( double_k )then
! **********************************************************************

!  double_cvmgp(): cvmgp() for kind double

elemental real( kind= double_k) function double_cvmgp( b1, b2, b3)

real( kind= double_k), intent( in) :: b1, b2, b3

!  double_cvmgp()

continue                                                   ! cvmgp()

   if( b3 >= 0._double_k )then
      double_cvmgp = b1

   else
      double_cvmgp = b2
   endif

return                                                     ! cvmgp()

!  double_cvmgp()

end function double_cvmgp

?? endif
?? if( quad_k )then
! **********************************************************************

!  quad_cvmgp(): cvmgp() for kind double

elemental real( kind= quad_k) function quad_cvmgp( b1, b2, b3)

real( kind= quad_k), intent( in) :: b1, b2, b3

!  quad_cvmgp()

continue                                                   ! cvmgp()

   if( b3 >= 0._quad_k )then
      quad_cvmgp = b1

   else
      quad_cvmgp = b2
   endif

return                                                     ! cvmgp()

!  quad_cvmgp()

end function quad_cvmgp

?? endif
! **********************************************************************

!  cvmgm(): first vector if third is true, second otherwise

?? if( quad_k )then
! **********************************************************************

!  byte_cvmgm(): cvmgm() for kind byte

elemental integer( kind= byte_k) function byte_cvmgm( b1, b2, b3)

integer( kind= byte_k), intent( in) :: b1, b2, b3

!  byte_cvmgm()

continue                                                   ! cvmgm()

   if( b3 < 0_byte_k )then
      byte_cvmgm = b1

   else
      byte_cvmgm = b2
   endif

return                                                     ! cvmgm()

!  byte_cvmgm()

end function byte_cvmgm

?? endif
?? if( short_k )then
! **********************************************************************

!  short_cvmgm(): cvmgm() for kind short

elemental integer( kind= short_k) function short_cvmgm( b1, b2, b3)

integer( kind= short_k), intent( in) :: b1, b2, b3

!  short_cvmgm()

continue                                                   ! cvmgm()

   if( b3 < 0_short_k )then
      short_cvmgm = b1

   else
      short_cvmgm = b2
   endif

return                                                     ! cvmgm()

!  short_cvmgm()

end function short_cvmgm

?? endif
?? if( int_k )then
! **********************************************************************

!  int_cvmgm(): cvmgm() for kind int

elemental integer( kind= int_k) function int_cvmgm( b1, b2, b3)

integer( kind= int_k), intent( in) :: b1, b2, b3

!  int_cvmgm()

continue                                                   ! cvmgm()

   if( b3 < 0_int_k )then
      int_cvmgm = b1

   else
      int_cvmgm = b2
   endif

return                                                     ! cvmgm()

!  int_cvmgm()

end function int_cvmgm

?? endif
?? if( long_k )then
! **********************************************************************

!  long_cvmgm(): cvmgm() for kind int

elemental integer( kind= long_k) function long_cvmgm( b1, b2, b3)

integer( kind= long_k), intent( in) :: b1, b2, b3

!  long_cvmgm()

continue                                                   ! cvmgm()

   if( b3 < 0_long_k )then
      long_cvmgm = b1

   else
      long_cvmgm = b2
   endif

return                                                     ! cvmgm()

!  long_cvmgm()

end function long_cvmgm

?? endif
?? if( single_k )then
! **********************************************************************

!  single_cvmgm(): cvmgm() for kind single

elemental real( kind= single_k) function single_cvmgm( b1, b2, b3)

real( kind= single_k), intent( in) :: b1, b2, b3

!  single_cvmgm()

continue                                                   ! cvmgm()

   if( b3 < 0._single_k )then
      single_cvmgm = b1

   else
      single_cvmgm = b2
   endif

return                                                     ! cvmgm()

!  single_cvmgm()

end function single_cvmgm

?? endif
?? if( double_k )then
! **********************************************************************

!  double_cvmgm(): cvmgm() for kind double

elemental real( kind= double_k) function double_cvmgm( b1, b2, b3)

real( kind= double_k), intent( in) :: b1, b2, b3

!  double_cvmgm()

continue                                                   ! cvmgm()

   if( b3 < 0._double_k )then
      double_cvmgm = b1

   else
      double_cvmgm = b2
   endif

return                                                     ! cvmgm()

!  double_cvmgm()

end function double_cvmgm

?? endif
?? if( quad_k )then
! **********************************************************************

!  quad_cvmgm(): cvmgm() for kind double

elemental real( kind= quad_k) function quad_cvmgm( b1, b2, b3)

real( kind= quad_k), intent( in) :: b1, b2, b3

!  quad_cvmgm()

continue                                                   ! cvmgm()

   if( b3 < 0._quad_k )then
      quad_cvmgm = b1

   else
      quad_cvmgm = b2
   endif

return                                                     ! cvmgm()

!  quad_cvmgm()

end function byte_cvmgz

?? endif
! **********************************************************************

!  cvmgz(): first vector if third is true, second otherwise

?? if( byte_k )then
! **********************************************************************

!  byte_cvmgz(): cvmgz() for kind byte

elemental integer( kind= byte_k) function byte_cvmgz( b1, b2, b3)

integer( kind= byte_k), intent( in) :: b1, b2, b3

!  byte_cvmgz()

continue                                                   ! cvmgz()

   if( b3 == 0_byte_k )then
      byte_cvmgz = b1

   else
      byte_cvmgz = b2
   endif

return                                                     ! cvmgz()

!  byte_cvmgz()

end function byte_cvmgz

?? endif
?? if( short_k )then
! **********************************************************************

!  short_cvmgz(): cvmgz() for kind short

elemental integer( kind= short_k) function short_cvmgz( b1, b2, b3)

integer( kind= short_k), intent( in) :: b1, b2, b3

!  short_cvmgz()

continue                                                   ! cvmgz()

   if( b3 == 0_short_k )then
      short_cvmgz = b1

   else
      short_cvmgz = b2
   endif

return                                                     ! cvmgz()

!  short_cvmgz()

end function short_cvmgz

?? endif
?? if( int_k )then
! **********************************************************************

!  int_cvmgz(): cvmgz() for kind int

elemental integer( kind= int_k) function int_cvmgz( b1, b2, b3)

integer( kind= int_k), intent( in) :: b1, b2, b3

!  int_cvmgz()

continue                                                   ! cvmgz()

   if( b3 == 0_int_k )then
      int_cvmgz = b1

   else
      int_cvmgz = b2
   endif

return                                                     ! cvmgz()

!  int_cvmgz()

end function int_cvmgz

?? endif
?? if( long_k )then
! **********************************************************************

!  long_cvmgz(): cvmgz() for kind int

elemental integer( kind= long_k) function long_cvmgz( b1, b2, b3)

integer( kind= long_k), intent( in) :: b1, b2, b3

!  long_cvmgz()

continue                                                   ! cvmgz()

   if( b3 == 0_long_k )then
      long_cvmgz = b1

   else
      long_cvmgz = b2
   endif

return                                                     ! cvmgz()

!  long_cvmgz()

end function long_cvmgz

?? endif
?? if( single_k )then
! **********************************************************************

!  single_cvmgz(): cvmgz() for kind single

elemental real( kind= single_k) function single_cvmgz( b1, b2, b3)

real( kind= single_k), intent( in) :: b1, b2, b3

!  single_cvmgz()

continue                                                   ! cvmgz()

   if( b3 == 0._single_k )then
      single_cvmgz = b1

   else
      single_cvmgz = b2
   endif

return                                                     ! cvmgz()

!  single_cvmgz()

end function single_cvmgz

?? endif
?? if( double_k )then
! **********************************************************************

!  double_cvmgz(): cvmgz() for kind double

elemental real( kind= double_k) function double_cvmgz( b1, b2, b3)

real( kind= double_k), intent( in) :: b1, b2, b3

!  double_cvmgz()

continue                                                   ! cvmgz()

   if( b3 == 0._double_k )then
      double_cvmgz = b1

   else
      double_cvmgz = b2
   endif

return                                                     ! cvmgz()

!  double_cvmgz()

end function double_cvmgz

?? endif
?? if( quad_k )then
! **********************************************************************

!  quad_cvmgz(): cvmgz() for kind double

elemental real( kind= quad_k) function quad_cvmgz( b1, b2, b3)

real( kind= quad_k), intent( in) :: b1, b2, b3

!  quad_cvmgz()

continue                                                   ! cvmgz()

   if( b3 == 0._quad_k )then
      quad_cvmgz = b1

   else
      quad_cvmgz = b2
   endif

return                                                     ! cvmgz()

!  quad_cvmgz()

end function single_complex_cvmgz

?? endif
?? if( single_k )then
! **********************************************************************

!  single_complex_cvmgz(): cvmgz() for kind single_complex

elemental complex( kind= single_k) function single_complex_cvmgz( b1, b2, b3)

complex( kind= single_k), intent( in) :: b1, b2, b3

!  single_complex_cvmgz()

continue                                                   ! cvmgz()

   if( abs( b3) == 0._single_k )then
      single_complex_cvmgz = b1

   else
      single_complex_cvmgz = b2
   endif

return                                                     ! cvmgz()

!  single_complex_cvmgz()

end function single_complex_cvmgz

?? endif
?? if( double_k )then
! **********************************************************************

!  double_complex_cvmgz(): cvmgz() for kind double_complex

elemental complex( kind= double_k) function double_complex_cvmgz( b1, b2, b3)

complex( kind= double_k), intent( in) :: b1, b2, b3

!  double_complex_cvmgz()

continue                                                   ! cvmgz()

   if( abs( b3) == 0._double_k )then
      double_complex_cvmgz = b1

   else
      double_complex_cvmgz = b2
   endif

return                                                     ! cvmgz()

!  double_complex_cvmgz()

end function quad_complex_cvmgz

?? endif
?? if( quad_k )then
! **********************************************************************

!  quad_complex_cvmgz(): cvmgz() for kind quad_complex

elemental complex( kind= quad_k) function quad_complex_cvmgz( b1, b2, b3)

complex( kind= quad_k), intent( in) :: b1, b2, b3

!  quad_complex_cvmgz()

continue                                                   ! cvmgz()

   if( abs( b3) == 0._quad_k )then
      quad_complex_cvmgz = b1

   else
      quad_complex_cvmgz = b2
   endif

return                                                     ! cvmgz()

!  quad_complex_cvmgz()

end function quad_complex_cvmgz

?? endif
! **********************************************************************

!  cvmgn(): first vector if third is true, second otherwise

?? if( byte_k )then
! **********************************************************************

!  byte_cvmgn(): cvmgn() for kind byte

elemental integer( kind= byte_k) function byte_cvmgn( b1, b2, b3)

integer( kind= byte_k), intent( in) :: b1, b2, b3

!  byte_cvmgn()

continue                                                   ! cvmgn()

   if( b3 /= 0_byte_k )then
      byte_cvmgn = b1

   else
      byte_cvmgn = b2
   endif

return                                                     ! cvmgn()

!  byte_cvmgn()

end function byte_cvmgn

?? endif
?? if( short_k )then
! **********************************************************************

!  short_cvmgn(): cvmgn() for kind short

elemental integer( kind= short_k) function short_cvmgn( b1, b2, b3)

integer( kind= short_k), intent( in) :: b1, b2, b3

!  short_cvmgn()

continue                                                   ! cvmgn()

   if( b3 /= 0_short_k )then
      short_cvmgn = b1

   else
      short_cvmgn = b2
   endif

return                                                     ! cvmgn()

!  short_cvmgn()

end function short_cvmgn

?? endif
?? if( int_k )then
! **********************************************************************

!  int_cvmgn(): cvmgn() for kind int

elemental integer( kind= int_k) function int_cvmgn( b1, b2, b3)

integer( kind= int_k), intent( in) :: b1, b2, b3

!  int_cvmgn()

continue                                                   ! cvmgn()

   if( b3 /= 0_int_k )then
      int_cvmgn = b1

   else
      int_cvmgn = b2
   endif

return                                                     ! cvmgn()

!  int_cvmgn()

end function int_cvmgn

?? endif
?? if( long_k )then
! **********************************************************************

!  long_cvmgn(): cvmgn() for kind int

elemental integer( kind= long_k) function long_cvmgn( b1, b2, b3)

integer( kind= long_k), intent( in) :: b1, b2, b3

!  long_cvmgn()

continue                                                   ! cvmgn()

   if( b3 /= 0_long_k )then
      long_cvmgn = b1

   else
      long_cvmgn = b2
   endif

return                                                     ! cvmgn()

!  long_cvmgn()

end function long_cvmgn

?? endif
?? if( single_k )then
! **********************************************************************

!  single_cvmgn(): cvmgn() for kind single

elemental real( kind= single_k) function single_cvmgn( b1, b2, b3)

real( kind= single_k), intent( in) :: b1, b2, b3

!  cvmgn()

continue                                                   ! cvmgn()

   if( b3 /= 0._single_k )then
      single_cvmgn = b1

   else
      single_cvmgn = b2
   endif

return                                                     ! cvmgn()

!  single_cvmgn()

end function single_cvmgn

?? endif
?? if( double_k )then
! **********************************************************************

!  double_cvmgn(): cvmgn() for kind double

elemental real( kind= double_k) function double_cvmgn( b1, b2, b3)

real( kind= double_k), intent( in) :: b1, b2, b3

!  double_cvmgn()

continue                                                   ! cvmgn()

   if( b3 /= 0._double_k )then
      double_cvmgn = b1

   else
      double_cvmgn = b2
   endif

return                                                     ! cvmgn()

!  double_cvmgn()

end function double_cvmgn

?? endif
?? if( quad_k )then
! **********************************************************************

!  quad_cvmgn(): cvmgn() for kind double

elemental real( kind= quad_k) function quad_cvmgn( b1, b2, b3)

real( kind= quad_k), intent( in) :: b1, b2, b3

!  quad_cvmgn()

continue                                                   ! cvmgn()

   if( b3 /= 0._quad_k )then
      quad_cvmgn = b1

   else
      quad_cvmgn = b2
   endif

return                                                     ! cvmgn()

!  quad_cvmgn()

end function quad_cvmgn

?? endif
?? if( single_k )then
! **********************************************************************

!  single_complex_cvmgn(): cvmgn() for kind single_complex

elemental complex( kind= single_k) function single_complex_cvmgn( b1, b2, b3)

complex( kind= single_k), intent( in) :: b1, b2, b3

!  single_complex_cvmgn()

continue                                                   ! cvmgn()

   if( abs( b3) /= 0._single_k )then
      single_complex_cvmgn = b1

   else
      single_complex_cvmgn = b2
   endif

return                                                     ! cvmgn()

!  single_complex_cvmgn()

end function single_complex_cvmgn

?? endif
?? if( double_k )then
! **********************************************************************

!  double_complex_cvmgn(): cvmgn() for kind double_complex

elemental complex( kind= double_k) function double_complex_cvmgn( b1, b2, b3)

complex( kind= double_k), intent( in) :: b1, b2, b3

!  double_complex_cvmgn()

continue                                                   ! cvmgn()

   if( abs( b3) /= 0._double_k )then
      double_complex_cvmgn = b1

   else
      double_complex_cvmgn = b2
   endif

return                                                     ! cvmgn()

!  double_complex_cvmgn()

end function double_complex_cvmgn

?? endif
?? if( quad_k )then
! **********************************************************************

!  quad_complex_cvmgn(): cvmgn() for kind quad_complex

elemental complex( kind= quad_k) function quad_complex_cvmgn( b1, b2, b3)

complex( kind= quad_k), intent( in) :: b1, b2, b3

!  quad_complex_cvmgn()

continue                                                   ! cvmgn()

   if( abs( b3) /= 0._quad_k )then
      quad_complex_cvmgn = b1

   else
      quad_complex_cvmgn = b2
   endif

return                                                     ! cvmgn()

!  quad_complex_cvmgn()

end function quad_complex_cvmgn

! **********************************************************************

!  parallel_functions

! **********************************************************************
! $Source$

end module parallel_functions                              ! eof
