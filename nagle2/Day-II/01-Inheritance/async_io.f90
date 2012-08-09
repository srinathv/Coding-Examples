! bof
! **********************************************************************
! Fortran 2003 program async_io

! **********************************************************************
! Source Control Strings

! $Id: async_io.f03 1.2 2005/03/24 17:26:19Z Dan Release $

! **********************************************************************
!  Copyright 2005 Purple Sage Computing Solutions, Inc.
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

! To report bugs, suggest enhancements, etc. to the Authors,
! Contact:
!    Purple Sage Computing Solutions, Inc.
!                               send email to dnagle@erols.com
!                                   or fax to 703 425-0983 (USA)
!                                  or mail to 10483 Malone Ct.
!                                             Fairfax, VA 22032 USA

! **********************************************************************
! async_io describe the program

! **********************************************************************

!  async_io uses

!     stdandard_types- describes the processor

!  async_io includes

!     <none>

!  async_io reads

!  async_io writes

!  async_io constants

!  async_io types

!  async_io data

!  async_io library

! **********************************************************************

!  async_io

! **********************************************************************

program async_io

! **********************************************************************

!  async_io uses modules

use f03_standard_definitions, only: is_status_error, is_status_info

! **********************************************************************

!  processor description

use processor_dependencies

! **********************************************************************

!  turn off implicit typing

implicit none                                                        ! explicit declarations

! **********************************************************************

!  async_io RCS strings

! **********************************************************************

!  program source filename supplied by RCS

character( len= *), parameter :: async_io_rcs_id = &
   '$Id: async_io.f03 1.2 2005/03/24 17:26:19Z Dan Release $'

! **********************************************************************

!  async_io constants

! **********************************************************************

integer, parameter :: input = 41, output = 42                        ! logical units

integer, parameter :: nmax = 2000                                    ! a large size

! **********************************************************************

!  async_io types

! **********************************************************************

!  no types

! **********************************************************************

!  async_io data

! **********************************************************************

   integer :: io_status                                              ! check iostat

   real, dimension( nmax, nmax), asynchronous, target :: input_buffer_1
   real, dimension( nmax, nmax), asynchronous, target :: input_buffer_2
   real, dimension( nmax, nmax), asynchronous, target :: output_buffer_1
   real, dimension( nmax, nmax), asynchronous, target :: output_buffer_2

   real, dimension( :, :), asynchronous, pointer :: input_calc_ptr, input_io_ptr
   real, dimension( :, :), asynchronous, pointer :: output_calc_ptr, output_io_ptr

! **********************************************************************

!  async_io text

! **********************************************************************

continue                                                             ! async_io

!  this program uses double buffering of input and output
!  to demonstrate asynchronous input/output techniques

!  there must be one read to start the input process
!  before the main loop

!  there must be one write to finish the output process
!  after the main loop

!  the main strategy is:

!  read input

!  process input to make output

!  write output

! **********************************************************************

!  open the input file and check for success

   open( unit= input, file= 'async.in', form= 'UNFORMATTED', status= 'OLD', asynchronous= 'YES', iostat= io_status)

   check_open_in_stat: if( is_status_error( io_status) )then

      stop "can't open async.in"

   endif check_open_in_stat

!  open the output file and check for success

   open( unit= output, file= 'async.out', form= 'UNFORMATTED', status= 'REPLACE', asynchronous= 'YES', iostat= io_status)

   check_open_out_stat: if( is_status_error( io_status) )then

      stop "can't open async.out"

   endif check_open_out_stat

! ----------------------------------------------------------------------

!  assign pointers

   input_calc_ptr => input_buffer_1                                  ! first time process input_buffer_1
   input_io_ptr => input_buffer_2                                    ! first time read input_buffer_2

   output_calc_ptr => output_buffer_1                                ! first time compute output_buffer_1
   output_io_ptr => output_buffer_1                                  ! output_buffer_1 will be ready first

!  read the first block

   read( unit= input, iostat= io_status) input_calc_ptr              ! initial read

   check_read_stat: if( is_status_error( io_status) )then

      stop "can't read async.in"

   endif check_read_stat

!  start to read the next block

   read( unit= input, asynchronous= 'YES', iostat= io_status) input_io_ptr

   check_readprime_stat: if( is_status_error( io_status) )then

      stop "can't asynch read async.in"

   endif check_readprime_stat

!  calculate the first output buffer

   call big_calculation( input_calc_ptr, output_calc_ptr)

! ----------------------------------------------------------------------

!  do forever

   pipeline: do                                                      ! exit upon end of file

!  write next block

      write( unit= output, asynchronous= 'YES', iostat= io_status) output_io_ptr

      check_writea_stat: if( is_status_error( io_status) )then

         stop "can't asynch write async.out"

      endif check_writea_stat

!  ensure previousd read has completed

      wait( unit= input, iostat= io_status)

      check_readwait_stat: if( is_status_error( io_status) )then

         stop "can't wait async.in"

      elseif( is_status_info( io_status) )then check_readwait_stat   ! wait might return eof

         exit pipeline

      endif check_readwait_stat

!  swap input pointers

      swap_in_ptr: if( associated( input_calc_ptr, input_buffer_1) )then

         input_calc_ptr => input_buffer_2
         input_io_ptr => input_buffer_1

      else swap_in_ptr

         input_calc_ptr => input_buffer_1
         input_io_ptr => input_buffer_2

      endif swap_in_ptr

!  read next block

      read( unit= input, asynchronous= 'YES', iostat= io_status) input_io_ptr

      check_reada_stat: if( is_status_error( io_status) )then

         stop "can't asynch read async.in"

      elseif( is_status_info( io_status) )then check_reada_stat      ! read might return eof immediately

         exit pipeline

      endif check_reada_stat

!  calculate

      call big_calculation( input_calc_ptr, output_calc_ptr)

!  ensure previousd write has completed

      wait( unit= output, iostat= io_status)

      check_writewait_stat: if( is_status_error( io_status) )then

         stop "can't wait async.out"

      endif check_writewait_stat

!  swap output pointers

      swap_out_ptr: if( associated( output_calc_ptr, output_buffer_1) )then

         output_calc_ptr => output_buffer_2
         output_io_ptr => output_buffer_1

      else swap_out_ptr

         output_calc_ptr => output_buffer_1
         output_io_ptr => output_buffer_2

      endif swap_out_ptr

!  end do forever

   enddo pipeline                                                    ! exit upon end of file

! ----------------------------------------------------------------------

!  write last block

   write( unit= input, iostat= io_status) output_io_ptr

   check_write_stat: if( is_status_error( io_status) )then

      stop "can't write async.out"

   endif check_write_stat

! ----------------------------------------------------------------------

!  close the input file and check for success

   close( unit= input, status= 'KEEP', iostat= io_status)

   check_close_in_stat: if( is_status_error( io_status) )then

      stop "can't close async.in"

   endif check_close_in_stat

!  close the output file and check for success

   close( unit= output, status= 'KEEP', iostat= io_status)

   check_close_out_stat: if( is_status_error( io_status) )then

      stop "can't close async.out"

   endif check_close_out_stat

! ----------------------------------------------------------------------

!  and done

stop 'async_io'                                                      ! async_io

! **********************************************************************

!  async_io library

! **********************************************************************

contains                                                             ! async_io

! **********************************************************************

!  big_calculation( in_array, out_array) is the supposed work

subroutine big_calculation( in_array, out_array)

real, dimension( :, :), intent( in) :: in_array
real, dimension( :, :), intent( out) :: out_array

continue

   out_array = in_array                                              ! or something

return

end subroutine big_calculation

!  async_io

! $Id: async_io.f03 1.2 2005/03/24 17:26:19Z Dan Release $
! **********************************************************************

end program async_io                                                 ! eof
