! bof
! **********************************************************************
! Fortran 95 program processor_model

! **********************************************************************
! Source Control Strings

! $Id: make_pm.f90 2.2 2005/05/30 15:47:01Z Dan Release $

! **********************************************************************
!  Copyright 2004 Purple Sage Computing Solutions, Inc.

! **********************************************************************
! Summary of License

!   This program is free software; you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation; either version 2 of the License, or
!   (at your option) any later version.

!   This program is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.

!   You should have received a copy of the GNU General Public License
!   along with this program; if not, write to the Free Software
!   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

! To report bugs, suggest enhancements, etc. to the Authors,
! Contact:
!    Purple Sage Computing Solutions, Inc.
!                               send email to dnagle@erols.com
!                                   or fax to 703 471 0684 (USA)
!                                  or mail to 12142 Purple Sage Ct.
!                                             Reston, VA 20194-5621 USA

! **********************************************************************
! prints a description of the processor model to output_unit

! **********************************************************************
! use processor_dependencies

! **********************************************************************

!  processor_model uses

!     processor_dependencies describes the processor

!  processor_model reads

!     none

!  processor_model writes

!     output_unit all output

!  processor_model library

!     none

! **********************************************************************

!  processor model

! **********************************************************************

program processor_model

! **********************************************************************

!  use standard kind definitions, &c

use processor_dependencies

! **********************************************************************
!  declare all variables

implicit none

! **********************************************************************

!  processor_model RCS strings

! **********************************************************************

!  program source filename supplied by RCS

character (len= *), parameter :: processor_model_rcs_id = &
   '$Id: make_pm.f90 2.2 2005/05/30 15:47:01Z Dan Release $'

! **********************************************************************

character( len= *), parameter :: &
   processor_model_timestamp = '20111115  091932.178'

! **********************************************************************

!  processor_model data

! **********************************************************************

!  date_and_time() character buffers

   character( len= 8) :: dbuff
   character( len= 10) :: tbuff
   character( len= 5) :: zbuff

!  date_and_time() values array

   integer, dimension( 8) :: val

!  system_clock() arguments

   integer :: im, ir, ic

!  cpu_time() argument

   real :: ct

!  random_number() seed array

   integer, dimension( random_seed_size) :: iseed

!  logical true and false

   logical, parameter :: ltrue = .true.
   logical, parameter :: lfalse = .false.

!  integer true and false

   integer :: itrue, ifalse

! **********************************************************************

!  processor_model text

continue                                                             ! processor_model

! **********************************************************************

!  banner

   write( unit= *, fmt= *)

   write( unit= *, fmt= *) 'Processor Model'

   write( unit= *, fmt= *)

! **********************************************************************

!  version control

   write( unit= *, fmt= *) 'Version Control'

   write( unit= *, fmt= *)

!  report processor_model version strings

   write( unit= *, fmt= *) 'processor_model ', processor_model_rcs_id

   write( unit= *, fmt= *)

   write( unit= *, fmt= *) 'timestamp ', processor_model_timestamp

   write( unit= *, fmt= *)

!  report compiler version strings

   write( unit= *, fmt= *) 'compiler_version ', compiler_version
   write( unit= *, fmt= *) 'compiler_serial ', compiler_serial
   write( unit= *, fmt= *) 'os_version ', os_version
   write( unit= *, fmt= *) 'standard_version ', standard_version

   write( unit= *, fmt= *)

!  report processor_dependencies version strings

   write( unit= *, fmt= *) 'processor_dependencies ', processor_dependencies_rcs_id

   write( unit= *, fmt= *)

! **********************************************************************

!  detect mismatch between processor_dependencies and processor_model

   if( processor_dependencies_rcs_id /= processor_model_rcs_id )then

      write( unit= *, fmt= *) 'processor_dependencies - processor_model mismatch'

      write( unit= *, fmt= *)

   endif

!  detect mismatch between processor_dependencies and processor_model

   if( processor_depends_timestamp /= processor_model_timestamp )then

      write( unit= *, fmt= *) 'processor_dependencies - processor_model mismatch'

      write( unit= *, fmt= *)

   endif

! **********************************************************************

!  processor dependent parameters

   write( unit= *, fmt= *) 'Processor Memory Parameters'

   write( unit= *, fmt= *)

   write( unit= *, fmt= *) 'size of a numeric storage unit', numeric_storage_size

   write( unit= *, fmt= *) 'size of a character storage unit', character_storage_size

   write( unit= *, fmt= *) 'size of a file storage unit', file_storage_size

   if( is_big_endian ) write( unit= *, fmt= *) 'big endian'

   if( is_little_endian ) write( unit= *, fmt= *) 'little endian'

   write( unit= *, fmt= *)

   write( unit= *, fmt= *) 'Processor Software Parameters'

   write( unit= *, fmt= *)

   write( unit= *, fmt= *) 'processor supports Fortran 95 Standard'

   if( has_varying_strings ) write( unit= *, fmt= *) 'processor supports iso_varying_string'

   write( unit= *, fmt= *)

   write( unit= *, fmt= *) 'Processor Input/Output Parameters'

   write( unit= *, fmt= *)

   write( unit= *, fmt= *) 'preconnected input unit', input_unit
   write( unit= *, fmt= *) 'preconnected output unit', output_unit

   if( output_unit == error_unit )then
      write( unit= *, fmt= *) 'no preconnected error unit'
   else
      write( unit= *, fmt= *) 'preconnected error unit', error_unit
   endif

   write( unit= *, fmt= *) 'default filename ', default_filename

   if( opt_plus_sign )then
      write( unit= *, fmt= *) 'writes optional plus sign'
   else
      write( unit= *, fmt= *) 'does not write optional plus sign'
   endif


   if( leading_zero_f )then
      write( unit= *, fmt= *) 'writes leading zero using f format'
   else
      write( unit= *, fmt= *) 'does not write leading zero using f format'
   endif

   if( leading_zero_e )then
      write( unit= *, fmt= *) 'writes leading zero using e format'
   else
      write( unit= *, fmt= *) 'does not write leading zero using f format'
   endif

   write( unit= *, fmt= *) 'smallest decimal exponemt with list-directed f format', ld_fmt_fmin
   write( unit= *, fmt= *) 'largest deciaml exponent with list-directed f format', ld_fmt_fmax
   if( ld_fmt_comma )then
      write( unit= *, fmt= *) 'writes comma using list directed format'
   else
      write( unit= *, fmt= *) 'does not write comma using list directed format'
   endif

   write( unit= *, fmt= *) 'end of record iostat',                   -2
   write( unit= *, fmt= *) 'end of file iostat',                   -1
   write( unit= *, fmt= *) 'direct access read undefined record iostat',                 5002

   write( unit= *, fmt= *) 'size of a file storage unit', file_storage_size
   write( unit= *, fmt= *) 'maximum record length', max_recl
   write( unit= *, fmt= *)

   write( unit= *, fmt= *) 'Miscelleanous Processor Dependent Parameters'

   write( unit= *, fmt= *)

   write( unit= *, fmt= *) 'random seed size', random_seed_size

   call random_seed( get= iseed)
   write( unit= *, fmt= *) 'initial random seed', iseed

   itrue = transfer( ltrue, itrue)
   write( unit= *, fmt= *) 'integer value of true', itrue
   ifalse = transfer( lfalse, ifalse)
   write( unit= *, fmt= *) 'integer value of false', ifalse

   if( is_ieee_fp )then
      write( unit= *, fmt= *) 'fp uses ieee 754 format'
   else
      write( unit= *, fmt= *) 'fp uses non-ieee format'
   endif

   if( is_2s_comp )then
      write( unit= *, fmt= *) 'integers are twos complement'
   else
      write( unit= *, fmt= *) 'integers are not twos complement'
   endif

   write( unit= *, fmt= *)

   write( unit= *, fmt= *) 'Processor Clock'

   write( unit= *, fmt= *)

   call system_clock( ic, ir, im)

   if( ic == -huge( 0) )then
      write( unit= *, fmt= *) 'processor has no clock'
   else
      write( unit= *, fmt= *) 'clock rate', ir
      write( unit= *, fmt= *) 'clock max', im
   endif

   call date_and_time( date= dbuff)

   if( dbuff == ' ' )then
      write( unit= *, fmt= *) 'date not available'
   else
      write( unit= *, fmt= *) 'date ', dbuff
   endif

   call date_and_time( time= tbuff)

   if( tbuff == ' ' )then
      write( unit= *, fmt= *) 'time not available'
   else
      write( unit= *, fmt= *) 'time ', tbuff
   endif

   call date_and_time( zone= zbuff)

   if( zbuff == ' ' )then
      write( unit= *, fmt= *) 'zone not available'
   else
      write( unit= *, fmt= *) 'zone ', zbuff
   endif

   call date_and_time( values= val)

   if( val( 1) == -huge( 0) )then
      write( unit= *, fmt= *) 'value year not available'
   else
      write( unit= *, fmt= *) 'value year', val( 1)
   endif

   if( val( 2) == -huge( 0) )then
      write( unit= *, fmt= *) 'value month not available'
   else
      write( unit= *, fmt= *) 'value month', val( 2)
   endif

   if( val( 3) == -huge( 0) )then
      write( unit= *, fmt= *) 'value day not available'
   else
      write( unit= *, fmt= *) 'value day', val( 3)
   endif

   if( val( 4) == -huge( 0) )then
      write( unit= *, fmt= *) 'value utc not available'
   else
      write( unit= *, fmt= *) 'value utc', val( 4)
   endif

   if( val( 5) == -huge( 0) )then
      write( unit= *, fmt= *) 'value hour not available'
   else
      write( unit= *, fmt= *) 'value hour', val( 5)
   endif

   if( val( 6) == -huge( 0) )then
      write( unit= *, fmt= *) 'value minute not available'
   else
      write( unit= *, fmt= *) 'value minute', val( 6)
   endif

   if( val( 7) == -huge( 0) )then
      write( unit= *, fmt= *) 'value second not available'
   else
      write( unit= *, fmt= *) 'value second', val( 7)
   endif

   if( val( 8) == -huge( 0) )then
      write( unit= *, fmt= *) 'value msec not available'
   else
      write( unit= *, fmt= *) 'value msec', val( 8)
   endif

   call cpu_time( ct)
   if( ct >= 0.0 )then
      write( unit= *, fmt= *) 'has cpu time'
   else
      write( unit= *, fmt= *) 'no cpu time'
   endif

   write( unit= *, fmt= *)

! **********************************************************************

!  report kinds summary

   write( unit= *, fmt= *) 'Kinds Summary'
   write( unit= *, fmt= *)

   write( unit= *, fmt= *) 'character kinds ',                    1
   write( unit= *, fmt= *) 'integer kinds ',                    5
   write( unit= *, fmt= *) 'logical kinds ',                    4
   write( unit= *, fmt= *) 'real kinds ',                    4

   write( unit= *, fmt= *)

   write( unit= *, fmt= *) 'default character ', kind( ' ')
   write( unit= *, fmt= *) 'default integer ', kind( 0)
   write( unit= *, fmt= *) 'default logical ', kind( .true.)
   write( unit= *, fmt= *) 'default real ', kind( 0.0)

   write( unit= *, fmt= *)

! **********************************************************************

!  report character kinds

   write( unit= *, fmt= *) 'Character Kinds'
   write( unit= *, fmt= *)

   if( ascii_k > 0 )then
      write( unit= *, fmt= *) 'ascii character'

      write( unit= *, fmt= *) 'kind ', ascii_k
      write( unit= *, fmt= *) 'parameter ascii_k'
      write( unit= *, fmt= *) 'bit_size ', bit_size( ascii_k_' ')

      write( unit= *, fmt= *) 'huge', ichar( huge( ascii_k_' ') )
      write( unit= *, fmt= *) 'tiny', ichar( tiny( ascii_k_' ') )

      write( unit= *, fmt= *)
   else
      write( unit= *, fmt= *) 'no ascii character'

      write( unit= *, fmt= *)
   endif

   if( ebcdic_k > 0 )then
      write( unit= *, fmt= *) 'ebcdic character'

      write( unit= *, fmt= *) 'kind ', ebcdic_k

      write( unit= *, fmt= *)
   else
      write( unit= *, fmt= *) 'no ebcdic character'

      write( unit= *, fmt= *)
   endif

   if( iso_10646_k > 0 )then
      write( unit= *, fmt= *) 'iso_10646 character'

      write( unit= *, fmt= *) 'kind ', iso_10646_k

      write( unit= *, fmt= *)
   else
      write( unit= *, fmt= *) 'no iso_10646 character'

      write( unit= *, fmt= *)
   endif

! **********************************************************************

!  report integer kinds

   write( unit= *, fmt= *) 'Integer Kinds'
   write( unit= *, fmt= *)

   if( byte_k > 0 )then
      write( unit= *, fmt= *) 'byte integer'

      write( unit= *, fmt= *) 'kind ', byte_k
      write( unit= *, fmt= *) 'parameter byte_k'
      write( unit= *, fmt= *) 'bit_size ', bit_size( 0_byte_k)

      write( unit= *, fmt= *) 'huge ', huge( 0_byte_k)
      write( unit= *, fmt= *) 'range ', range( 0_byte_k)

      write( unit= *, fmt= *) 'digits ', digits( 0_byte_k)
      write( unit= *, fmt= *) 'radix ', radix( 0_byte_k)

      write( unit= *, fmt= *)
   else
      write( unit= *, fmt= *) 'no byte integer'

      write( unit= *, fmt= *)
   endif

   if( short_k > 0 )then
      write( unit= *, fmt= *) 'short integer'

      write( unit= *, fmt= *) 'kind ', short_k
      write( unit= *, fmt= *) 'parameter short_k'
      write( unit= *, fmt= *) 'bit_size ', bit_size( 0_short_k)

      write( unit= *, fmt= *) 'huge ', huge( 0_short_k)
      write( unit= *, fmt= *) 'range ', range( 0_short_k)

      write( unit= *, fmt= *) 'digits ', digits( 0_short_k)
      write( unit= *, fmt= *) 'radix ', radix( 0_short_k)

      write( unit= *, fmt= *)
   else
      write( unit= *, fmt= *) 'no short integer'

      write( unit= *, fmt= *)
   endif

   if( int_k > 0 )then
      write( unit= *, fmt= *) 'int integer'

      write( unit= *, fmt= *) 'kind ', int_k
      write( unit= *, fmt= *) 'parameter int_k'
      write( unit= *, fmt= *) 'bit_size ', bit_size( 0_int_k)

      write( unit= *, fmt= *) 'huge ', huge( 0_int_k)
      write( unit= *, fmt= *) 'range ', range( 0_int_k)

      write( unit= *, fmt= *) 'digits ', digits( 0_int_k)
      write( unit= *, fmt= *) 'radix ', radix( 0_int_k)

      write( unit= *, fmt= *)
   else
      write( unit= *, fmt= *) 'no int integer'

      write( unit= *, fmt= *)
   endif

   if( long_k > 0 )then
      write( unit= *, fmt= *) 'long integer'

      write( unit= *, fmt= *) 'kind ', long_k
      write( unit= *, fmt= *) 'parameter long_k'
      write( unit= *, fmt= *) 'bit_size ', bit_size( 0_long_k)

      write( unit= *, fmt= *) 'huge ', huge( 0_long_k)
      write( unit= *, fmt= *) 'range ', range( 0_long_k)

      write( unit= *, fmt= *) 'digits ', digits( 0_long_k)
      write( unit= *, fmt= *) 'radix ', radix( 0_long_k)

      write( unit= *, fmt= *)
   else
      write( unit= *, fmt= *) 'no long integer'

      write( unit= *, fmt= *)
   endif

! **********************************************************************

!  report logical kinds

   write( unit= *, fmt= *) 'Logical Kinds'
   write( unit= *, fmt= *)

   if( l_byte_k > 0 )then
      write( unit= *, fmt= *) 'byte logical'

      write( unit= *, fmt= *) 'kind ', l_byte_k
      write( unit= *, fmt= *) 'parameter l_byte_k'
      write( unit= *, fmt= *) 'bit_size ', bit_size( .true._l_byte_k)

      write( unit= *, fmt= *)
   else
      write( unit= *, fmt= *) 'no byte logical'

      write( unit= *, fmt= *)
   endif

   if( l_short_k > 0 )then
      write( unit= *, fmt= *) 'short logical'

      write( unit= *, fmt= *) 'kind ', l_short_k
      write( unit= *, fmt= *) 'parameter l_short_k'
      write( unit= *, fmt= *) 'bit_size ', bit_size( .true._l_short_k)

      write( unit= *, fmt= *)
   else
      write( unit= *, fmt= *) 'no short logical'

      write( unit= *, fmt= *)
   endif

   if( l_int_k > 0 )then
      write( unit= *, fmt= *) 'int logical'

      write( unit= *, fmt= *) 'kind ', l_int_k
      write( unit= *, fmt= *) 'parameter l_int_k'
      write( unit= *, fmt= *) 'bit_size ', bit_size( .true._l_int_k)

      write( unit= *, fmt= *)
   else
      write( unit= *, fmt= *) 'no int logical'

      write( unit= *, fmt= *)
   endif

   if( l_long_k > 0 )then
      write( unit= *, fmt= *) 'long logical'

      write( unit= *, fmt= *) 'kind ', l_long_k
      write( unit= *, fmt= *) 'parameter l_long_k'
      write( unit= *, fmt= *) 'bit_size ', bit_size( .true._l_long_k)

      write( unit= *, fmt= *)
   else
      write( unit= *, fmt= *) 'no long logical'

      write( unit= *, fmt= *)
   endif

! **********************************************************************

!  report real kinds

   write( unit= *, fmt= *) 'Real Kinds'
   write( unit= *, fmt= *)

   if( single_k > 0 )then
      write( unit= *, fmt= *) 'single real'

      write( unit= *, fmt= *) 'kind ', single_k
      write( unit= *, fmt= *) 'parameter single_k'
      write( unit= *, fmt= *) 'bit_size ', bit_size( 1.0_single_k)

      write( unit= *, fmt= *) 'huge ', huge( 1.0_single_k)
      write( unit= *, fmt= *) 'tiny ', tiny( 1.0_single_k)
      write( unit= *, fmt= *) 'max_exact_int ', max_exact_int( 1.0_single_k)

      write( unit= *, fmt= *) 'minexponent ', minexponent( 1.0_single_k)
      write( unit= *, fmt= *) 'maxexponent ', maxexponent( 1.0_single_k)

      write( unit= *, fmt= *) 'range ', range( 1.0_single_k)

      write( unit= *, fmt= *) 'digits ', digits( 1.0_single_k)
      write( unit= *, fmt= *) 'radix ', radix( 1.0_single_k)

      write( unit= *, fmt= *) 'exponent ', exponent( 1.0_single_k)
      write( unit= *, fmt= *) 'fraction ', fraction( 1.0_single_k)

      write( unit= *, fmt= *) 'epsilon ', epsilon( 1.0_single_k)
      write( unit= *, fmt= *) 'precision ', precision( 1.0_single_k)

      write( unit= *, fmt= *) 'nearest ', nearest( 1.0_single_k, 2.0_single_k)
      write( unit= *, fmt= *) 'rrspacing ', rrspacing( 1.0_single_k)
      write( unit= *, fmt= *) 'spacing ', spacing( 1.0_single_k)

      write( unit= *, fmt= *)
   else
      write( unit= *, fmt= *) 'no single real'

      write( unit= *, fmt= *)
   endif

   if( double_k > 0 )then
      write( unit= *, fmt= *) 'double real'

      write( unit= *, fmt= *) 'kind ', double_k
      write( unit= *, fmt= *) 'parameter double_k'
      write( unit= *, fmt= *) 'bit_size ', bit_size( 1.0_double_k)

      write( unit= *, fmt= *) 'huge ', huge( 1.0_double_k)
      write( unit= *, fmt= *) 'tiny ', tiny( 1.0_double_k)
      write( unit= *, fmt= *) 'max_exact_int ', max_exact_int( 1.0_double_k)

      write( unit= *, fmt= *) 'minexponent ', minexponent( 1.0_double_k)
      write( unit= *, fmt= *) 'maxexponent ', maxexponent( 1.0_double_k)

      write( unit= *, fmt= *) 'range ', range( 1.0_double_k)

      write( unit= *, fmt= *) 'digits ', digits( 1.0_double_k)
      write( unit= *, fmt= *) 'radix ', radix( 1.0_double_k)

      write( unit= *, fmt= *) 'exponent ', exponent( 1.0_double_k)
      write( unit= *, fmt= *) 'fraction ', fraction( 1.0_double_k)

      write( unit= *, fmt= *) 'epsilon ', epsilon( 1.0_double_k)
      write( unit= *, fmt= *) 'precision ', precision( 1.0_double_k)

      write( unit= *, fmt= *) 'nearest ', nearest( 1.0_double_k, 2.0_double_k)
      write( unit= *, fmt= *) 'rrspacing ', rrspacing( 1.0_double_k)
      write( unit= *, fmt= *) 'spacing ', spacing( 1.0_double_k)

      write( unit= *, fmt= *)
   else
      write( unit= *, fmt= *) 'no double real'

      write( unit= *, fmt= *)
   endif

   if( quad_k > 0 )then
      write( unit= *, fmt= *) 'quad real'

      write( unit= *, fmt= *) 'kind ', quad_k
      write( unit= *, fmt= *) 'parameter quad_k'
      write( unit= *, fmt= *) 'bit_size ', bit_size( 1.0_quad_k)

      write( unit= *, fmt= *) 'huge ', huge( 1.0_quad_k)
      write( unit= *, fmt= *) 'tiny ', tiny( 1.0_quad_k)

      write( unit= *, fmt= *) 'minexponent ', minexponent( 1.0_quad_k)
      write( unit= *, fmt= *) 'maxexponent ', maxexponent( 1.0_quad_k)
      write( unit= *, fmt= *) 'max_exact_int ', max_exact_int( 1.0_quad_k)

      write( unit= *, fmt= *) 'range ', range( 1.0_quad_k)

      write( unit= *, fmt= *) 'digits ', digits( 1.0_quad_k)
      write( unit= *, fmt= *) 'radix ', radix( 1.0_quad_k)

      write( unit= *, fmt= *) 'exponent ', exponent( 1.0_quad_k)
      write( unit= *, fmt= *) 'fraction ', fraction( 1.0_quad_k)

      write( unit= *, fmt= *) 'epsilon ', epsilon( 1.0_quad_k)
      write( unit= *, fmt= *) 'precision ', precision( 1.0_quad_k)

      write( unit= *, fmt= *) 'nearest ', nearest( 1.0_quad_k, 2.0_quad_k)
      write( unit= *, fmt= *) 'rrspacing ', rrspacing( 1.0_quad_k)
      write( unit= *, fmt= *) 'spacing ', spacing( 1.0_quad_k)

      write( unit= *, fmt= *)
   else
      write( unit= *, fmt= *) 'no quad real'

      write( unit= *, fmt= *)
   endif

! **********************************************************************

!  report complex kinds

   write( unit= *, fmt= *) 'Complex Kinds'
   write( unit= *, fmt= *)

   if( single_k > 0 )then
      write( unit= *, fmt= *) 'single complex'

      write( unit= *, fmt= *) 'kind ', single_k
      write( unit= *, fmt= *) 'parameter single_k'
      write( unit= *, fmt= *) 'bit_size ', bit_size( cmplx( 0.0, 0.0, kind= single_k) )

      write( unit= *, fmt= *) 'epsilon ', epsilon( cmplx( 0.0, 0.0, kind= single_k) )
      write( unit= *, fmt= *) 'huge ', huge( cmplx( 0.0, 0.0, kind= single_k) )
      write( unit= *, fmt= *) 'tiny ', tiny( cmplx( 0.0, 0.0, kind= single_k) )

      write( unit= *, fmt= *)
   else
      write( unit= *, fmt= *) 'no single complex'

      write( unit= *, fmt= *)
   endif

   if( double_k > 0 )then
      write( unit= *, fmt= *) 'double complex'

      write( unit= *, fmt= *) 'kind ', double_k
      write( unit= *, fmt= *) 'parameter double_k'
      write( unit= *, fmt= *) 'bit_size ', bit_size( cmplx( 0.0, 0.0, kind= double_k) )

      write( unit= *, fmt= *) 'epsilon ', epsilon( cmplx( 0.0, 0.0, kind= double_k) )
      write( unit= *, fmt= *) 'huge ', huge( cmplx( 0.0, 0.0, kind= double_k) )
      write( unit= *, fmt= *) 'tiny ', tiny( cmplx( 0.0, 0.0, kind= double_k) )

      write( unit= *, fmt= *)
   else
      write( unit= *, fmt= *) 'no double complex'

      write( unit= *, fmt= *)
   endif

   if( quad_k > 0 )then
      write( unit= *, fmt= *) 'quad complex'

      write( unit= *, fmt= *) 'kind ', quad_k
      write( unit= *, fmt= *) 'parameter quad_k'
      write( unit= *, fmt= *) 'bit_size ', bit_size( cmplx( 0.0, 0.0, kind= quad_k) )

      write( unit= *, fmt= *) 'epsilon ', epsilon( cmplx( 0.0, 0.0, kind= quad_k) )
      write( unit= *, fmt= *) 'huge ', huge( cmplx( 0.0, 0.0, kind= quad_k) )
      write( unit= *, fmt= *) 'tiny ', tiny( cmplx( 0.0, 0.0, kind= quad_k) )

      write( unit= *, fmt= *)
   else
      write( unit= *, fmt= *) 'no quad complex'

      write( unit= *, fmt= *)
   endif

stop 'processor_model'                                               ! processor_model

! **********************************************************************

! processor_model

! $Id: make_pm.f90 2.2 2005/05/30 15:47:01Z Dan Release $
! **********************************************************************

end program processor_model                                          ! eof
