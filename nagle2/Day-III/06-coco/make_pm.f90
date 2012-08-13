! bof
! **********************************************************************
! Fortran 95 program make_processor_model

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
!                                  or mail to 10483 Malone Ct.
!                                             Fairfax, VA 22032 USA

! **********************************************************************
! writes a processor_dependencies module and a processor_model program

! **********************************************************************
! use none

! **********************************************************************

!  make_processor_model uses

!     <none>

!  make_processor_model includes

!     <none>

!  make_processor_model reads

!     make_pm.in: an optional namelist file of quantities beyond diagnosis

!  make_processor_model writes

!     make_pm.log: logfile of make_processor_model actions
!     coco.inc | f90ppr.inc | fpp.inc: optional file of preprocessor definitions
!     procdep.f90: fortran 95 source code module processor_dependencies
!     pm.f90: fortran 95 source code to display processor model

!  make_processor_model library

!     read_input_file() reads the processor description file, if present
!     seek_namelist_group() tries to find a namelist group in the input file
!     seek_integer_kinds() seeks integer kinds
!     seek_real_kinds() seeks real kinds
!     check_character_kinds() checks user specified character kinds
!     check_logical_kinds() checks user specified logical kinds
!     check_hardware_values() experiments to verify claims about the hardware
!     is_power_of_2() returns true if its positive argument is a power of two
!     check_ieee_single() experiments to verify ieee single format
!     check_ieee_double() experiments to verify ieee double format
!     check_ieee_single_extended() experiments to verify ieee single extended format
!     check_ieee_double_extended() experiments to verify ieee double extended format
!     diagnose_input_output() experiments on input/output processor dependencies
!     write_bit_size() writes a bit_size() using the Method of Olagnon
!     write_include_file() write preprocessor include file
!     write_processor_dependencies() writes the processor_dependencies module
!     write_processor_model() writes the processor_model program

! **********************************************************************

!  make_processor_model

! **********************************************************************

program make_processor_model

! **********************************************************************

!  use no module

! **********************************************************************

!  declare all variable names

implicit none                                                        ! no implicit declarations

! **********************************************************************

!  make_processor_model RCS strings

! **********************************************************************

!  program identifier string supplied by RCS

character( len= *), parameter :: make_pm_rcs_id = &
   '$Id: make_pm.f90 2.2 2005/05/30 15:47:01Z Dan Release $'

! **********************************************************************

!  make_processor_model stop character codes

! **********************************************************************

!  program normal exit

character( len= *), parameter :: normal_stop_code = &
   'make_processor_model: complete'

!  program error exit

character( len= *), parameter :: error_stop_code = &
   'make_processor_model: not complete'

! **********************************************************************

!  make_processor_model logical units

! **********************************************************************

!  make_pm.log unit

integer, parameter :: log_unit = 10                                  ! write logfile

!  make_pm.in unit

integer, parameter :: nml_unit = 11                                  ! read namelist

!  procdep.f90 unit

integer, parameter :: pd_unit = 12                                   ! write module source

!  pm.f90 unit

integer, parameter :: pm_unit = 13                                   ! write program source

!  preprocessor include file

integer, parameter :: inc_unit = 14                                  ! write include file

! ----------------------------------------------------------------------

!  unit to use to guess the name of a file opened without a name

integer, parameter :: fn_unit = 3                                    ! logical unit number
character( len= *), parameter :: ch_fn_unit = '3'                    ! search for this character

!  unit for direct access experiments

integer, parameter :: da_unit = fn_unit + 1                          ! use another unit

! **********************************************************************

!  make_processor_model formats

! **********************************************************************

!  format for fortran source output

character( len= *), parameter :: fmtpr = '( a)'                      ! string

!  format for default kind values (non-advancing i/o)

character( len= *), parameter :: fmti = '( 1x, i20)'                 ! + integer

!  formats for fortran source output with single integer

character( len= *), parameter :: fmtpri = '( a, 1x, i20)'            ! string + integer

!  format for fortran source output with single digit integer & closing paren

character( len= *), parameter :: fmtprip = '( a, 1x, i5, ")")'       ! string + integer + ")"

!  format for fortran source output with 2 integers & closing paren

character( len= *), parameter :: fmtpriip = &
                                 '( a, 1x, i5, ", ", i5, ")")'       ! string + integer + "," + integer + ")"

! **********************************************************************

!  make_processor_model constants

! **********************************************************************

!  character buffer length reserved for file names

integer, parameter :: name_len = 1024                                ! hopefully long enough

!  character component length reserved for kind names

integer, parameter :: kind_len = 64                                  ! must be long enough

! ----------------------------------------------------------------------

!  this can't be changed because it's the name of the file containing the namelists

character( len= *), parameter :: rc_name = 'make_pm.in'              ! nml_unit

!  this can't be changed because it's opened before make_pm.in is read

character( len= *), parameter :: logname = 'make_pm.log'             ! log_unit

! ----------------------------------------------------------------------

!  ascii change case

integer, parameter :: change_case = 32                               ! compare lower case

! ----------------------------------------------------------------------

!  denotes a kind not supported

integer, parameter :: not_supported = -1                             ! internal code

! **********************************************************************

!  codes for various standards

integer, parameter :: no_std = 0                                     ! any standard pre-f90 (no modules)
integer, parameter :: f90_std = no_std + 1                           ! f90
integer, parameter :: f95_std = f90_std + 1                          ! f95
integer, parameter :: f03_std = f95_std + 1                          ! f03
integer, parameter :: f08_std = f03_std + 1                          ! f08

! **********************************************************************

!  codes for various preprocessors

integer, parameter :: no_inc = 0                                     ! no preprocessor file
integer, parameter :: coco_inc = no_inc + 1                          ! coco.inc
integer, parameter :: f90ppr_inc = coco_inc + 1                      ! f90ppr.inc
integer, parameter :: fpp_inc = f90ppr_inc + 1                       ! fpp.inc

character( len= *), parameter :: coco_name = 'coco.inc'              ! coco include filename
character( len= *), parameter :: f90ppr_name = 'f90ppr.inc'          ! f90ppr include filename
character( len= *), parameter :: fpp_name = 'fpp.inc'                ! fpp include filename

! **********************************************************************

!  complaints, messages, etc.

character( len= *), parameter :: cant_open = "can't open "           ! announce failure

character( len= *), parameter :: write_ln = '      write( unit= *, fmt= *)'

character( len= *), parameter :: star_banner = &
   '! **********************************************************************'

character( len= *), parameter :: wa_str = '(words)'                  ! units for message

character( len= *), parameter :: ba_str = '(bytes)'                  ! units for message

! **********************************************************************

!  default number of (named) kinds

integer, parameter :: default_number_of_integers = 4                 ! byte, short, int, long

integer, parameter :: default_number_of_reals = 3                    ! single, double, quad

! **********************************************************************

!  make_processor_model() types

! **********************************************************************

!  an integer kind

type :: integer_kind_t                                               ! describe an integer kind

   character( len= kind_len) :: kind_name                            ! the kinds name (less the '_k')

   integer :: kind_value                                             ! integers have kind values
   integer :: max_digits                                             ! most digits this kind supports
   integer :: integer_bit_size                                       ! estimated size in bits

   logical :: supported                                              ! kind is byte, short, int, long, or int<bits>
   logical :: default_kind                                           ! one kind is the default

end type integer_kind_t                                              ! describe an integer kind

! **********************************************************************

!  a real kind (& therefore a complex kind)

type :: real_kind_t                                                  ! describe an real kind

   character( len= kind_len) :: kind_name                            ! the kinds name (less the '_k')

   integer :: kind_value                                             ! reals have kind values
   integer :: max_precision                                          ! reals support a precision
   integer :: max_range                                              ! reals support a range
   integer :: real_bit_size                                          ! estimated size in bits

   logical :: supported                                              ! kind is one of single, double, quad
   logical :: default_kind                                           ! one kind is the default real
   logical :: default_dp_kind                                        ! one kind is the default double precision

end type real_kind_t                                                 ! describe an real kind

! **********************************************************************

!  a logical kind

type :: logical_kind_t                                               ! describe a logical kind

   character( len= kind_len) :: kind_name                            ! the kinds name (less the '_k')

   integer :: kind_value                                             ! logicals have kind values

   logical :: supported                                              ! kind is one of single, double, quad
   logical :: default_kind                                           ! one kind is the default

end type logical_kind_t                                              ! describe a logical kind

! **********************************************************************

!  a character kind

type :: character_kind_t                                             ! describe a character kind

   character( len= kind_len) :: kind_name                            ! the kinds name (less the '_k')

   integer :: kind_value                                             ! characters have kind values

   logical :: supported                                              ! kind is one of single, double, quad
   logical :: default_kind                                           ! one kind is the default

   character( len= name_len) :: inquiry_string                       ! argument to selected_char_kind()

end type character_kind_t                                            ! describe a character kind

! **********************************************************************

!  make_processor_model data

! **********************************************************************

!  configuration data

! **********************************************************************

!  standard code designates the standard supported by the processor

integer :: standard = no_std                                         ! set via namelist string

character( len= *), parameter :: f90_str = 'Fortran 90'
character( len= *), parameter :: f95_str = 'Fortran 95'
character( len= *), parameter :: f03_str = 'Fortran 2003'
character( len= *), parameter :: f08_str = 'Fortran 2008'

! **********************************************************************

!  preprocessor code designates the preprocessor, if any, of the include file

integer :: ppr_inc = no_inc                                          ! set via namelist string

! **********************************************************************

!  status date and time strings

integer, parameter :: dt_len = 10                                    ! long enough for date string
integer, parameter :: tm_len = 10                                    ! long enough for time string

character( len= dt_len) :: run_date = ' '                            ! date for logfile
character( len= tm_len) :: run_time = ' '                            ! time for logfile

integer, parameter :: ts_len = dt_len + tm_len                       ! long enough for both

character( len= ts_len) :: timestamp                                 ! timestamp pd & pm

! **********************************************************************

!  data which are computed by make_processor_model

! **********************************************************************

!  measured hardware values

logical :: storage_size_is_2n                                        ! true when all storage units appear to be 2^n

integer :: measured_word_size                                        ! from bit_size()

integer :: measured_char_size                                        ! from size-transfer

integer :: measured_byte_size                                        ! from size-transfer

integer, parameter :: ua_len = max( len( ba_str), len( wa_str))

character( len= ua_len) :: ua_str

! **********************************************************************

!  describes the integer kinds found

integer :: number_of_integers = 0                                    ! from count_integer_kinds()

type( integer_kind_t), allocatable, dimension( :) :: integer_kinds   ! array of integers found

!  indexes to integer kind array

integer :: byte_idx = 1
integer :: short_idx = 2
integer :: int_idx = 3
integer :: long_idx = 4                                              ! from seek_integer_kinds()

!  describes the real kinds found

integer :: number_of_reals = 0                                       ! from count_real_kinds()

type( real_kind_t), allocatable, dimension( :) :: real_kinds         ! array of kinds found

!  indexes to real kind array

integer :: single_idx = 1
integer :: double_idx = 2
integer :: quad_idx = 3                                              ! from seek_real_kinds()

! **********************************************************************

!  values obtained via I/O experiments

! ----------------------------------------------------------------------

!  maximum record length

integer :: mrecl                                                     ! from inquire()

!  name of a file opened without a name

character( len= name_len) :: def_fn                                  ! from inquire()

!  iostat end-of-record and end-of-file codes

integer :: eor_flag, eof_flag                                        ! from read()

!  flags from leading zero experiments

logical :: lz_f_flag, lz_e_flag                                      ! from write()

!  flag from leading plus experiments

logical :: plus_flag                                                 ! from write()

!  see if list directed format uses separator ( comma or dot)

logical :: has_ld_sep                                                ! from write()

!  values of list-directed format experiments

integer :: ld_min, ld_max                                            ! from write()

!  iostat values when trying to read missing records within direct access files

integer :: da_missing, da_eof                                        ! from read()

! ----------------------------------------------------------------------

!  big_endian or little_endian

logical :: measured_big_endian                                       ! true if big endian

! **********************************************************************

!  data which must be read from the input file

! **********************************************************************

!  logical kinds read from input file

type( logical_kind_t) :: byte_logical                                ! logical kinds
type( logical_kind_t) :: short_logical
type( logical_kind_t) :: int_logical
type( logical_kind_t) :: long_logical

logical :: define_logicals                                           ! true if defining logical kinds

! ----------------------------------------------------------------------

!  character kinds read from input file

type( character_kind_t) :: ascii_character                           ! ascii characters
type( character_kind_t) :: ebcdic_character                          ! ebcdic characters
type( character_kind_t) :: iso_10646_character                       ! iso_10646 characters

logical :: define_characters                                         ! true if defining character kinds

! ----------------------------------------------------------------------

!  mark type/kind not to be investigated

logical :: want_ib                                                   ! allow user to defeat any mention of integer byte
logical :: want_is                                                   ! allow user to defeat any mention of integer short
logical :: want_ii                                                   ! allow user to defeat any mention of integer int
logical :: want_il                                                   ! allow user to defeat any mention of integer long

logical :: want_rs                                                   ! allow user to defeat any mention of real single
logical :: want_rd                                                   ! allow user to defeat any mention of real double
logical :: want_rq                                                   ! allow user to defeat any mention of real quad

! **********************************************************************

!  the namelist input file

! **********************************************************************

!  namelists to be read from make_pm.in

!  note that the values specified here are the defaults

! ----------------------------------------------------------------------

!  namelist of file names

character( len= name_len) :: pdname = 'procdep.f90'                  ! module processor_dependencies

character( len= name_len) :: pmname = 'pm.f90'                       ! program processor_model

character( len= name_len) :: incname = ''                            ! preprocessor include file ( none by default)

namelist /files/ pdname, pmname, incname                             ! file names

! ----------------------------------------------------------------------

!  namelist of hardware variables

logical :: ieeefp = .true.                                           ! ieee 754 format

logical :: twoscomp = .true.                                         ! 2's complement integers

integer :: bytesize = 0                                              ! default is diagnosed byte size

integer :: wordsize = 0                                              ! if > 0, word addressable

namelist /hw/ ieeefp, twoscomp, bytesize, wordsize                   ! hardware

! ----------------------------------------------------------------------

!  namelist of software variables

integer :: stdin = 5                                                 ! unit= * for input

integer :: stdout = 6                                                ! unit= * for output

integer :: stderr = -1                                               ! no preconnected error unit

character( len= 3) :: std = 'f95'                                    ! f90, f95 or f03 standard

integer, parameter :: vers_len = 80                                  ! length of version strings

character( len= vers_len) :: com_vers = ''                           ! Example: 'Acme Fortran 90 v 1.0a'
character( len= vers_len) :: com_sern = 'Compiler S/N'               ! Example: 'S/N: 007'

character( len= vers_len) :: os_vers = 'OS Version'                  ! Example: 'Ajax Z80 Unix v 19.5.e'

namelist /sw/ stdin, stdout, stderr, &                               ! preconnected units
              std, com_vers, com_sern, os_vers                       ! versions

! ----------------------------------------------------------------------

!  namelist of kinds variables for kinds which can't be diagnosed

integer :: ascii = 1                                                 ! default character is ascii

integer :: ebcdic = not_supported                                    ! no ebcdic

integer :: iso_10646 = not_supported                                 ! no iso_10646

logical :: strings = .false.                                         ! true if ISO Varying String module is available

logical :: logeqint = .true.                                         ! make logicals same kinds as integers

integer :: logbyte = not_supported                                   ! off by default

integer :: logshort = not_supported                                  ! off by default

integer :: logint = not_supported                                    ! off by default

integer :: loglong = not_supported                                   ! off by default

namelist /kinds/ ascii, ebcdic, iso_10646, strings, &                ! character kinds
                 logeqint, logbyte, logshort, logint, loglong        ! logical kinds

! ----------------------------------------------------------------------

!  namelist of floating point parameters ( xp= x's precision, xr= x's range, x= { single | double | quad})

logical :: autoreal = .true.                                         ! attempt automatic kind detection

integer :: sp = 6, sr = 37                                           ! an ieee 32 bit ( single)

integer :: dp = 15, dr = 307                                         ! an ieee 64 bit ( double)

integer :: qp = 33, qr = 4931                                        ! an 'ieee' 128 bit ( quad)

namelist /float/ autoreal, &                                         ! search for reals
                 sp, sr, dp, dr, qp, qr                              ! floating point parameters

! ----------------------------------------------------------------------

!  namelist of integer parameters ( xd= x's digits, x= { byte | short | int | long})

logical :: autoint = .true.                                          ! attempt automatic kind detection

integer :: bd = 2, sd = 4, id = 9, ld = 18                           ! integer digits for byte, short, int, long

namelist /fixed/ autoint, &                                          ! search for integers
                 bd, sd, id, ld                                      ! integer parameters

! **********************************************************************

!  make_processor_model local

! **********************************************************************

!  status flag from subroutines

   integer :: istat                                                  ! ok = 0, error > 0, something < 0

!  index of change case loops

   integer :: char_ptr                                               ! loop through strings

!  count numbers of various kinds

   integer :: count_kinds                                            ! number of kinds etc detected

! **********************************************************************

!  make_processor_model text

! **********************************************************************

continue                                                             ! make_processor_model

! **********************************************************************

!  open logfile or quit

   open( unit= log_unit, file= logname, status= 'REPLACE', &         ! attempt open
         action= 'WRITE', iostat= istat)

!  if can't open the logfile, complain to stdout and quit

   open_error: if( istat > 0 )then                                   ! if error with open

      write( unit= *, fmt= fmtpr) 'ERROR: ' // cant_open // logname  ! add filename to complaint

      stop error_stop_code                                           ! and quit

   endif open_error                                                  ! if error with open

!  write banner in logfile

   write( unit= log_unit, fmt= fmtpr) 'make_processor_model'         ! program name
   write( unit= log_unit, fmt= fmtpr) make_pm_rcs_id                 ! program version

!  write date/time in logfile

   call date_and_time( date= run_date, time= run_time)               ! time of this run

   timestamp = run_date // run_time                                  ! combine

   write( unit= log_unit, fmt= fmtpr) 'timestamp: ' // timestamp     ! log timestamp

! **********************************************************************

!  try to read namelist input file

   write( unit= log_unit, fmt= fmtpr) 'reading ' // rc_name          ! log attempt

   call read_input_file( istat)                                      ! read file 'make_pm.in'

!  check results of attempt to read input

   read_input_file_status: if( istat > 0 )then                       ! if error reading make_pm.in

      write( unit= log_unit, fmt= fmtpr) 'ERROR: trouble reading ' // rc_name

      stop error_stop_code                                           ! quit

   elseif( istat < 0 )then read_input_file_status                    ! no file named 'make_pm.in'

      write( unit= log_unit, fmt= fmtpr) 'no ' // rc_name // ' found'

   else read_input_file_status                                       ! read file 'make_pm.in'

      write( unit= log_unit, fmt= fmtpr) 'read ' // rc_name // ' ok'

   endif read_input_file_status                                      ! handle return code from read_input_file()

! ----------------------------------------------------------------------

!  search for only those kinds requested

   want_ib = bd > 0                                                  ! seek byte integers ?
   want_is = sd > 0                                                  ! seek short integers ?
   want_ii = id > 0                                                  ! seek int integers ?
   want_il = ld > 0                                                  ! seek long integers ?

   want_rs = sp > 0                                                  ! seek single reals ?
   want_rd = dp > 0                                                  ! seek double reals ?
   want_rq = qp > 0                                                  ! seek quad reals ?

! **********************************************************************

!  write processor_dependencies and processor_model for f90 or f95 or f03

! **********************************************************************

!  make character comaprisons in lower case only

   each_character_std: do char_ptr = 1, len( std)                    ! loop thru first to last

      to_lower_case_std: select case( std( char_ptr: char_ptr))      ! examine each character

      case( 'A': 'Z') to_lower_case_std                              ! upper case to lower case

         std( char_ptr: char_ptr) = achar( iachar( std( char_ptr: char_ptr)) + change_case)

      end select to_lower_case_std                                   ! ignore other characters

   enddo each_character_std                                          ! examine each character

! ----------------------------------------------------------------------

!  check std is 'f90' or 'f95' or 'f03' or 'f08'

   which_standard: select case( std)                                 ! convert std to integer code

!  standard supported is Fortran 95

   case( 'f95') which_standard                                       ! write for f95

      write( unit= log_unit, fmt= fmtpr) 'processor supports Fortran 95 standard'

      standard = f95_std

!  set default compiler version string if it's null

      f95_version: if( com_vers == '' )then                          ! f95 default string

         com_vers = f95_str

      endif f95_version                                              ! f95 default string

!  standard supported is Fortran 90

   case( 'f90') which_standard                                       ! write for f90

      write( unit= log_unit, fmt= fmtpr) 'processor supports Fortran 90 standard'

      standard = f90_std                                             ! support f90 standard

!  set default compiler version string if it's null

      f90_version: if( com_vers == '' )then                          ! f90 default string

         com_vers = f90_str

      endif f90_version                                              ! f90 default string

!  standard supported is Fortran 2003

   case( 'f03') which_standard                                       ! write for f03

      write( unit= log_unit, fmt= fmtpr) 'processor supports Fortran 2003 standard'

      standard = f03_std                                             ! support f03 standard

!  set default compiler version string if it's null

      f03_version: if( com_vers == '' )then                          ! f03 default string

         com_vers = f03_str

      endif f03_version                                              ! f03 default string

!  standard supported is Fortran 2003

   case( 'f08') which_standard                                       ! write for f03

      write( unit= log_unit, fmt= fmtpr) 'processor supports Fortran 2008 standard'

      standard = f08_std                                             ! support f08 standard

!  set default compiler version string if it's null

      f08_version: if( com_vers == '' )then                          ! f08 default string

         com_vers = f08_str

      endif f08_version                                              ! f08 default string

!  no earlier standard is supported ( must have modules)

   case default which_standard                                       ! must be f90 or f95 or f03

      write( unit= log_unit, fmt= fmtpr) 'ERROR: std = ' &
             // trim( std) // ': std must = f90, f95, f03 or f08'    ! must have modules!

      stop error_stop_code                                           ! quit

   end select which_standard                                         ! check f90 or f95 or f03

! **********************************************************************

!  write preprocessor include file for coco or f90ppr or fpp or none

! **********************************************************************

!  make character comaprisons in lower case only

   each_character_inc: do char_ptr = 1, len( incname)                ! loop thru first to last

      to_lower_case: select case( incname( char_ptr: char_ptr))      ! examine each character

      case( 'A': 'Z') to_lower_case                                  ! upper case to lower case

         incname( char_ptr: char_ptr) = achar( iachar( incname( char_ptr: char_ptr)) + change_case)

      end select to_lower_case                                       ! ignore other characters

   enddo each_character_inc                                          ! examine each character

! ----------------------------------------------------------------------

!  check preprocessor is coco or f90ppr or fpp

   select_preprocessor: select case( incname)                        ! which preprocessor

!  preprocessor is coco ( Part 3 of the Standard)

   case( coco_name) select_preprocessor                              ! write for coco

      write( unit= log_unit, fmt= fmtpr) 'include file for coco'

      ppr_inc = coco_inc

!  preprocessor is f90ppr ( Moware)

   case( f90ppr_name) select_preprocessor                            ! write for f90ppr

      write( unit= log_unit, fmt= fmtpr) 'include file for f90ppr'

      ppr_inc = f90ppr_inc                                           ! f90ppr preprocessor

!  preprocessor is fpp/cpp or ...

   case( fpp_name) select_preprocessor                               ! write for fpp/cpp

      write( unit= log_unit, fmt= fmtpr) 'include file for fpp/cpp'

      ppr_inc = fpp_inc                                              ! fpp/cpp preprocessor

!  only formats supported

   case default select_preprocessor                                  ! must be coco or f90ppr or fpp

!  so other names are errors

      null_string_ok: if( len_trim( incname) > 0 )then               ! null string means no preprocessor file

         write( unit= log_unit, fmt= fmtpr) 'ERROR: incname = ' &
                // trim( incname) // ': incname must = coco.inc, f90ppr.inc or fpp.inc'

         stop error_stop_code                                        ! quit

!  no name is the default

      else null_string_ok                                            ! null string means no preprocessor file

         write( unit= log_unit, fmt= fmtpr) 'no preprocessor include file'

      endif null_string_ok                                           ! null string means no preprocessor file

   end select select_preprocessor                                    ! which preprocessor

! ----------------------------------------------------------------------

!  log iso_varying_strings

   has_varying_strings: if( strings )then                            ! report strings

      write( unit= log_unit, fmt= fmtpr) 'processor supports iso_varying_strings'

   else has_varying_strings                                          ! report strings

      write( unit= log_unit, fmt= fmtpr) 'processor does not support iso_varying_strings'

   endif has_varying_strings                                         ! report strings

! **********************************************************************

!  check hardware parameters before doing anything else

   call check_hardware_values                                        ! check user input

! **********************************************************************

!  get kinds via selected_<what>_kind() or make_pm.in

! **********************************************************************

!  determine integer kinds ---------------------------------------------

! **********************************************************************

!  seek integer kinds if requested

   detect_ints: if( autoint )then                                    ! want automatic search

      call count_integer_kinds                                       ! find out how many integer kinds

   endif detect_ints                                                 ! want automatic search

   number_of_integers = max( number_of_integers, default_number_of_integers)

   allocate( integer_kinds( number_of_integers), stat= istat)        ! array of integer kinds

   int_alloc_error: if( istat > 0 )then

      write( unit=log_unit, fmt= fmtpr) 'ERROR: trouble allocating integer kind array'

      stop error_stop_code                                           ! quit
         
   endif int_alloc_error

!  name string, kind value, number of digits, bit size, is supported, is the default kind

   integer_kinds( byte_idx) = integer_kind_t( 'byte', 0, bd, 0, .false., .false.)
   integer_kinds( short_idx) = integer_kind_t( 'short', 0, sd, 0, .false., .false.)
   integer_kinds( int_idx) = integer_kind_t( 'int', 0, id, 0, .false., .false.)
   integer_kinds( long_idx) = integer_kind_t( 'long', 0, ld, 0, .false., .false.)

!  automatically find integer kinds

   find_ints: if( autoint )then                                      ! want automatic search

      call seek_integer_kinds                                        ! values of array of integer kinds

   endif find_ints                                                   ! want automatic search

! **********************************************************************

!  detect integer kinds

! **********************************************************************

!  processor kind values

   integer_kinds( byte_idx)% kind_value = selected_int_kind( bd)     ! byte kind
   integer_kinds( short_idx)% kind_value = selected_int_kind( sd)    ! short kind
   integer_kinds( int_idx)% kind_value = selected_int_kind( id)      ! int kind
   integer_kinds( long_idx)% kind_value = selected_int_kind( ld)     ! long kind

!  byte is supported if it exists and is not the same as short and the user wants it

   integer_kinds( byte_idx)% supported = ( integer_kinds( byte_idx)% kind_value > 0) &
      .and. ( integer_kinds( byte_idx)% kind_value /= integer_kinds( short_idx)% kind_value) &
      .and. want_ib

!  short is supported if it exists and is not the same as int and the user wants it

   integer_kinds( short_idx)% supported = ( integer_kinds( short_idx)% kind_value > 0) &
      .and. ( integer_kinds( short_idx)% kind_value /= integer_kinds( int_idx)% kind_value) &
      .and. want_is

!  int is supported if it exists and is not the same as long and the user wants it

   integer_kinds( int_idx)% supported = ( integer_kinds( int_idx)% kind_value > 0) &
      .and. ( integer_kinds( int_idx)% kind_value /= integer_kinds( long_idx)% kind_value) &
      .and. want_ii

!  long is supported if it exists and the user wants it

   integer_kinds( long_idx)% supported = ( integer_kinds( long_idx)% kind_value > 0) &
      .and. want_il

! ----------------------------------------------------------------------

!  write integer kinds in logfile

   write( unit= log_unit, fmt= fmtpr, advance= 'NO') 'assigned integer kinds:'

!  processor has bytes   

   has_byte: if( integer_kinds( byte_idx)% supported )then

      write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' byte'

   endif has_byte
   
!  processor has shorts

   has_short: if( integer_kinds( short_idx)% supported )then

      write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' short'

   endif has_short

!  processor has ints

   has_int: if( integer_kinds( int_idx)% supported )then

      write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' int'

   endif has_int

!  processor has longs

   has_long: if( integer_kinds( long_idx)% supported )then

      write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' long'

   endif has_long

   write( unit= log_unit, fmt= *)                                    ! end of record

! ----------------------------------------------------------------------

!  complain if there isn't at least one integer kind detected

   count_kinds = count( integer_kinds% supported)                    ! count supported integer kinds

   count_integers: if( count_kinds < 1 )then                         ! report bad count

      write( unit= log_unit, fmt= fmtpr) 'WARNING: no integer kinds detected'

   endif count_integers                                              ! report bad count

! ----------------------------------------------------------------------

!  detect default integer kind

   integer_kinds( byte_idx)% default_kind = integer_kinds( byte_idx)% supported &
      .and. integer_kinds( byte_idx)% kind_value == kind( 0)         ! default is byte

   integer_kinds( short_idx)% default_kind = integer_kinds( short_idx)% supported &
      .and. integer_kinds( short_idx)% kind_value == kind( 0)        ! default is short

   integer_kinds( int_idx)% default_kind = integer_kinds( int_idx)% supported &
      .and. integer_kinds( int_idx)% kind_value == kind( 0)          ! default is int

   integer_kinds( long_idx)% default_kind = integer_kinds( long_idx)% supported &
      .and. integer_kinds( long_idx)% kind_value == kind( 0)         ! default is long

!  report default integer kind

   write( unit= log_unit, fmt= fmtpr, advance= 'NO') 'default integer kind:'

!  default is byte

   def_integer: if( integer_kinds( byte_idx)% default_kind )then     ! default integer

      write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' byte'

!  default is short

   elseif( integer_kinds( short_idx)% default_kind )then def_integer ! default integer

      write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' short'

!  default is int

   elseif( integer_kinds( int_idx)% default_kind )then def_integer   ! default integer

      write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' int'

!  default is long

   elseif( integer_kinds( long_idx)% default_kind )then def_integer  ! default integer

      write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' long'

!  default is not found

   else def_integer                                                  ! no default integer

      write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' WARNING: default integer not detected!'

   endif def_integer                                                 ! default integer

   write( unit= log_unit, fmt= *)                                    ! end of record

! ----------------------------------------------------------------------

!  complain if there isn't exactly one default integer kind detected

   count_kinds = count( integer_kinds% default_kind)                 ! count default integer kinds

   count_def_integers: if( count_kinds < 1 )then                     ! complain if too few

      write( unit= log_unit, fmt= fmtpr) 'WARNING: default integer kind not detected!'

   elseif( count_kinds > 1 )then count_def_integers                  ! complain if too few

      write( unit= log_unit, fmt= fmtpr) 'WARNING: default integer kind not unique!'

   endif count_def_integers                                          ! possible complaints

! **********************************************************************

!  determine real kinds ------------------------------------------------

! **********************************************************************

!  seek real kinds if requested

   detect_reals: if( autoreal )then                                  ! want automatic search

      call count_real_kinds                                          ! find out how many real kinds

   endif detect_reals                                                ! want automatic search

   number_of_reals = max( default_number_of_reals, number_of_reals)

   allocate( real_kinds( number_of_reals), stat= istat)

   real_alloc_error: if( istat > 0 )then

      write( unit=log_unit, fmt= fmtpr) 'ERROR: trouble allocating real kind array'

      stop error_stop_code                                           ! quit
         
   endif real_alloc_error

!  name string, kind value, precision, range, bit size, is supported, is the default kind, is the default double

   real_kinds( single_idx) = &
      real_kind_t( 'single', 0, sp, sr, 0, .false., .false., .false.)
   real_kinds( double_idx) = &
      real_kind_t( 'double', 0, dp, dr, 0, .false., .false., .false.)
   real_kinds( quad_idx) = &
      real_kind_t( 'quad', 0, qp, qr, 0, .false., .false., .false.)

!  automatically find real kinds

   find_reals: if( autoreal )then                                    ! want automatic search

      call seek_real_kinds                                           ! try to detect real kinds

   endif find_reals                                                  ! want automatic search

! **********************************************************************

!  detect real kinds

! **********************************************************************

!  get kind values

   real_kinds( single_idx)% kind_value = selected_real_kind( sp, sr) ! single kind
   real_kinds( double_idx)% kind_value = selected_real_kind( dp, dr) ! double kind
   real_kinds( quad_idx)% kind_value = selected_real_kind( qp, qr)   ! quad kind

!  single is supported if it exists and is no the same as double and the user wants it

   real_kinds( single_idx)% supported = ( real_kinds( single_idx)% kind_value > 0) &
      .and. ( real_kinds( single_idx)% kind_value /= real_kinds( double_idx)% kind_value) &
      .and. want_rs

!  double is supported if it exists and is no the same as quad and the user wants it

   real_kinds( double_idx)% supported = ( real_kinds( double_idx)% kind_value > 0) &
      .and. ( real_kinds( double_idx)% kind_value /= real_kinds( quad_idx)% kind_value) &
      .and. want_rd

!  quad is supported if it exists and the user wants it

   real_kinds( quad_idx)% supported = real_kinds( quad_idx)% kind_value > 0 &
      .and. want_rq

! ----------------------------------------------------------------------

!  log real kinds

   write( unit= log_unit, fmt= fmtpr, advance= 'NO') 'assigned real kinds:'

   has_single: if( real_kinds( single_idx)% supported )then

      write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' single'

   endif has_single

   has_double: if( real_kinds( double_idx)% supported )then

      write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' double'

   endif has_double

   has_quad: if( real_kinds( quad_idx)% supported )then

      write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' quad'

   endif has_quad

   write( unit= log_unit, fmt= *)                                    ! end of record

! ----------------------------------------------------------------------

!  detect default real kind

   real_kinds( single_idx)% default_kind = real_kinds( single_idx)% supported &
      .and. real_kinds( single_idx)% kind_value == kind( 0.0)        ! default is single

   real_kinds( double_idx)% default_kind = real_kinds( double_idx)% supported &
      .and. real_kinds( double_idx)% kind_value == kind( 0.0)        ! default is double

   real_kinds( quad_idx)% default_kind = real_kinds( quad_idx)% supported &
      .and. real_kinds( quad_idx)% kind_value == kind( 0.0)          ! default is quad

!  report default real kind

   write( unit= log_unit, fmt= fmtpr, advance= 'NO') 'default real kind:'

   def_real: if( real_kinds( single_idx)% default_kind )then         ! default real

      write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' single'

   elseif( real_kinds( double_idx)% default_kind )then def_real      ! default real

      write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' double'

   elseif( real_kinds( quad_idx)% default_kind )then def_real        ! default real

      write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' quad'

   else def_real                                                     ! no default real

      write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' WARNING: default real not detected!'

   endif def_real                                                    ! default real

   write( unit= log_unit, fmt= *)                                    ! end of record

!  single is the default double precision kind if it exists and has the kind of 0.0d0

   real_kinds( single_idx)% default_dp_kind = real_kinds( single_idx)% supported &
      .and. real_kinds( single_idx)% kind_value == kind( 0.0d0)      ! double precision is single

!  double is the default double precision kind if it exists and has the kind of 0.0d0

   real_kinds( double_idx)% default_dp_kind = real_kinds( double_idx)% supported &
      .and. real_kinds( double_idx)% kind_value == kind( 0.0d0)      ! double precision is double

!  quad is the default double precision kind if it exists and has the kind of 0.0d0

   real_kinds( quad_idx)% default_dp_kind = real_kinds( quad_idx)% supported &
      .and. real_kinds( quad_idx)% kind_value == kind( 0.0d0)        ! double precision is quad

! ----------------------------------------------------------------------

!  report the double precision kind

   write( unit= log_unit, fmt= fmtpr, advance= 'NO') 'double precision kind:'

   def_dp: if( real_kinds( single_idx)% default_dp_kind )then        ! double precision

      write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' single'

   elseif( real_kinds( double_idx)% default_dp_kind )then def_dp     ! double precision

      write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' double'

   elseif( real_kinds( quad_idx)% default_dp_kind )then def_dp       ! double precision

      write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' quad'

   else def_dp                                                       ! no double precision

      write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' WARNING: double precision not detected!'

   endif def_dp                                                      ! double precision

   write( unit= log_unit, fmt= *)                                    ! end of record

! ----------------------------------------------------------------------

!  complain if there aren't at least two real kinds detected

   count_reals: if( count( real_kinds% supported) < 2 )then          ! count supported real kinds

      write( unit= log_unit, fmt= fmtpr) 'WARNING: fewer than 2 real kinds detected'

   endif count_reals                                                 ! count supported real kinds

! ----------------------------------------------------------------------

!  complain if there aren't exactly two default real kinds detected

   count_kinds = count( real_kinds% default_kind)                    ! count supported default real kinds

   count_def_reals: if( count_kinds < 1 )then                        ! complain if too few

      write( unit= log_unit, fmt= fmtpr) 'WARNING: default real kind not detected'

   elseif( count_kinds > 1 )then count_def_reals                     ! complain if too many

      write( unit= log_unit, fmt= fmtpr) 'WARNING: default real kind not unique'

   endif count_def_reals                                             ! possible complaints

   count_kinds = count( real_kinds% default_dp_kind)                 ! count supported default real kinds

   count_def_dp_reals: if( count_kinds < 1 )then                     ! complain if too few

      write( unit= log_unit, fmt= fmtpr) 'WARNING: default double kind not detected'

   elseif( count_kinds > 1 )then count_def_dp_reals                  ! complain if too many

      write( unit= log_unit, fmt= fmtpr) 'WARNING: default double kind not unique'

   endif count_def_dp_reals                                          ! possible complaints

! **********************************************************************

!  check correspondence between default real and double precision kinds

   s_dp_real: if( kind( 0.0) == kind( 0.0d0) )then                   ! single real <-> dp real

      write( unit= log_unit, fmt= fmtpr) 'default real kind same value as double precision real kind'

   endif s_dp_real                                                   ! single real <-> dp real

!  try to guess correspondence between real and integer kinds

   int_real: if( kind( 0) == kind( 0.0) )then                        ! default integer <-> default real

      write( unit= log_unit, fmt= fmtpr) 'default integer kind same value as default real kind'

   elseif( kind( 0) == kind( 0.0d0) )then int_real                   ! default integer <-> default dp

      write( unit= log_unit, fmt= fmtpr) 'default integer kind same value as double precision kind'

   else int_real

      write( unit= log_unit, fmt= fmtpr) 'NOTE: integer kind <-> real kind correspondence not detected!'

   endif int_real                                                    ! default integer <-> default real

! **********************************************************************

!  process character kinds from input valiables

! **********************************************************************

!  name string, kind value, is supported, is the default, string passed to selected_char_kind()

! ----------------------------------------------------------------------

!  ascii characters

   ascii_character = character_kind_t( 'ascii', max( ascii, not_supported), ascii > 0, .false., 'ASCII')

!  ebcdic characters

   ebcdic_character = character_kind_t( 'ebcdic', max( ebcdic, not_supported), ebcdic > 0, .false., 'EBCDIC')

!  iso_10646 characters

   iso_10646_character = character_kind_t( 'iso_10646', max( iso_10646, not_supported), iso_10646 > 0, .false., &
                                           'ISO_10646')

!  found any character kinds

   define_characters = ascii_character% supported .or. ebcdic_character% supported &
                     .or. iso_10646_character% supported

! ----------------------------------------------------------------------

!  log character kinds

   write( unit= log_unit, fmt= fmtpr, advance= 'NO') 'defined character kinds:'

   want_char: if( define_characters )then                            ! if characters defined

      define_ascii: if( ascii_character% supported )then

         write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' ascii'

      endif define_ascii

      define_ebcdic: if( ebcdic_character% supported )then

         write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' ebcdic'

      endif define_ebcdic

      define_iso_10646: if( iso_10646_character% supported )then

         write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' iso_10646'

      endif define_iso_10646

   else want_char                                                    ! if characters defined

      write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' none'

   endif want_char                                                   ! if characters defined

   write( unit= log_unit, fmt= *)                                    ! end of record

! ----------------------------------------------------------------------

!  default character kind

   write( unit= log_unit, fmt= fmtpr, advance= 'NO') 'default character kind:'

   default_char: if( define_characters )then                         ! default character

      ascii_character% default_kind = ascii_character% supported .and. ascii_character% kind_value == kind( ' ')
      ebcdic_character% default_kind = ebcdic_character% supported &
                                  .and. ebcdic_character% kind_value == kind( ' ')
      iso_10646_character% default_kind = iso_10646_character% supported &
                                    .and. iso_10646_character% kind_value == kind ( ' ')

      default_ascii: if( ascii_character% default_kind )then         ! default is ascii

         write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' ascii'

      endif default_ascii                                            ! default is ascii

      default_ebcdic: if( ebcdic_character% default_kind )then       ! default is ebcdic

         write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' ebcdic'

      endif default_ebcdic                                           ! default is ebcdic

      default_iso_10646: if( iso_10646_character% default_kind )then ! default is iso 10646

         write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' iso_10646'

      endif default_iso_10646                                        ! default is iso 10646

   else default_char                                                 ! default character

      write( unit= log_unit, fmt= fmti, advance= 'NO') kind( ' ')    ! no name so print value

   endif default_char                                                ! default character

   write( unit= log_unit, fmt= *)                                    ! end of record

! ----------------------------------------------------------------------

!  character kinds sanity check

   define_char: if( define_characters )then                          ! if defined character kinds

      call check_character_kinds                                     ! check user input

   endif define_char                                                 ! if defined character kinds

! **********************************************************************

!  process logical kinds from input variables

! **********************************************************************

!  name string, kind value, is supported, is the default

! ----------------------------------------------------------------------

!  if logical kinds correspond to integer kinds

   log_eq_int: if( logeqint )then                                    ! make logicals same as integers

!  byte kind

      same_byte: if( integer_kinds( byte_idx)% supported )then

         byte_logical = logical_kind_t( 'l_byte', integer_kinds( byte_idx)% kind_value, .true., .false.)

      else same_byte

         byte_logical = logical_kind_t( '', not_supported, .false., .false.)

      endif same_byte

!  short kind

      same_short: if( integer_kinds( short_idx)% supported )then

         short_logical = logical_kind_t( 'l_short', integer_kinds( short_idx)% kind_value, .true., .false.)

      else same_short

         short_logical = logical_kind_t( '', not_supported, .false., .false.)

      endif same_short

!  int kind

      same_int: if( integer_kinds( int_idx)% supported )then

         int_logical = logical_kind_t( 'l_int', integer_kinds( int_idx)% kind_value, .true., .false.)

      else same_int

         int_logical = logical_kind_t( '', not_supported, .false., .false.)

      endif same_int

!  long kind

      same_long: if( integer_kinds( long_idx)% supported )then

         long_logical = logical_kind_t( 'l_long', integer_kinds( long_idx)% kind_value, .true., .false.)

      else same_long

         long_logical = logical_kind_t( '', not_supported, .false., .false.)

      endif same_long

!  logical kinds specified individually

   else log_eq_int

!  byte kind

      want_bl: if( logbyte > 0 )then

         byte_logical = logical_kind_t( 'l_byte', max( logbyte, not_supported), .true., .false.)

      else want_bl

         byte_logical = logical_kind_t( '', max( logbyte, not_supported), .false., .false.)

      endif want_bl

!  short kind

      want_sl: if( logshort > 0 )then

         short_logical = logical_kind_t( 'l_short', max( logshort, not_supported), .true., .false.)

      else want_sl

         short_logical = logical_kind_t( '', max( logshort, not_supported), .false., .false.)

      endif want_sl

!  int kind

      want_intl: if( logint > 0 )then

         int_logical = logical_kind_t( 'l_int', max( logint, not_supported), .true., .false.)

      else want_intl

         int_logical = logical_kind_t( '', max( logint, not_supported), .false., .false.)

      endif want_intl

 !  long kind

      want_ll: if( loglong > 0 )then

         long_logical = logical_kind_t( 'l_long', max( loglong, not_supported), .true., .false.)

      else want_ll

         long_logical = logical_kind_t( '', max( loglong, not_supported), .false., .false.)

      endif want_ll

   endif log_eq_int                                                  ! make logicals same as integers

! ----------------------------------------------------------------------

!  ensure unique logical kind values

   byte_logical% supported = byte_logical% kind_value > 0 &
      .and. byte_logical% kind_value /= short_logical% kind_value

   short_logical% supported = short_logical% kind_value > 0 &
      .and. short_logical% kind_value /= int_logical% kind_value

   int_logical% supported = int_logical% kind_value > 0 &
      .and. int_logical% kind_value /= long_logical% kind_value

   long_logical% supported = long_logical% kind_value > 0

!  found any logical kinds

   define_logicals = byte_logical% supported .or. short_logical% supported &
                     .or. int_logical% supported .or. long_logical% supported

! ----------------------------------------------------------------------

!  log logical kinds

   write( unit= log_unit, fmt= fmtpr, advance= 'NO') 'defined logical kinds:'

   report_logicals: if( define_logicals )then                        ! if logicals defined

      report_logical_byte: if( byte_logical% supported )then

         write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' byte'

      endif report_logical_byte

      report_logical_short: if( short_logical% supported )then

         write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' short'

      endif report_logical_short

      report_logical_int: if( int_logical% supported )then

         write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' int'

      endif report_logical_int

      report_logical_long: if( long_logical% supported )then

         write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' long'

      endif report_logical_long

   else report_logicals                                              ! if logicals defined

      write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' none'

   endif report_logicals                                             ! if logicals defined

   write( unit= log_unit, fmt= *)                                    ! end of record

! ----------------------------------------------------------------------

!  default logical kind

   byte_logical% default_kind = byte_logical% supported &
      .and. byte_logical% kind_value == kind( .true.)                ! default logical is byte

   short_logical% default_kind = short_logical% supported &
      .and. short_logical% kind_value == kind( .true.)               ! default logical is short

   int_logical% default_kind = int_logical% supported &
      .and. int_logical% kind_value == kind( .true.)                 ! default logical is int

   long_logical% default_kind = long_logical% supported &
      .and. long_logical% kind_value == kind( .true.)                ! default logical is long

   write( unit= log_unit, fmt= fmtpr, advance= 'NO') 'default logical kind:'

   report_default_logical: if( define_logicals )then                 ! if logicals defined

      report_default_logical_byte: if( byte_logical% default_kind )then

         write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' byte'

      endif report_default_logical_byte

      report_default_logical_short: if( short_logical% default_kind )then

         write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' short'

      endif report_default_logical_short

      report_default_logical_int: if( int_logical% default_kind )then

         write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' int'

      endif report_default_logical_int

      report_default_logical_long: if( long_logical% default_kind )then

         write( unit= log_unit, fmt= fmtpr, advance= 'NO') ' long'

      endif report_default_logical_long

   else report_default_logical                                       ! if logicals defined

      write( unit= log_unit, fmt= fmti, advance= 'NO') kind( .true.) ! no name so print value

   endif report_default_logical                                      ! if logicals defined

   write( unit= log_unit, fmt= *)                                    ! end of record

! ----------------------------------------------------------------------

!  sanity check and promotion of logical kinds

   check_logicals: if( define_logicals )then                         ! defined logical kinds

      call check_logical_kinds                                       ! check user input

   endif check_logicals                                              ! defined logical kinds

! **********************************************************************

!  try to diagnose (some) i/o parameters

   call diagnose_input_output                                        ! basic io tests

! **********************************************************************

!  write the defines include file

! **********************************************************************

!  write which kinds are defined to preprocessor include file

   got_incfile: select case( ppr_inc)                                ! inc file was named

!  no include file was requested

   case( no_inc) got_incfile                                         ! inc file was named

      write( unit= log_unit, fmt= fmtpr) 'no preprocessor include file written'

!  an include file was requested

   case default got_incfile                                          ! inc file was named

      write( unit= log_unit, fmt= fmtpr) 'writing ' // trim( incname)

      call write_include_file( istat)                                ! write module processor_dependencies

      write_include_file_error: if( istat > 0 )then                  ! if error writing stdtype.f90

         write( unit= log_unit, fmt= fmtpr) 'ERROR: trouble writing ' // trim( incname)

         stop error_stop_code                                        ! quit

      else write_include_file_error                                  ! trouble closing

         write( unit= log_unit, fmt= fmtpr) 'wrote ' // trim( incname) // ' ok'

      endif write_include_file_error                                 ! if error writing stdtype.f90

   end select got_incfile                                            ! inc file was named

! **********************************************************************

!  write the Fortran source for processor_dependencies and processor_model

! **********************************************************************

!  write procdep.f90

   write( unit= log_unit, fmt= fmtpr) 'writing ' // trim( pdname)    ! log attempt

   call write_processor_dependencies( istat)                         ! write module processor_dependencies

!  verify status

   write_processor_depend_error: if( istat > 0 )then                 ! if error writing procdep.f90

      write( unit= log_unit, fmt= fmtpr) 'ERROR: trouble writing ' // trim( pdname)

      stop error_stop_code                                           ! quit

   else write_processor_depend_error                                 ! if error writing procdep.f90

      write( unit= log_unit, fmt= fmtpr) 'wrote ' // trim( pdname) // ' ok'

   endif write_processor_depend_error                                ! if error writing procdep.f90

! **********************************************************************

!  write pm.f90

   write( unit= log_unit, fmt= fmtpr) 'writing ' // trim( pmname)    ! log attempt

   call write_processor_model( istat)                                ! write program processor_model

!  verify status

   write_processor_model_error: if( istat > 0 )then                  ! if error writing pm.f90

      write( unit= log_unit, fmt= fmtpr) 'ERROR: trouble writing ' // trim( pmname)

      stop error_stop_code                                           ! quit

   else write_processor_model_error                                  ! if error writing pm.f90

      write( unit= log_unit, fmt= fmtpr) 'wrote ' // trim( pmname) // ' ok'

   endif write_processor_model_error                                 ! if error writing pm.f90

! **********************************************************************

!  write date/time in logfile

   call date_and_time( date= run_date, time= run_time)               ! time of this run

   timestamp = run_date // run_time                                  ! combine

   write( unit= log_unit, fmt= fmtpr) 'timestamp: ' // timestamp     ! log timestamp

!  close logfile

   close( unit= log_unit, status= 'KEEP')                            ! keep logfile

! **********************************************************************

!  make_processor_model

stop normal_stop_code                                                ! make_processor_model

! **********************************************************************

!  make_processor_model library

! **********************************************************************

contains                                                             ! make_processor_model

! **********************************************************************

!  read_input_file() reads the user provided configuration file, if accessible

subroutine read_input_file( istat)

! **********************************************************************

!  read_input_file() interface

integer, intent( out) :: istat                                       ! status

! **********************************************************************

!  read_input_file() local

   logical :: have_rc                                                ! true if make_pm.in file exists

   logical :: found                                                  ! found a namelist group

   character( len= *), parameter :: error_msg = 'ERROR: trouble reading namelist group '

! **********************************************************************

!  read_input_file() text

continue                                                             ! read_input_file()

!  detect file make_pm.in

   inquire( file= rc_name, exist= have_rc)                           ! does rc_name exist?

!  if file make_pm.in exists

   rc_exists: if( have_rc )then                                      ! make_pm.in exists

! open file make_pm.in to read from the beginning

      open( unit= nml_unit, file= rc_name, iostat= istat, position= 'REWIND', &
            action= 'READ', status= 'OLD')

      open_error: if( istat > 0 )then                                ! can't open make_pm.in

         write( unit= log_unit, fmt= fmtpr) cant_open // rc_name     ! complain

         return                                                      ! read_input_file()

      endif open_error                                               ! can't open make_pm.in

   else rc_exists                                                    ! make_pm.in exists

!  no make_pm.in file was found

      istat = -1                                                     ! ok, use defaults

      return                                                         ! read_input_file()

   endif rc_exists                                                   ! make_pm.in exists

! ----------------------------------------------------------------------

!  read namelist group 'files'

   call seek_namelist_group( 'files', found, istat)

   found_files: if( found )then

      read( unit= nml_unit, nml= files, iostat= istat)               ! filenames

   endif found_files

   files_error: if( istat > 0 )then                                  ! can't read files

      write( unit= log_unit, fmt= fmtpr) error_msg // '"files"'      ! complain

      return                                                         ! read_input_file()

   endif files_error                                                 ! can't read files

   write( unit= log_unit, nml= files)                                ! echo to logfile

   rewind( unit= nml_unit)                                           ! read namelist groups in any order

! ----------------------------------------------------------------------

!  read namelist group 'hw'

   call seek_namelist_group( 'hw', found, istat)

   found_hw: if( found )then

      read( unit= nml_unit, nml= hw, iostat= istat)                  ! hardware

   endif found_hw

   hw_error: if( istat > 0 )then                                     ! can't read hw

      write( unit= log_unit, fmt= fmtpr) error_msg // '"hw"'         ! complain

      return                                                         ! read_input_file()

   endif hw_error                                                    ! can't read hw

   write( unit= log_unit, nml= hw)                                   ! echo to logfile

   rewind( unit= nml_unit)                                           ! read namelist groups in any order

! ----------------------------------------------------------------------

!  read namelist group 'sw'

   call seek_namelist_group( 'sw', found, istat)

   found_sw: if( found )then

      read( unit= nml_unit, nml= sw, iostat= istat)                  ! basic software

   endif found_sw

   sw_error: if( istat > 0 )then                                     ! can't read sw

      write( unit= log_unit, fmt= fmtpr) error_msg // '"sw"'         ! complain

      return                                                         ! read_input_file()

   endif sw_error                                                    ! can't read sw

   write( unit= log_unit, nml= sw)                                   ! echo to logfile

   rewind( unit= nml_unit)                                           ! read namelist groups in any order

! ----------------------------------------------------------------------

!  read namelist group 'kinds'

   call seek_namelist_group( 'kinds', found, istat)

   found_kinds: if( found )then

      read( unit= nml_unit, nml= kinds, iostat= istat)               ! non-numeric kinds

   endif found_kinds

   kinds_error: if( istat > 0 )then                                  ! can't read kinds

      write( unit= log_unit, fmt= fmtpr) error_msg // '"kinds"'      ! complain

      return                                                         ! read_input_file()

   endif kinds_error                                                 ! can't read kinds

   write( unit= log_unit, nml= kinds)                                ! echo to logfile

   rewind( unit= nml_unit)                                           ! read namelist groups in any order

! ----------------------------------------------------------------------

!  read namelist group 'float'

   call seek_namelist_group( 'float', found, istat)

   found_float: if( found )then

      read( unit= nml_unit, nml= float, iostat= istat)               ! real, complex kinds

   endif found_float

   float_error: if( istat > 0 )then                                  ! can't read float

      write( unit= log_unit, fmt= fmtpr) error_msg // '"float"'      ! complain

      return                                                         ! read_input_file()

   endif float_error                                                 ! can't read float

   write( unit= log_unit, nml= float)                                ! echo to logfile

   rewind( unit= nml_unit)                                           ! read namelist groups in any order

! ----------------------------------------------------------------------

!  read namelist group 'fixed'

   call seek_namelist_group( 'fixed', found, istat)

   found_fixed: if( found )then

      read( unit= nml_unit, nml= fixed, iostat= istat)               ! integer kinds

   endif found_fixed

   fixed_error: if( istat > 0 )then                                  ! can't read fixed

      write( unit= log_unit, fmt= fmtpr) error_msg // '"fixed"'      ! complain

      return                                                         ! read_input_file()

   endif fixed_error                                                 ! can't read fixed

   write( unit= log_unit, nml= fixed)                                ! echo to logfile

! ----------------------------------------------------------------------

!  close file make_pm.in

   close( unit= nml_unit, iostat= istat, status= 'KEEP')             ! (tried to) read all groups

   close_error: if( istat > 0 )then                                  ! trouble closing

      write( unit= log_unit, fmt= fmtpr) 'trouble closing ' // rc_name

   endif close_error                                                 ! trouble closing

return                                                               ! read_input_file()

! **********************************************************************

!  read_input_file()

end subroutine read_input_file

! **********************************************************************

!  seek_namelist_group() reads the user provided configuration file, if accessible

subroutine seek_namelist_group( group_name, found, istat)

! **********************************************************************

!  seek_namelist_group() interface

character( len= *), intent( in) :: group_name                        ! name of namelist to seek

logical, intent( out) :: found                                       ! true if found otherwise false

integer, intent( out) :: istat                                       ! status

! **********************************************************************

!  seek_namelist_group() constants

   character( len= *), parameter :: error_msg = 'error seeking namelist group '

   character( len= *), parameter :: afmt = '(a)'                     ! read format

!  introduce namelist group name

   character( len= *), parameter :: signal = '&'                     ! start of namelist group name

!  ignore blanks

   character( len= *), parameter :: blank = ' '                      ! may appear before &

!  namelist separators

   character( len= *), parameter :: separators = ' ,!/'              ! may appear after a group name

! **********************************************************************

!  seek_namelist_group() local

   character( len= 1024) :: input_buffer                             ! read with a format

   character( len= 32) :: lower_case_name                            ! lower case namelist group name

   integer :: name_len                                               ! length of group name

   integer :: input_name_len                                         ! length of name in input

   integer :: name_ptr                                               ! point to characters in name

   integer :: signal_ptr                                             ! point to signal in input buffer

   integer :: input_name_loc                                         ! first character in input buffer name

   integer :: input_name_ptr                                         ! point to characters in input buffer name

! **********************************************************************

!  seek_namelist_group() text

continue                                                             ! seek_namelist_group()

!  get name length & convert name to lc

   name_len = len_trim( group_name)                                  ! length of name

   lower_case_name = blank                                           ! set to blanks

!  loop through each character in name

   name_to_lower_case: do name_ptr = 1, name_len                     ! each character in name

!  examine each character in name

      each_character: select case( group_name( name_ptr: name_ptr))  ! examine

!  if upper case letter

      case( 'A': 'Z') each_character                                 ! find upper case

!  convert upper case letter to lower case letter

         lower_case_name( name_ptr: name_ptr) = achar( iachar( group_name( name_ptr: name_ptr)) + change_case)

!  otherwise

      case default each_character                                    ! find something else

!  copy character

         lower_case_name( name_ptr: name_ptr) = group_name( name_ptr: name_ptr)

      end select each_character                                      ! examine

   enddo name_to_lower_case                                          ! each character in name

! ----------------------------------------------------------------------

!  initialize read records loop

   istat = 0                                                         ! no errors nor end

   found = .false.                                                   ! not found yet

!  read until end of file or nemalist group is found

   seek_name: do while( istat == 0 )                                 ! read til found or eof

!  read a record as a character string

      read( unit= nml_unit, fmt = afmt, iostat= istat) input_buffer  ! read record as characters

!  case of istat

      read_status: select case( istat)                               ! check io status

!  encountered an end

      case( : -1) read_status                                        ! must be eof

!  end of file

         istat = 0                                                   ! eof without finding group name

         exit seek_name                                              ! quit

!  read error

      case( 1: ) read_status                                         ! read error

!  complain & quit

         write( unit= log_unit, fmt= fmtpr) error_msg // group_name  ! add name to complaint

         exit seek_name                                              ! quit

! ----------------------------------------------------------------------

!  got record to decode

      case default read_status                                       ! read a string

!  seek signal of namelist group name

         signal_ptr = verify( input_buffer, blank)                   ! find first non blank

!  if one is found

         first_non_blank: if( signal_ptr > 0 )then                   ! point to first non blank

!  if possible group name

            found_signal: if( input_buffer( signal_ptr: signal_ptr) ==  signal )then

!  name starts at next character after signal

               input_name_loc = signal_ptr + 1                       ! first character of name

!  find separator in input buffer

               input_name_len = scan( input_buffer( input_name_loc: ), separators) + signal_ptr

!  convert any upper case letter to lower case to match name

               input_name_to_lower_case: do input_name_ptr = input_name_loc, input_name_len

!  examine each character

                  input_each_character: select case( input_buffer( input_name_ptr: input_name_ptr))

!  upper case letter

                  case( 'A': 'Z') input_each_character               ! upper case to lower case

!  convert to lower case letter

                     input_buffer( input_name_ptr: input_name_ptr) = &
                        achar( iachar( input_buffer( input_name_ptr: input_name_ptr)) + change_case)

                  end select input_each_character                    ! upper case to lower case

               enddo input_name_to_lower_case                        ! do each character

!  look for a name match

               name_eq: if( input_buffer( input_name_loc: input_name_len) == lower_case_name )then

!  found name following signal

                  found = .true.                                     ! eureka

!  if match, backspace so caller can reread record

                  backspace( unit= nml_unit, iostat= istat)          ! put it back

!  check for backspace error

                  back_error: if( istat > 0 )then                    ! if error

                     write( unit= log_unit, fmt= *) error_msg // group_name

                  endif back_error                                   ! if error

!  exit from read loop

                  exit seek_name                                     ! quit successfully

!  name match

               endif name_eq                                         ! name found in input_buffer

!  found signal

            endif found_signal                                       ! & found in input_buffer

!  nonblank record

         endif first_non_blank                                       ! skip blanks in input_buffer

! ----------------------------------------------------------------------

!  read iostat case

      end select read_status                                         ! read not error nor end

!  read to eof

   enddo seek_name                                                   ! read til found or eof

!  return

return                                                               ! seek_namelist_group()

! **********************************************************************

!  seek_namelist_group()

end subroutine seek_namelist_group

! **********************************************************************

!  count_integer_kinds() counts integer kinds

subroutine count_integer_kinds

! **********************************************************************

!  This subroutine counts the number of integer kinds this processor supports.

! **********************************************************************

!  count_integer_kinds() local

   integer :: count_kinds                                            ! count the number of integer kinds

   integer :: this_kind                                              ! find kinds

   integer :: previous_kind                                          ! the previous kind found

   integer :: this_digits                                            ! loop through digits

! **********************************************************************

!  count_integer_kinds()

continue                                                             ! count_integer_kinds()

! **********************************************************************

!  The search assumes the following:

!  There must be at least one kind of integer.
!  One kind of integer will support at least one digit.
!  All integer kinds can be found by searching through increasing numbers of digits supported.
!  A negative kind value will be returned only when all kinds have been found.

! **********************************************************************

!  initialize search for integer kinds

   count_kinds = 1                                                   ! number of kinds found so far

   this_digits = 1                                                   ! digits tested by selected_int_kind()

   previous_kind = selected_int_kind( this_digits)                   ! must support one integer kind

! ----------------------------------------------------------------------

!  do until all kinds are found

   find_all_kinds: do                                                ! loop over digits

      this_kind = selected_int_kind( this_digits)                    ! kind with this digits

!  branch: or all kinds found --> quit, found new kind --> count

      new_or_same_kind: if( this_kind < 0 )then                      ! not a kind value

         exit find_all_kinds                                         ! found all kinds

      elseif( this_kind /= previous_kind )then new_or_same_kind      ! found new kind

         count_kinds = count_kinds + 1                               ! new kind

         previous_kind = this_kind                                   ! update previous kind

      endif new_or_same_kind                                         ! case of kind value

      this_digits = this_digits + 1                                  ! try one more digit

!  do until kind value is negative

   enddo find_all_kinds                                              ! loop over digits

! ----------------------------------------------------------------------

!  report and record number of integer kinds

   write( unit= log_unit, fmt= *) 'number of integer kinds found: ', count_kinds

   number_of_integers = count_kinds                                  ! count of kinds

! **********************************************************************

!  return with integer kind values known

return                                                               ! count_integer_kinds()

! **********************************************************************

!  count_integer_kinds()

end subroutine count_integer_kinds

! **********************************************************************

!  seek_integer_kinds() seeks integer kinds

subroutine seek_integer_kinds

! **********************************************************************

!  This subroutine finds the integer kinds available and
!  tries to map them to the byte, short, int, and long kind names.

! **********************************************************************

!  seek_integer_kinds() constants

! ----------------------------------------------------------------------

!  prefix of nondefault integer kind names

   character( len= *), parameter :: kind_prefix = 'int'              ! int<digits>

! **********************************************************************

!  seek_integer_kinds() local

! ----------------------------------------------------------------------

   integer :: previous_kind                                          ! last valid kind value found

   integer :: selected_kind                                          ! returned by selected_int_kind()

   integer :: this_digits                                            ! loop over digits

   integer :: kinds_count                                            ! count kinds found

   integer :: match_count                                            ! try to assign each kind found

   character( len= kind_len) :: name_buffer                          ! construct unusual names

! **********************************************************************

!  seek_integer_kinds()

continue                                                             ! seek_integer_kinds()

! **********************************************************************

!  The search assumes the following:

!  There must be at least one kind of integer.
!  One kind of integer will support at least one digit.
!  All integer kinds can be found by searching through increasing numbers of digits supported.
!  A negative kind value will be returned only when all kinds have been found.

! **********************************************************************

!  initialize search for integer kinds

   kinds_count = 1                                                   ! start with the first kind to be found
   this_digits = 1                                                   ! which must have at least one digit

   selected_kind = selected_int_kind( r= this_digits)                ! must support one integer kind

   integer_kinds( kinds_count)% kind_value = selected_kind           ! first kind value found
   integer_kinds( kinds_count)% max_digits = this_digits             ! last digits used to select this kind

   previous_kind = selected_kind                                     ! kind already found

! ----------------------------------------------------------------------

!  do until kind value is negative

   find_all_kinds: do                                                ! loop over digits

      selected_kind = selected_int_kind( r= this_digits)             ! kind with one more digit

!  branch: all kinds found, found new kind, or found old kind again

      new_or_same_kind: if( selected_kind < 0 )then                  ! not a kind value

         exit find_all_kinds                                         ! found all kinds

      elseif( selected_kind /= previous_kind )then new_or_same_kind  ! found new kind

         kinds_count = kinds_count + 1                               ! new kind

         integer_kinds( kinds_count)% kind_value = selected_kind     ! store kind value

         integer_kinds( kinds_count)% max_digits = this_digits       ! most digits supported

         integer_kinds( kinds_count)% supported = .false.            ! not named yet

         previous_kind = selected_kind                               ! update previous kind

      else new_or_same_kind                                          ! same kind value

         integer_kinds( kinds_count)% max_digits = this_digits       ! supports one more digit

      endif new_or_same_kind                                         ! case of kind value

!  go find next kind

      this_digits = this_digits + 1                                  ! try one more digit

   enddo find_all_kinds                                              ! loop over digits

! ----------------------------------------------------------------------

!  found up to max_kinds integer kinds

   report_kinds: do this_digits = 1, ubound( array= integer_kinds, dim= 1)

      call find_integer_bit_size( integer_kinds( this_digits))

      report_supported: if( integer_kinds( this_digits)% kind_value > 0 )then

         write( unit= log_unit, fmt= *) 'found integer kind: ', integer_kinds( this_digits)% kind_value, &
                                        ' supporting digits: ', integer_kinds( this_digits)% max_digits, &
                                        ' estimated bit size: ', integer_kinds( this_digits)% integer_bit_size

      endif report_supported

   enddo report_kinds

! **********************************************************************

!  The mapping assumes the following:

!  All kinds supported were found by the above search.
!  The integer kinds will support approximately 2, 4, 9 or 18 digits.
!  A kind may be missing.
!  There are no more than four kinds of integers.
!  These kinds may be sensibly mapped to byte, short, int and long.

! **********************************************************************

!  try to set integer kinds parameters

   set_digits: if( storage_size_is_2n )then                          ! (probably) a power of 2 word size

! ----------------------------------------------------------------------

!  try to match exactly each kind found with one of byte, short, int, long

      assign_kinds_exactly: do match_count = 1, kinds_count          ! search thru kinds found

         kinds_2n: select case( integer_kinds( match_count)% max_digits )

         case( 2) kinds_2n                                           ! byte digits

            byte_idx = match_count

            bd = integer_kinds( match_count)% max_digits             ! set byte digits inquiry

            integer_kinds( match_count)% kind_name = 'byte'

            integer_kinds( match_count)% supported = .true.          ! this kind is assigned to byte

         case( 4) kinds_2n                                           ! short digits

            short_idx = match_count

            sd = integer_kinds( match_count)% max_digits             ! set short digits inquiry

            integer_kinds( match_count)% kind_name = 'short'

            integer_kinds( match_count)% supported = .true.          ! this kind assigned to short

         case( 9) kinds_2n                                           ! int digits

            int_idx = match_count

            id = integer_kinds( match_count)% max_digits             ! set int digits inquiry

            integer_kinds( match_count)% kind_name = 'int'

            integer_kinds( match_count)% supported = .true.          ! this kind assigned to int

         case( 18) kinds_2n                                          ! long digits

            long_idx = match_count

            ld = integer_kinds( match_count)% max_digits             ! set long digits inquiry

            integer_kinds( match_count)% kind_name = 'long'

            integer_kinds( match_count)% supported = .true.          ! this kind assigned to long

         case default kinds_2n                                       ! odd sizes in 2^n word

            write( unit= name_buffer, fmt= *) integer_kinds( match_count)% max_digits

            integer_kinds( match_count)% kind_name = kind_prefix // adjustl( name_buffer)

            integer_kinds( match_count)% supported = .true.          ! this kind assigned to long

         end select kinds_2n                                         ! select range

      enddo assign_kinds_exactly                                     ! search thru kinds found

! ----------------------------------------------------------------------

!  processor word size is not a power of 2

   else set_digits

! ----------------------------------------------------------------------

!  try to match approximately each kind found with one of byte, short, int, long

      assign_kinds: do match_count = 1, kinds_count                  ! search thru kinds found

         kinds_odd: select case( integer_kinds( match_count)% max_digits )

         case( 2: 3) kinds_odd                                       ! byte range

            one_byte: if( byte_idx == 0 )then                        ! only one byte

               byte_idx = match_count

               bd = integer_kinds( match_count)% max_digits          ! set byte digits inquiry

               integer_kinds( match_count)% kind_name = 'byte'

               integer_kinds( match_count)% supported = .true.       ! this kind is assigned to byte

            endif one_byte                                           ! only one byte

         case( 4: 5) kinds_odd                                       ! short range

            one_short: if( short_idx == 0 )then                      ! only one short

               short_idx = match_count

               sd = integer_kinds( match_count)% max_digits          ! set short digits inquiry

               integer_kinds( match_count)% kind_name = 'short'

               integer_kinds( match_count)% supported = .true.       ! this kind assigned to short

            endif one_short                                          ! only one short

         case( 9: 10) kinds_odd                                      ! int range

            one_int: if( int_idx == 0 )then                          ! only one int

               int_idx = match_count

               id = integer_kinds( match_count)% max_digits          ! set int digits inquiry

               integer_kinds( match_count)% kind_name = 'int'

               integer_kinds( match_count)% supported = .true.       ! this kind assigned to int

            endif one_int                                            ! only one int

         case( 18: 20) kinds_odd                                     ! long range

            one_long: if( long_idx == 0 )then                        ! only one long

               long_idx = match_count

               ld = integer_kinds( match_count)% max_digits          ! set long digits inquiry

               integer_kinds( match_count)% kind_name = 'long'

               integer_kinds( match_count)% supported = .true.       ! this kind assigned to long

            endif one_long                                           ! only one long

         case default kinds_odd                                      ! odd sized integer kind

            write( unit= name_buffer, fmt= *) integer_kinds( match_count)% max_digits

            integer_kinds( match_count)% kind_name = kind_prefix // adjustl( name_buffer)

            integer_kinds( match_count)% supported = .true.          ! this kind assigned to i<max_digits>

         end select kinds_odd                                        ! select range

      enddo assign_kinds                                             ! search thru kinds found

   endif set_digits

! ----------------------------------------------------------------------

!  ensure that byte, short, int, long have entries

   ensure_byte: if( byte_idx == 0 )then                              ! if byte has not been assigned

      find_byte: do match_count = 1, kinds_count                     ! scan small to large

         byte_slot: if( .not. integer_kinds( match_count)% supported )then

            integer_kinds( match_count)% kind_name = 'byte'          ! first is byte

            byte_idx = match_count                                   ! set index
            
            exit find_byte                                           ! done
            
         endif byte_slot                                             ! find unnamed kind
      
      enddo find_byte                                                ! scan small to large
      
   endif ensure_byte                                                 ! if byte has not been assigned

   ensure_short: if( short_idx == 0 )then                            ! if short has not been assigned

      find_short: do match_count = 1, kinds_count                    ! scan small to large

         short_slot: if( .not. integer_kinds( match_count)% supported )then

            integer_kinds( match_count)% kind_name = 'short'         ! next is short

            short_idx = match_count                                  ! set index
            
            exit find_short                                          ! done
            
         endif short_slot                                            ! find unnamed kind
      
      enddo find_short                                               ! scan small to large
      
   endif ensure_short                                                ! if short has not been assigned

   ensure_int: if( int_idx == 0 )then                                ! if int has not been assigned

      find_int: do match_count = 1, kinds_count                      ! scan small to large

         int_slot: if( .not. integer_kinds( match_count)% supported )then

            integer_kinds( match_count)% kind_name = 'int'           ! next is int

            int_idx = match_count                                    ! set index
            
            exit find_int                                            ! done
            
         endif int_slot                                              ! find unnamed kind
      
      enddo find_int                                                 ! scan small to large
      
   endif ensure_int                                                  ! if int has not been assigned

   ensure_long: if( long_idx == 0 )then                              ! if long has not been assigned

      find_long: do match_count = 1, kinds_count                     ! scan small to large

         long_slot: if( .not. integer_kinds( match_count)% supported )then

            integer_kinds( match_count)% kind_name = 'long'          ! next is long

            long_idx = match_count                                   ! set index
            
            exit find_long                                           ! done
            
         endif long_slot                                             ! find unnamed kind
      
      enddo find_long                                                ! scan small to large
      
   endif ensure_long                                                 ! if long has not been assigned

! **********************************************************************

!  return with integer kind values known

return                                                               ! seek_integer_kinds()

! **********************************************************************

!  seek_integer_kinds()

end subroutine seek_integer_kinds

! **********************************************************************

!  find_integer_bit_size() counts integer kinds

subroutine find_integer_bit_size( this_integer)

! **********************************************************************

!  This subroutine finds the range of a real kind whose precision is known.

! **********************************************************************

!  find_integer_bit_size() interface

type( integer_kind_t), intent( inout) :: this_integer                ! find the range of this precision

! **********************************************************************

!  find_integer_bit_size() constants

   integer, parameter :: sign_bit = 1                                ! bits in the sign bit

! **********************************************************************

!  find_integer_bit_size() local

   integer :: this_bits                                              ! loop through powers of two

   real :: recip_log10_radix

! **********************************************************************

!  find_integer_bit_size()

continue                                                             ! find_integer_bit_size()

! **********************************************************************

!  initialize search for integer bit size

   recip_log10_radix = 1.0 / log10( real( radix( 0)) )               ! convert decimal to radix

   this_bits = 1                                                     ! initialize

!  find power of two nearly equal to the digits range

   find_digits_bits: do                                              ! loop over range

      digits_range: if( ceiling( ( this_integer% max_digits * recip_log10_radix) / this_bits) > 1 )then

         this_bits = this_bits + 1                                   ! one more bit

         cycle find_digits_bits                                      ! try again

      else digits_range

         exit find_digits_bits                                       ! quit

      endif digits_range                                             ! case of kind value

   enddo find_digits_bits                                            ! loop over range

   this_integer% integer_bit_size = sign_bit + this_bits             ! estimate

!  if a power of two sized processor, round to nearest power of two

   if_2n_hw: if( storage_size_is_2n )then                            ! hw is 2n

      round_2n: select case( this_integer% integer_bit_size)         ! select power of two

      case( 6: 9) round_2n

         this_integer% integer_bit_size = 8                          ! round to nearest

      case( 14: 17) round_2n

         this_integer% integer_bit_size = 16                         ! round to nearest

      case( 29: 33) round_2n

         this_integer% integer_bit_size = 32                         ! round to nearest

      case( 58: 66) round_2n

         this_integer% integer_bit_size = 64                         ! round to nearest

      case( 116: 132) round_2n

         this_integer% integer_bit_size = 128                        ! round to nearest

      case default round_2n                                          ! remark difficulty

         write( unit= log_unit, fmt= fmtpri) 'WARNING: estimated integer size not nearly a power of 2'

      end select round_2n                                            ! select power of two

   endif if_2n_hw                                                    ! hw is 2n

! **********************************************************************

!  return with integer kind values known

return                                                               ! find_integer_bit_size()

! **********************************************************************

!  find_integer_bit_size()

end subroutine find_integer_bit_size

! **********************************************************************

!  count_real_kinds() counts real kinds

subroutine count_real_kinds

! **********************************************************************

!  This subroutine counts the number of real kinds available.

! **********************************************************************

!  count_real_kinds() local

   integer :: count_kinds                                            ! count the number of integer kinds

   integer :: this_kind                                              ! find kinds

   integer :: this_precision                                         ! loop through precision

   integer :: previous_kind                                          ! the previous kind found

! **********************************************************************

!  count_real_kinds()

continue                                                             ! count_real_kinds()

! **********************************************************************

!  The search assumes the following:

!  There must be at least one kind of integer.
!  One kind of integer will support at least one digit.
!  All integer kinds can be found by searching through increasing numbers of digits supported.
!  A negative kind value will be returned only when all kinds have been found.

! **********************************************************************

!  initialize search for integer kinds

   count_kinds = 1                                                   ! number of valid results so far

   this_precision = 1                                                ! arg of selected_int_kind()

   previous_kind = selected_real_kind( p= this_precision)            ! must support one integer kind

! ----------------------------------------------------------------------

!  do until kind value is negative

   find_all_kinds: do                                                ! loop over digits

      this_kind = selected_real_kind( p= this_precision)             ! kind with one more digit

!  branch: all kinds found --> quit, or found new kind --> count

      new_or_same_kind: if( this_kind < 0 )then                      ! not a kind value so all kinds found

         exit find_all_kinds                                         ! quit seeking

      elseif( this_kind /= previous_kind )then new_or_same_kind      ! found new kind

         count_kinds = count_kinds + 1                               ! new kind

         previous_kind = this_kind                                   ! update previous kind

      endif new_or_same_kind                                         ! case of kind value

      this_precision = this_precision + 1                            ! try one more digit

   enddo find_all_kinds                                              ! loop over digits

! ----------------------------------------------------------------------

!  report and record number of real kinds

   write( unit= log_unit, fmt= *) 'number of real kinds found: ', count_kinds

   number_of_reals = count_kinds                                     ! count of kinds

! **********************************************************************

!  return with integer kind values known

return                                                               ! count_real_kinds()

! **********************************************************************

!  count_real_kinds()

end subroutine count_real_kinds

! **********************************************************************

!  seek_real_kinds() seeks real kinds

subroutine seek_real_kinds

! **********************************************************************

!  seek_real_kinds() constants

! ----------------------------------------------------------------------

   character( len= *), parameter :: kind_prefix = 'r'                ! prefix of nonstandard kind names

   character( len= *), parameter :: range_complaint = &              ! warn of the unexpected
            'NOTE: range does not increase with increasing precision'

! **********************************************************************

!  seek_real_kinds() local

   integer :: precision_count                                        ! count by precision

   integer :: kinds_count                                            ! count kinds found
   integer :: this_kind                                              ! loop through kinds

   integer :: selected_kind                                          ! returned by selected_real_kind()

   integer :: previous_kind                                          ! previous kind found

   integer :: match_count                                            ! try to assign each kind found

   character( len= kind_len) :: name_buf                             ! construct names of kinds

! **********************************************************************

!  seek_real_kinds()

continue                                                             ! seek_real_kinds()

! **********************************************************************

!  initialize search for real kinds by precision

   kinds_count = 1                                                   ! start with the first kind to be found
   precision_count = 1                                               ! which must have a precision of at least 1

   selected_kind = selected_real_kind( p= precision_count)           ! must support two real kinds

   real_kinds( kinds_count)% kind_value = selected_kind              ! first kind value found
   real_kinds( kinds_count)% max_precision = precision_count         ! precision used to select this kind

   previous_kind = selected_kind                                     ! kind already found

! ----------------------------------------------------------------------

!  do until kind value is negative

   find_all_kinds: do                                                ! loop over precision

      selected_kind = selected_real_kind( p= precision_count)        ! next kind

!  this precision is too great so seek range of previous kind

      new_old_all_prec: if( selected_kind < 0 )then                  ! to much precision

!  complete the previous real kind

         call seek_range_this_precision( real_kinds( kinds_count))   ! seek range

!  beyond the last kind available

         exit find_all_kinds                                         ! found all kinds by precision

!  found a different kind value greater than zero

      elseif( selected_kind /= previous_kind )then new_old_all_prec  ! found new kind

!  complete the previous real kind

         call seek_range_this_precision( real_kinds( kinds_count))   ! seek range

         real_kinds( kinds_count)% supported = .false.               ! not yet assigned a name

!  start new real kind

         kinds_count = kinds_count + 1                               ! try to find a new kind

         real_kinds( kinds_count)% max_precision = precision_count   ! at least this precision

         real_kinds( kinds_count)% kind_value = selected_kind        ! start new kind

         previous_kind = selected_kind                               ! update

!  still finding the same kind value

      else new_old_all_prec                                          ! same as old kind

         real_kinds( kinds_count)% max_precision = precision_count   ! at least this precision

      endif new_old_all_prec                                         ! case of kind value

      precision_count = precision_count + 1                          ! next precision

   enddo find_all_kinds                                              ! loop over precision

! ----------------------------------------------------------------------

!  report real kinds

   report_kinds: do this_kind = 1, ubound( array= real_kinds, dim= 1)

      call find_real_bit_size( real_kinds( this_kind))

      report_supported: if( real_kinds( this_kind)% kind_value > 0 )then

         write( unit= log_unit, fmt= *) 'found real kind: ', real_kinds( this_kind)% kind_value, &
                                        ' supporting precision: ', real_kinds( this_kind)% max_precision, &
                                        ' supporting range: ', real_kinds( this_kind)% max_range, &
                                        ' estimated bit size: ', real_kinds( this_kind)% real_bit_size

      endif report_supported

   enddo report_kinds

! **********************************************************************

!  check for range anomalies

   range_check: do this_kind = 2, ubound( array= real_kinds, dim= 1) ! check all pairs of kinds

      anomalie: if( real_kinds( this_kind - 1)% max_range >= real_kinds( this_kind)% max_range )then

         write( unit= log_unit, fmt= *) range_complaint

         write( unit= log_unit, fmt= *) &
            'kind values: ', real_kinds( this_kind - 1)% kind_value, real_kinds( this_kind)% kind_value

         write( unit= log_unit, fmt= *) &
            'ranges: ', real_kinds( this_kind - 1)% max_range, real_kinds( this_kind)% max_range

      endif anomalie

   enddo range_check                                                 ! check all pairs of kinds

! **********************************************************************

!  try to set real kinds parameters

   set_kinds: if( storage_size_is_2n )then                           ! word size is 2^n (probably 32 or 64)

! ----------------------------------------------------------------------

!  try to match kinds found exactly with single, double, quad

      assign_kinds_exactly: do match_count = 1, ubound( array= real_kinds, dim= 1)

         call find_real_bit_size( real_kinds( match_count))

         kinds_2n: select case( real_kinds( match_count)% real_bit_size )

         case( 32) kinds_2n                                          ! single precision

            single_idx = match_count

            sp = real_kinds( match_count)% max_precision             ! set single precision value
            sr = real_kinds( match_count)% max_range                 ! set single range value

            real_kinds( match_count)% kind_name = 'single'

            real_kinds( match_count)% supported = .true.             ! this kind assigned

         case( 64) kinds_2n                                          ! double precision

            double_idx = match_count

            dp = real_kinds( match_count)% max_precision             ! set double precision value
            dr = real_kinds( match_count)% max_range                 ! set double range value

            real_kinds( match_count)% kind_name = 'double'

            real_kinds( match_count)% supported = .true.             ! this kind assigned

         case( 128) kinds_2n                                         ! quad range

            quad_idx = match_count

            qp = real_kinds( match_count)% max_precision             ! set quad precision
            qr = real_kinds( match_count)% max_range                 ! set quad range

            real_kinds( match_count)% kind_name = 'quad'

            real_kinds( match_count)% supported = .true.             ! this kind assigned

         case default kinds_2n

            write( unit= name_buf, fmt= *) real_kinds( match_count)% max_precision

            real_kinds( match_count)% kind_name = kind_prefix // adjustl( name_buf)

            real_kinds( match_count)% supported = .true.             ! this kind assigned

         end select kinds_2n                                         ! select range

      enddo assign_kinds_exactly                                     ! search thru kinds found

! ----------------------------------------------------------------------

!  not a power of 2 word size

   else set_kinds                                                    ! want to auto detect

! ----------------------------------------------------------------------

!  try to match kinds found with single, double, quad

      assign_kinds: do match_count = 1, ubound( array= real_kinds, dim= 1)

         kinds_odd: select case( real_kinds( match_count)% max_precision )

         case( 28: 36) kinds_odd                                     ! single precision

            one_single: if( single_idx == 0 )then                    ! only one single

               single_idx = match_count

               sp = real_kinds( match_count)% max_precision          ! set single precision value
               sr = real_kinds( match_count)% max_range              ! set single range value

               real_kinds( match_count)% kind_name = 'single'

               real_kinds( match_count)% supported = .true.          ! this kind assigned

            endif one_single                                         ! only one single

         case( 58: 72) kinds_odd                                     ! double precision

            one_double: if( double_idx == 0 )then                    ! only one double

               double_idx = match_count

               dp = real_kinds( match_count)% max_precision          ! set double precision value
               dr = real_kinds( match_count)% max_range              ! set double range value

               real_kinds( match_count)% kind_name = 'double'

               real_kinds( match_count)% supported = .true.          ! this kind assigned

            endif one_double                                         ! only one double

         case( 120: 144) kinds_odd                                   ! quad precision

            one_quad: if( quad_idx == 0 )then                        ! only one quad

               quad_idx = match_count

               qp = real_kinds( match_count)% max_precision          ! set quad precision value
               qr = real_kinds( match_count)% max_range              ! set quad range value

               real_kinds( match_count)% kind_name = 'quad'

               real_kinds( match_count)% supported = .true.          ! this kind assigned

            endif one_quad                                           ! only one quad

         case default kinds_odd

            write( unit= name_buf, fmt= *) real_kinds( match_count)% max_precision

            real_kinds( match_count)% kind_name = kind_prefix // adjustl( name_buf)

            real_kinds( match_count)% supported = .true.             ! this kind assigned to long

         end select kinds_odd                                        ! select range

      enddo assign_kinds                                             ! search thru kinds found

   endif set_kinds

! ----------------------------------------------------------------------

!  ensure that single, double, quad have entries

   ensure_single: if( single_idx == 0 )then                          ! if single has not been assigned

      find_single: do match_count = 1, kinds_count                   ! scan small to large

         single_slot: if( .not. real_kinds( match_count)% supported )then

            real_kinds( match_count)% kind_name = 'single'           ! first is single

            single_idx = match_count                                 ! set index

            exit find_single                                         ! done
            
         endif single_slot                                           ! if unnamed
      
      enddo find_single                                              ! scan small to large
      
   endif ensure_single                                               ! if single has not been assigned

   ensure_double: if( double_idx == 0 )then                          ! if double has not been assigned

      find_double: do match_count = 1, kinds_count                   ! scan small to large

         double_slot: if( .not. real_kinds( match_count)% supported )then

            real_kinds( match_count)% kind_name = 'double'           ! next is double

            double_idx = match_count                                 ! set index
            
            exit find_double                                         ! done
            
         endif double_slot                                           ! if unnamed
      
      enddo find_double                                              ! scan small to large
      
   endif ensure_double                                               ! if double has not been assigned

   ensure_quad: if( quad_idx == 0 )then                              ! if quad has not been assigned

      find_quad: do match_count = 1, kinds_count                     ! scan small to large

         quad_slot: if( .not. real_kinds( match_count)% supported )then

            real_kinds( match_count)% kind_name = 'quad'             ! next is quad

            quad_idx = match_count                                   ! set index
            
            exit find_quad                                           ! done
            
         endif quad_slot                                             ! if unnamed
      
      enddo find_quad                                                ! scan small to large
      
   endif ensure_quad                                                 ! if quad has not been assigned

! **********************************************************************

!  return with real kinds values known

return                                                               ! seek_real_kinds()

! **********************************************************************

!  seek_real_kinds()

end subroutine seek_real_kinds

! **********************************************************************

!  seek_range_this_precision() computes the range of real kinds

subroutine seek_range_this_precision( this_real)

! **********************************************************************

!  This subroutine finds the range of a real kind whose precision is known.

! **********************************************************************

!  seek_range_this_precision() local

type( real_kind_t), intent( inout) :: this_real                      ! count the number of integer kinds

! **********************************************************************

!  seek_range_this_precision() local

   integer :: range_count                                            ! loop through range

   integer :: selected_kind

! **********************************************************************

!  seek_range_this_precision()

continue                                                             ! seek_range_this_precision()

! **********************************************************************

!  The search assumes the following:

!  The real has been found.
!  The precision has been found and stored in this_real.
!  The range can be found by holding the precision constant.
!  The range can be found by incrementing the range.

! **********************************************************************

!  initialize search for integer kinds

   range_count = 1                                                   ! initialize range count

   find_last_max_range: do                                           ! loop over range

      selected_kind = selected_real_kind( p= this_real% max_precision, r= range_count)

!  this range gives the same kind value as the previous range until new kind value or no more kinds

      last_range: if( selected_kind /= this_real% kind_value )then

         exit find_last_max_range                                    ! quit

      else last_range                                                ! case of kind value

         this_real% max_range = range_count                          ! update range

      endif last_range                                               ! case of kind value

      range_count = range_count + 1                                  ! next range

   enddo find_last_max_range                                         ! loop over range

! **********************************************************************

!  return with integer kind values known

return                                                               ! seek_range_this_precision()

! **********************************************************************

!  seek_range_this_precision()

end subroutine seek_range_this_precision

! **********************************************************************

!  find_real_bit_size() counts integer kinds

subroutine find_real_bit_size( this_real)

! **********************************************************************

!  This subroutine finds the range of a real kind whose precision is known.

! **********************************************************************

!  find_real_bit_size() interface

type( real_kind_t), intent( inout) :: this_real                      ! find the range of this precision

! **********************************************************************

!  find_real_bit_size() constants

   integer, parameter :: sign_bit = 1                                ! bits in the sign bit

! **********************************************************************

!  find_real_bit_size() local

   integer :: this_bits                                              ! loop through powers of two

   integer :: k                                                      ! as per the standard model of reals

   real :: recip_log10_radix

! **********************************************************************

!  find_real_bit_size()

continue                                                             ! find_real_bit_size()

! **********************************************************************

!  The calculation assumes the following:

!  There is one bit used as the sign bit.
!  The range is (nearly) a power of two.
!  There is some number of bits in the precision.
!  There are no unused bits.

! **********************************************************************

!  initialize search for real bit size

   k_0_1: if( log10( real( radix( 0.0)) ) == anint( log10( real( radix( 0.0)) )) )then

      k = 1

   else k_0_1

      k = 0

   endif k_0_1

   recip_log10_radix = 1.0 / log10( real( radix( 0.0)) )             ! convert decimal to radix

   this_bits = 1                                                     ! initialize

   find_exponent_bits: do                                            ! loop over range

!  find power of two nearly equal to the exponent range

      exp_range: if( ceiling( ( this_real% max_range * recip_log10_radix) / 2.0**this_bits) > 1 )then

         this_bits = this_bits + 1                                   ! one more bit

         cycle find_exponent_bits                                    ! try again

      else exp_range

         exit find_exponent_bits                                     ! quit

      endif exp_range                                                ! case of kind value

   enddo find_exponent_bits                                          ! loop over range

   this_real% real_bit_size = sign_bit + this_bits + nint( ( this_real% max_precision + 1) * recip_log10_radix - k)

!  if a power of two sized processor, round to nearest power of two

   if_2n_hw: if( storage_size_is_2n )then                            ! hw is 2n

      round_2n: select case( this_real% real_bit_size)               ! select power of two

      case( 14: 17) round_2n

         this_real% real_bit_size = 16                               ! round to nearest

      case( 29: 33) round_2n

         this_real% real_bit_size = 32                               ! round to nearest

      case( 59: 66) round_2n

         this_real% real_bit_size = 64                               ! round to nearest

      case( 116: 132) round_2n

         this_real% real_bit_size = 128                              ! round to nearest

      case( 244: 264) round_2n

         this_real% real_bit_size = 256                              ! round to nearest

      case default round_2n                                          ! remark difficulty

         write( unit= log_unit, fmt= fmtpri) 'WARNING: estimated real size not nearly a power of 2'

      end select round_2n                                            ! select power of two

   endif if_2n_hw                                                    ! hw is 2n

! **********************************************************************

!  return with integer kind values known

return                                                               ! find_real_bit_size()

! **********************************************************************

!  find_real_bit_size()

end subroutine find_real_bit_size

! **********************************************************************

!  check_character_kinds() perform sanity checks on logical kinds

subroutine check_character_kinds

! **********************************************************************

!  check_character_kinds() local

   integer :: count_chars                                            ! count number of kinds

! **********************************************************************

!  check_character_kinds() text

continue                                                             ! check_character_kinds()

!  check that there are no duplicate kind values

   check_ascii: if( ascii_character% supported )then

      warn_ascii_dup: if( ascii_character% kind_value == ebcdic_character% kind_value )then

         write( unit= log_unit, fmt= fmtpr) 'WARNING: ascii and ebcdic have same kind value!'

         stop error_stop_code                                        ! must be wrong

      elseif( ascii_character% kind_value == iso_10646_character% kind_value )then warn_ascii_dup

         write( unit= log_unit, fmt= fmtpr) 'WARNING: ascii and iso_10646 have same kind value!'

         stop error_stop_code                                        ! must be wrong

      endif warn_ascii_dup

   endif check_ascii

   check_ebcdic: if( ebcdic_character% supported )then

      warn_ebcdic_dup: if( ebcdic_character% kind_value > iso_10646_character% kind_value )then

         write( unit= log_unit, fmt= fmtpr) 'WARNING: ebcdic and iso_10646 have same kind value!'

         stop error_stop_code                                        ! must be wrong

      endif warn_ebcdic_dup

   endif check_ebcdic                                                ! check for duplicate values

!  complain if there isn't exactly one default character kind detected

   count_chars = count( (/ ascii_character% default_kind, ebcdic_character% default_kind, &
                           iso_10646_character% default_kind /))

   count_def_chars: if( count_chars < 1 )then

      write( unit= log_unit, fmt= fmtpr) 'WARNING: default character kind not detected'

   elseif( count_chars > 1)then count_def_chars                      ! count default character kinds

      write( unit= log_unit, fmt= fmtpr) 'WARNING: default character kind not unique'

   endif count_def_chars                                             ! count default character kinds

return                                                               ! check_character_kinds()

! **********************************************************************

!  check_character_kinds()

end subroutine check_character_kinds

! **********************************************************************

!  check_logical_kinds() perform sanity checks on logical kinds

subroutine check_logical_kinds

! **********************************************************************

!  check_logical_kinds() local

integer :: count_logicals

! **********************************************************************

!  check_logical_kinds() text

continue                                                             ! check_logical_kinds()

!  check for duplicate integer kind definitions

   check_byte: if( byte_logical% supported )then                     ! logical byte duplicate value

!  check byte == short, int, long

      warn_byte_dup: if( byte_logical% kind_value == short_logical% kind_value )then

         write( unit= log_unit, fmt= fmtpr) &
            'WARNING: byte logical and short logical have same kind value!'

         stop error_stop_code                                        ! must be wrong

      elseif( byte_logical% kind_value == int_logical% kind_value )then warn_byte_dup

         write( unit= log_unit, fmt= fmtpr) &
            'WARNING: byte logical and int logical have same kind value!'

         stop error_stop_code                                        ! must be wrong

      elseif( byte_logical% kind_value == long_logical% kind_value )then warn_byte_dup

         write( unit= log_unit, fmt= fmtpr) &
            'WARNING: byte logical and long logical have same kind value!'

         stop error_stop_code                                        ! must be wrong

      endif warn_byte_dup                                            ! byte duplicates long

   endif check_byte                                                  ! logical byte duplicate value

!  if logical short defined with a duplicate defined, complain and quit

   check_short: if( short_logical% supported )then                   ! logical short duplicate value

      warn_short_dup: if( short_logical% kind_value == int_logical% kind_value )then

         write( unit= log_unit, fmt= fmtpr) &
            'WARNING: short logical and int logical have same kind value!'

         stop error_stop_code                                        ! must be wrong

      elseif( short_logical% kind_value == long_logical% kind_value )then warn_short_dup

         write( unit= log_unit, fmt= fmtpr) &
            'WARNING: short logical and long logical have same kind value!'

         stop error_stop_code                                        ! must be wrong

      endif warn_short_dup                                           ! short duplicates long

   endif check_short                                                 ! logical short duplicate value

!  if logical int defined with a duplicate defined, complain and quit

   check_int: if( int_logical% supported )then                       ! logical int duplicate value

      warn_int_dup: if( int_logical% kind_value == long_logical% kind_value )then

         write( unit= log_unit, fmt= fmtpr) &
            'WARNING: int logical and long logical have same kind value!'

         stop error_stop_code                                        ! must be wrong

      endif warn_int_dup                                             ! int duplicates long

   endif check_int                                                   ! logical int duplicate value

! ----------------------------------------------------------------------

!  warn if logical kind is defined and the corresponding integer isn't

   user_byte_logical: if( byte_logical% supported )then              ! user defined byte logical

      no_byte_integer: if( .not. integer_kinds( byte_idx)% supported )then

         write( unit= log_unit, fmt= fmtpr) &
            'WARNING: logical byte defined but integer byte not detected!'

      endif no_byte_integer                                          ! but byte integer wasn't detected

   endif user_byte_logical                                           ! user defined byte logical

   user_short_logical: if( short_logical% supported )then            ! user defined short logical

      no_short_integer: if( .not. integer_kinds( short_idx)% supported )then

         write( unit= log_unit, fmt= fmtpr) &
            'WARNING: logical short defined but integer short not detected!'

      endif no_short_integer                                         ! but short integer wasn't detected

   endif user_short_logical                                          ! user defined short logical

   user_int_logical: if( int_logical% supported )then                ! user defined int logical

      no_int_integer: if( .not. integer_kinds( int_idx)% supported )then

         write( unit= log_unit, fmt= fmtpr) &
            'WARNING: logical int defined but integer int not detected!'

      endif no_int_integer                                           ! but int integer wasn't detected

   endif user_int_logical                                            ! user defined int logical

   user_long_logical: if( long_logical% supported )then              ! user defined long logical

      no_long_integer: if( .not. integer_kinds( long_idx)% supported )then

         write( unit= log_unit, fmt= fmtpr) &
            'WARNING: logical long defined but integer long not detected!'

      endif no_long_integer                                          ! but long integer wasn't detected

   endif user_long_logical                                           ! user defined long logical

! **********************************************************************

!  try to guess correspondence between logical and integer kinds

   def_int_logical: if( kind( 0) == kind( .true.) )then              ! default integer <-> default real

      write( unit= log_unit, fmt= fmtpr) 'default integer kind same value as default logical kind'

   else def_int_logical                                              ! default integer <-> default real

      write( unit= log_unit, fmt= fmtpr) 'WARNING: integer kind <-> logical kind correspondence not detected!'

   endif def_int_logical                                             ! default integer <-> default real

!  complain if there isn't exactly one default logical kind detected

   count_logicals = count( (/ byte_logical% default_kind, short_logical% default_kind, &
                              int_logical% default_kind, long_logical% default_kind /))

   count_def_log: if( count_logicals < 1 )then

      write( unit= log_unit, fmt= fmtpr) 'WARNING: default logical kind not detected'

   elseif( count_logicals > 1 )then count_def_log                    ! count default logical kinds

      write( unit= log_unit, fmt= fmtpr) 'WARNING: default logical kind not unique'

   endif count_def_log                                               ! count default logical kinds

return                                                               ! check_logical_kinds()

! **********************************************************************

!  check_logical_kinds()

end subroutine check_logical_kinds

! **********************************************************************

!  check_hardware_values() perform sanity checks on hardware values

subroutine check_hardware_values

! **********************************************************************

!  check_hardware_values() text

continue                                                             ! check_hardware_values()

!  check basic hardware parameters

   call basic_hardware_parameters                                    ! check sizes, endian

!  if claims IEEE 754

   ieee_32_64: if( ieeefp )then                                      ! claims IEEE fp

!  if ieeefp then nsu = 32 bits or 64 bits

      word_32_64: select case( measured_word_size )                  ! IEEE sanity

!  32 bit words

      case( 32) word_32_64                                           ! IEEE single

!  perform a few checks to see if real has the values expected of IEEE 32 bit

         call check_ieee_single( ieeefp)                             ! sanity check IEEE claim

         appears_ok_32: if( ieeefp )then                             ! ok

            write( unit= log_unit, fmt=  fmtpr) &
               'real appears to be an IEEE 754 real single format'

         else appears_ok_32                                          ! not ok

            write( unit= log_unit, fmt=  fmtpr) &
               'WARNING: default real does not appear to be valid IEEE 754 single format'

         endif appears_ok_32                                         ! check 32

!  64 bit words

      case( 64) word_32_64                                           ! IEEE double

!  perform a few checks to see if real has the values expected of IEEE 64 bit

         call check_ieee_double( ieeefp)                             ! sanity check IEEE claim

         appears_ok_64: if( ieeefp )then                             ! ok

            write( unit= log_unit, fmt=  fmtpr) &
               'real appears to be an IEEE 754 real format double format'

         else appears_ok_64                                          ! not ok

            write( unit= log_unit, fmt=  fmtpr) &
               'WARNING: default real does not appear to be valid IEEE 754 double format'

         endif appears_ok_64                                         ! check 64

!  check for IEEE 754 Extended types

      case default word_32_64                                        ! extended or problem

         word_extended: select case( measured_word_size)             ! IEEE extended sanity

         case( 43: 78) word_extended                                 ! IEEE extended sanity

            call check_ieee_single_extended( ieeefp)

            pass_single_extended: if( ieeefp )then

               write( unit= log_unit, fmt= fmtpr) 'real appears to be IEEE 754 Extended single format'

            endif pass_single_extended

         case( 79: ) word_extended                                   ! IEEE extended sanity

            call check_ieee_double_extended( ieeefp)

            pass_double_extended: if( ieeefp )then

               write( unit= log_unit, fmt= fmtpr) 'real appears to be IEEE 754 Extended double format'

            endif pass_double_extended

         case default word_extended                                  ! IEEE extended sanity

            ieeefp = .false.                                         ! wrong word size

         end select word_extended                                    ! IEEE extended sanity

         appears_ok_extended: if( .not. ieeefp )then

            write( unit= log_unit, fmt=  fmtpr) &
               'WARNING: claims IEEE format but appears not to be'

         endif appears_ok_extended

      end select word_32_64                                          ! IEEE sanity

   endif ieee_32_64                                                  ! claims IEEE fp

!  sanity checked

return                                                               ! check_hardware_values()

! **********************************************************************

!  check_hardware_values()

end subroutine check_hardware_values

! **********************************************************************

!  basic_hardware_parameters() perform sanity checks on hardware values

subroutine basic_hardware_parameters

! **********************************************************************

!  basic_hardware_parameters() constants

   integer, parameter :: smallest_kind = selected_int_kind( 1)

! **********************************************************************

!  basic_hardware_parameters() local

   character( len= 1), allocatable, dimension( :) :: char_target     ! allocate as needed

   integer, dimension( 1) :: size_target                             ! transfer target

   logical :: word_size_is_2n                                        ! true if word size is 2^n

   logical :: char_size_is_2n                                        ! true if character size is 2^n

   logical :: byte_size_is_2n                                        ! true if byte size is 2^n

! **********************************************************************

!  basic_hardware_parameters() text

continue                                                             ! basic_hardware_parameters()

!  check word size is a power of 2

   measured_word_size = bit_size( 0)                                 ! assume model size is storage size

   word_size_is_2n = is_power_of_2( measured_word_size)              ! word size is 2^n

   word_2n: if( .not. word_size_is_2n )then                          ! remark
      
      write( unit= log_unit, fmt=  fmtpr) &
         "NOTE: word size is not a power of two"                     ! not 2^n

   endif word_2n                                                     ! remark

! ----------------------------------------------------------------------

!  allocate character array to check whether it's a power of 2 and to check endian

   allocate( char_target( measured_word_size))                       ! target of transfer

!  compute character size

   measured_char_size = size( transfer( char_target, size_target))   ! count

!  check processor endian via char_target while it's allocated

   char_target = transfer( (/ 1 /), char_target, measured_word_size) ! words to bytes

   measured_big_endian = ichar( char_target( 1)) == 0                ! which byte

   deallocate( char_target)                                          ! target of transfer

!  check whether character size is a power of 2

   char_size_is_2n = is_power_of_2( measured_char_size)              ! compute if is power of 2

   char_2n: if( .not. char_size_is_2n )then                          ! complain if not power of 2
      
      write( unit= log_unit, fmt=  fmtpr) &
         "NOTE: character size is not a power of two"

   endif char_2n                                                     ! complain if not power of 2

! ----------------------------------------------------------------------

!  does the processor have byte integers?

   might_have_bytes: if( smallest_kind /= kind( 0) )then             ! if smallest integer isn't default

!  find out how big the smallest integer is
      
      measured_byte_size = bit_size( 0_smallest_kind)                ! size of smallest integer

      byte_size_is_2n = is_power_of_2( measured_byte_size)           ! compute if size is 2^n

      byte_2n: if( .not. byte_size_is_2n )then                       ! if not, complain
      
         write( unit= log_unit, fmt=  fmtpr) &
            "NOTE: smallest integer size is not a power of two"

      endif byte_2n                                                  ! if not, complain

!  see if size of char is size of smallest integer

      char_eq_byte: if( measured_byte_size == measured_char_size )then

         write( unit= log_unit, fmt=  fmtpr) &                       ! note conclusion in log
            "smallest integer size is same as character storage unit size"

      endif char_eq_byte                                             ! char size is smallest integer size

   endif might_have_bytes                                            ! if smallest integer isn't default

! **********************************************************************

!  check that measured byte size is a multiple of measured word size

   measured_words_to_bytes: if( mod( measured_word_size, measured_byte_size) /= 0 )then

      write( unit= log_unit, fmt=  fmtpr) &
         'WARNING: measured numerical storage unit size is not a multiple of measured character storage unit size'

   endif measured_words_to_bytes

! **********************************************************************

!  check claim of two's compliment

   twos_comp_int: if( twoscomp )then                                 ! claim 2s compliment format

      call check_2s_comp( twoscomp)                                  ! sanity check 2s compliment format

      appears_ok_int: if( twoscomp )then                             ! claim upheld

         write( unit= log_unit, fmt=  fmtpr) &
            "integer appears to be two's compliment integer format"

      else appears_ok_int                                            ! claim not upheld

         write( unit= log_unit, fmt=  fmtpr) &
            "WARNING: claims two's compliment integer format but appears not to be"

      endif appears_ok_int                                           ! check claim

   endif twos_comp_int                                               ! claim 2s compliment format

! **********************************************************************

!  if byte addressable then not word addressable

   byte_and_word: if( bytesize > 0 .and. wordsize > 0 )then          ! input bytesize and wordsize

      byte_eq_word: if( bytesize == wordsize )then                   ! bytesize equals wordsize

         write( unit= log_unit, fmt=  fmtpri) &
            'WARNING: claimed byte size equals claimed word size', bytesize

      endif byte_eq_word                                             ! bytesize equals wordsize

   endif byte_and_word                                               ! have bytesize and wordsize

!  if bytesize > 0 then (nsu .mod. bytesize) = 0

   byte_addressable: if( bytesize > 0 )then                          ! check even number of bytes per word

      word_eq_some_bytes: if( mod( measured_word_size, bytesize) == 0 )then

         write( unit= log_unit, fmt=  fmtpri) 'bits per byte', bytesize

         write( unit= log_unit, fmt=  fmtpri) 'bytes per word', bit_size( 0) / bytesize

      else word_eq_some_bytes

         write( unit= log_unit, fmt=  fmtpr) &
            'WARNING: measured numerical storage unit size is not a multiple of input character storage unit sise'

      endif word_eq_some_bytes

   endif byte_addressable                                            ! check even number of bytes per word

!  or if wordsize > 0 then wordsize = bit_size( 0)

   word_addressable: if( wordsize > 0 )then                          ! check wordsize is default integer size

      word_eq_nsu: if( measured_word_size == wordsize )then

         write( unit= log_unit, fmt=  fmtpri) &
            'bits per numeric storage unit (word size)', wordsize

      else word_eq_nsu

         write( unit= log_unit, fmt=  fmtpr) &
            'WARNING: claims word addressable yet input wordsize is not one numeric storage unit'

      endif word_eq_nsu

   endif word_addressable                                            ! check wordsize is default integer size

!  if bytesize is not set, set it

   set_byte_size: if( bytesize == 0 )then                            ! bytesize not set in input file

      bytesize = measured_byte_size                                  ! set bytesize
      
   endif set_byte_size                                               ! bytesize not set in input file

!  finally, set wordsize to measured value   

   set_word_size: if( wordsize == 0 )then                            ! if not specified

      wordsize = measured_word_size                                  ! set as determined
      
      ua_str = ba_str                                                ! label address unit is bytes

      write( unit= log_unit, fmt=  fmtpr) 'byte addressable'         ! address mode in logfile

   else set_word_size                                                ! if not specified

      ua_str = wa_str                                                ! label address unit as words

      write( unit= log_unit, fmt=  fmtpr) 'word addressable'         ! address mode in logfile

   endif set_word_size                                               ! if not specified

! **********************************************************************

!  set and report storage size summary

   storage_size_is_2n = word_size_is_2n .and. char_size_is_2n .and. byte_size_is_2n

   storage_2n: if( .not. storage_size_is_2n )then                    ! remark
      
      write( unit= log_unit, fmt=  fmtpr) &
         "NOTE: storage size is not a power of two"                  ! not 2^n

   else storage_2n                                                   ! remark

      write( unit= log_unit, fmt= fmtpr) &
         "size of storage units is a power of two"

   endif storage_2n                                                  ! remark

! **********************************************************************

!  basic hardware checked

return                                                               ! basic_hardware_parameters()

! **********************************************************************

!  basic_hardware_parameters()

end subroutine basic_hardware_parameters

! **********************************************************************

!  is_power_of_2() verify that the positive integer argument is a power of 2

logical function is_power_of_2( i)

integer, intent( in) :: i                                            ! subject of test

! **********************************************************************

!  is_power_of_2() text

continue                                                             ! is_power_of_2()

   is_power_of_2 = iand( not( i), i - 1) == ( i - 1)

return                                                               ! is_power_of_2()

! **********************************************************************

!  is_power_of_2()

end function is_power_of_2

! **********************************************************************

!  check_ieee_single() try to verify default real is ieee 32 bit format

subroutine check_ieee_single( flag)

logical, intent( out) :: flag                                        ! ok or not

! **********************************************************************

!  check_ieee_single() text

continue                                                             ! check_ieee_single()

!  this procedure is only called if the word size is 32 bits

   flag = radix( 0.0) == 2 &                                         ! 754 is base 2
          .and. ( maxexponent( 0.0) - minexponent( 0.0)) == 253 &    ! binary exponent less 2 extrema
          .and. digits( 0.0) == 24                                   ! bits of significand

return                                                               ! check_ieee_single()

! **********************************************************************

!  check_ieee_single()

end subroutine check_ieee_single

! **********************************************************************

!  check_ieee_double() try to verify default real is ieee 64 bit format

subroutine check_ieee_double( flag)

logical, intent( out) :: flag                                        ! ok or not

! **********************************************************************

!  check_ieee_double() text

continue                                                             ! check_ieee_double()

!  this procedure is only called if the word size is 64 bits

   flag = radix( 0.0) == 2 &                                         ! 754 is base 2
          .and. ( maxexponent( 0.0) - minexponent( 0.0)) == 2045 &   ! binary exponent less 2 extrema
          .and. digits( 0.0) == 53                                   ! bits of significand

return                                                               ! check_ieee_double()

! **********************************************************************

!  check_ieee_double()

end subroutine check_ieee_double

! **********************************************************************

!  check_ieee_single_extended() try to verify default real is ieee 64 bit format

subroutine check_ieee_single_extended( flag)

logical, intent( out) :: flag                                        ! ok or not

! **********************************************************************

!  check_ieee_single_extended() text

continue                                                             ! check_ieee_single_extended()

!  this procedure is only called if the word size is 43 to 78 bits

   flag = radix( 0.0) == 2 &                                         ! 754 is base 2
          .and. ( maxexponent( 0.0) - minexponent( 0.0)) >= 2045 &   ! binary exponent less 2 extrema
          .and. digits( 0.0) >= 32                                   ! bits of significand

return                                                               ! check_ieee_single_extended()

! **********************************************************************

!  check_ieee_single_extended()

end subroutine check_ieee_single_extended

! **********************************************************************

!  check_ieee_double_extended() try to verify default real is ieee 64 bit format

subroutine check_ieee_double_extended( flag)

logical, intent( out) :: flag                                        ! ok or not

! **********************************************************************

!  check_ieee_double_extended() text

continue                                                             ! check_ieee_double_extended()

!  this procedure is only called if the word size is >78 bits

   flag = radix( 0.0) == 2 &                                         ! 754 is base 2
          .and. ( maxexponent( 0.0) - minexponent( 0.0)) >= 32765 &  ! binary exponent less 2 extrema
          .and. digits( 0.0) >= 64                                   ! bits of significand

return                                                               ! check_ieee_double_extended()

! **********************************************************************

!  check_ieee_double_extended()

end subroutine check_ieee_double_extended

! **********************************************************************

!  check_2s_comp() try to verify default real is ieee 64 bit format

subroutine check_2s_comp( flag)

logical, intent( out) :: flag                                        ! ok or not

! **********************************************************************

!  check_2s_comp() text

continue                                                             ! check_2s_comp()

!  this procedure attempts to detect 2s compliment integers

   flag = radix( 0) == 2 &                                           ! binary integers
          .and. not( 0) == -1                                        ! 2s compliment

return                                                               ! check_2s_comp()

! **********************************************************************

!  check_2s_comp()

end subroutine check_2s_comp

! **********************************************************************

!  diagnose_input_output() perform experiments on the input output system

subroutine diagnose_input_output

! **********************************************************************

!  diagnose_input_output local

   character( len= *), parameter :: fmt_eof = '(/)'                  ! new record
   character( len= *), parameter :: fmt_eor = '(a1)'                 ! read one character

   integer, parameter :: da_recl = 1                                 ! record length in default characters

!  variables for optional plus and leading zero experiments

   integer :: op_fmt_val                                             ! value to write
   real :: lz_fmt_val                                                ! value to write
   character( len= 40) :: io_diag_buf                                ! buffer to be written

!  index of unit number in default filename

   integer :: fn_idx                                                 ! index result of searching filename

!  if guess of name of a file inquire returned a name

   logical :: got_fn                                                 ! from inquire()

!  test list-directed format d1, d2

   real :: x                                                         ! value to write fmt= *

!  test missing records

   character( len= 1) :: ch_var                                      ! variable to read from missing record

! **********************************************************************

!  diagnose_input_output() text

continue                                                             ! diagnose_input_output()

! ----------------------------------------------------------------------

!  try to get a file name for a connection without one

   open( unit= fn_unit, iostat= istat)                               ! file with no name

   fn_guess: if( istat /= 0 )then                                    ! io filename diagnosis failed

      write( unit= log_unit, fmt= fmtpr) 'WARNING: trouble opening file to diagnose io parameters'

      mrecl = 0                                                      ! unable to diagnose

      def_fn = ''                                                    ! null filename

      eor_flag = 0

      eof_flag = 0

      has_ld_sep = .false.

   else fn_guess                                                     ! io filename diagnosis working so far

!  get maximum record length

      inquire( unit= fn_unit, recl= mrecl)                           ! sequential formatted record

!  get default filename

      inquire( unit= fn_unit, named= got_fn, name= def_fn)           ! processor's filename

      fn_obtained: if( got_fn )then                                  ! inquire returned filename

!  where is unit number in filename?

         fn_idx = index( def_fn, ch_fn_unit, back= .true.)           ! try to find unit number (from the right)

         found_unit: if( fn_idx > 0 )then                            ! unit encoded within name

!  seek leading zeros of unit number in filename

            def_fn( fn_idx: fn_idx) = '#'                            ! where the unit number is

            do while( def_fn( fn_idx - 1: fn_idx - 1) == '0' )       ! zero in filename

               def_fn( fn_idx - 1: fn_idx - 1) = '#'                 ! set leading 0's to #'s

               fn_idx = fn_idx - 1                                   ! check next character

            enddo                                                    ! zero in filename

         endif found_unit                                            ! unit encoded within name

!  is a path present?

         fn_idx = scan( def_fn, '/\:', back= .true.)                 ! cover unix, win-dos, mac anyway

         got_path: if( fn_idx > 0 )then                              ! found a path

            def_fn = 'PATH' // def_fn( fn_idx: )                     ! remove path directory names

         endif got_path                                              ! found a path

!  file name not returned

      else fn_obtained                                               ! inquire returned filename

         def_fn = ""                                                 ! no filename

      endif fn_obtained                                              ! inquire returned filename

! ----------------------------------------------------------------------

!  diagnose eor and eof

      write( unit= fn_unit, fmt= fmtpr) '1'                          ! write a record
      write( unit= fn_unit, fmt= fmtpr) '2'                          ! write another record

      rewind( unit= fn_unit)                                         ! back to the beginning of file

!  find eor

      eor_flag = 0                                                   ! initialize iostat flag

      search_eor: do while( eor_flag == 0)                           ! loop til end of record encountered

         read( unit= fn_unit, fmt= fmt_eor, advance= 'NO', iostat= eor_flag) io_diag_buf( 1: 1)

      enddo search_eor                                               ! loop til end of record encountered

!  find eof

      eof_flag = 0                                                   ! initialize iostat flag

      search_eof: do while( eof_flag == 0)                           ! loop til end of file encountered

         read( unit= fn_unit, fmt= fmt_eof, iostat= eof_flag)

      enddo search_eof                                               ! loop til end of file encountered

! ----------------------------------------------------------------------

!  check for separator (comma or dot with f03) when using list directed format

      rewind( unit= fn_unit)                                         ! back to the beginning of file

      write( unit= fn_unit, fmt= *) eor_flag, eof_flag               ! write record with two integers

      rewind( unit= fn_unit)                                         ! back to the beginning of file

      read( unit= fn_unit, fmt= '(a)') io_diag_buf                   ! read record as string

      has_ld_sep = index( io_diag_buf, ',') > 0                      ! seek comma

! ----------------------------------------------------------------------

!  clean up

      close( unit= fn_unit, status= 'DELETE', iostat= istat)         ! finished with test file

      close_error: if( istat /= 0 )then                              ! error closing trial unit

         write( unit= log_unit, fmt= fmtpr) 'WARNING: trouble closing file opened to diagnose io parameters'

      endif close_error                                              ! error closing trial unit

   endif fn_guess                                                    ! io filename diagnosis

! ----------------------------------------------------------------------

!  try to get iostat= values for unwritten records

   open( unit= da_unit, status= 'SCRATCH', &
         access= 'DIRECT', recl= da_recl, iostat= istat)             ! scratch direct access file

!  open failed so report trouble

   da_get_iostat: if( istat /= 0 )then                               ! open direct access failed

      write( unit= log_unit, fmt= fmtpr) 'WARNING: trouble opening file to diagnose direct access parameters'

      da_missing = 0                                                 ! unable to diagnose

      da_eof = 0

!  open succeeded so test missing records iostat

   else da_get_iostat                                                ! direct access diagnosis working so far

      write( unit= da_unit, rec= 1) '1'
      write( unit= da_unit, rec= 3) '3'

      read( unit= da_unit, rec= 2, iostat= da_missing) ch_var
      read( unit= da_unit, rec= 4, iostat= da_eof) ch_var

!  complain in log file when iostat values are not the same

      not_same: if( da_missing /= da_eof )then

         write( unit= log_unit, fmt= fmtpr) 'WARNING: missing direct access record is not the same beyond last record'
         write( unit= log_unit, fmt= fmtpri) 'read unwritten record iostat', da_missing
         write( unit= log_unit, fmt= fmtpri) 'read beyond last record iostat', da_eof

!  reading an undefined record must be considered an error
!  zero is the status-ok iostat value, so change it

         must_be_error: if( da_missing == 0 .and. da_eof > 0 )then

            da_missing = da_eof

         endif must_be_error

      endif not_same

!  clean up

      close( unit= da_unit, status= 'DELETE', iostat= istat)         ! finished with test file

      close_da_error: if( istat /= 0 )then                           ! error closing trial unit

         write( unit= log_unit, fmt= fmtpr) 'WARNING: trouble closing file opened to diagnose direct access parameters'

      endif close_da_error                                           ! error closing trial unit

   endif da_get_iostat                                               ! direct access diagnosis working so far

! ----------------------------------------------------------------------

!  diagnose optional plus sign on integer and real formats

   io_diag_buf = ' '                                                 ! blank out buffer for test
   op_fmt_val = 12                                                   ! test value

!  write value to buffer with processor default for optional leading plus sign

   write( unit= io_diag_buf, fmt= '( s, i3)' ) op_fmt_val

   io_diag_buf = adjustl( io_diag_buf)                               ! remove leading blanks

!  first character should be '+' or '1' or test failed

   op_i: if( io_diag_buf( 1: 1) == '+' )then                         ! got '+'

      plus_flag = .true.                                             ! set flag

   elseif( io_diag_buf( 1: 1) == '1' )then op_i                      ! got '1'

      plus_flag = .false.                                            ! set flag

   else op_i                                                         ! got ???

      write( unit= log_unit, fmt= fmtpr) 'WARNING: test optional plus sign failed'

      stop error_stop_code                                           ! must be wrong

   endif op_i

!  diagnose leading zeros on f and e formats

   io_diag_buf = ' '                                                 ! blank out buffer for test
   lz_fmt_val = 0.123                                                ! test value

!  write value to buffer suppressing optional leading plus sign

   write( unit= io_diag_buf, fmt= '( ss, f6.3)' ) lz_fmt_val

   io_diag_buf = adjustl( io_diag_buf)                               ! remove leading blanks

!  first character should be '0' or '.' or test failed

   lz_f: if( io_diag_buf( 1: 1) == '0' )then                         ! got '0'

      lz_f_flag = .true.                                             ! set flag

   elseif( io_diag_buf( 1: 1) == '.' )then lz_f                      ! got '.'

      lz_f_flag = .false.                                            ! set flag

   else lz_f                                                         ! got ???

      write( unit= log_unit, fmt= fmtpr) 'WARNING: test leading zero f format failed'

      stop error_stop_code                                           ! must be wrong

   endif lz_f

!  reset for e format test

   io_diag_buf = ' '                                                 ! blank out buffer for test

!  write value to buffer suppressing optional leading plus sign

   write( unit= io_diag_buf, fmt= '( ss, e16.8)' ) lz_fmt_val

   io_diag_buf = adjustl( io_diag_buf)                               ! remove leading blanks

!  first character should be '0' or '.' or test failed

   lz_e: if( io_diag_buf( 1: 1) == '0' )then                         ! got '0'

      lz_e_flag = .true.                                             ! set flag

   elseif( io_diag_buf( 1: 1) == '.' )then lz_e                      ! got '.'

      lz_e_flag = .false.                                            ! set flag

   else lz_e                                                         ! got ???

      write( unit= log_unit, fmt= fmtpr) 'WARNING: test leading zero e format failed'

      stop error_stop_code                                           ! must be wrong

   endif lz_e

! ----------------------------------------------------------------------

!  diagnose list-directed format switch between f and e formats

   ld_min = 0                                                        ! start at 10^0

   x = 3.0                                                           ! start in the middle of a decade

   ld_fmt_min: do                                                    ! forever

      io_diag_buf = ' '                                              ! blank out buffer

      x = x / 10                                                     ! test next decade

      write( unit= io_diag_buf, fmt= *) x                            ! list-directed format

      if( scan( io_diag_buf, 'Ee') > 0 ) exit ld_fmt_min             ! quit upon E format

      ld_min = ld_min - 1                                            ! still F format

   enddo ld_fmt_min                                                  ! forever

   ld_max = 0                                                        ! start at 10^0

   x = 3.0                                                           ! start in the middle of a decade

   ld_fmt_max: do                                                    ! forever

      io_diag_buf = ' '                                              ! blank out buffer

      write( unit= io_diag_buf, fmt= *) x                            ! list-directed format

      x = x * 10                                                     ! test next decade

      if( scan( io_diag_buf, 'Ee') > 0 ) exit ld_fmt_max             ! quit upon E format

      ld_max = ld_max + 1                                            ! still F format

   enddo ld_fmt_max                                                  ! forever

! ----------------------------------------------------------------------

return                                                               ! diagnose_input_output()

! **********************************************************************

!  diagnose_input_output()

end subroutine diagnose_input_output

! **********************************************************************

!  write_integer_kind() writes an integer kind parameter & integer type

subroutine write_integer_kind( int_var)

! **********************************************************************

!  write_integer_kind() interface

type( integer_kind_t), intent( in) :: int_var                        ! name of kind

! **********************************************************************

!  write_integer_kind() local

   integer :: kind_name_len

! **********************************************************************

!  write_integer_kind() text

continue                                                             ! write_integer_kind()

   kind_name_len = len_trim( int_var% kind_name)

   write( unit= pd_unit, fmt= fmtpr) "!  " // int_var% kind_name( 1: kind_name_len) // " signed integer"
   write( unit= pd_unit, fmt= *)

!  ?kind? integers

   if( int_var% supported )then                                      ! has this kind
   
      write( unit= pd_unit, fmt= fmtprip) &
         "integer, public, parameter :: " // int_var% kind_name( 1: kind_name_len) &
         // "_k = selected_int_kind(", int_var% max_digits

!  ?kind? integers

   else                                                              ! has this kind
   
      write( unit= pd_unit, fmt= fmtpr) &
         "integer, public, parameter :: " // int_var% kind_name( 1: kind_name_len) // "_k = -1"

   endif                                                             ! has this kind

return                                                               ! write_integer_kind()

! **********************************************************************

!  write_integer_kind()

end subroutine write_integer_kind

! **********************************************************************

!  write_real_kind() writes an real kind parameter & real type

subroutine write_real_kind( real_var)

! **********************************************************************

!  write_real_kind() interface

type( real_kind_t), intent( in) :: real_var                          ! name of kind

! **********************************************************************

!  write_real_kind() local

   integer :: kind_name_len

! **********************************************************************

!  write_real_kind() text

continue                                                             ! write_real_kind()

   kind_name_len = len_trim( real_var% kind_name)

!  comment real single kind

   if( ieeefp )then                                                  ! ieee single
      write( unit= pd_unit, fmt= fmtpr) "!  IEEE 754 " // real_var% kind_name( 1: kind_name_len) // " real kind"
   else                                                              ! ieee single
      write( unit= pd_unit, fmt= fmtpr) "!  " // real_var% kind_name( 1: kind_name_len) // " real kind"
   endif                                                             ! ieee single

   write( unit= pd_unit, fmt= *) 

!  has single real

   if( real_var% supported )then                                     ! has ?kind? reals
   
      write( unit= pd_unit, fmt= fmtpriip) &
         "integer, public, parameter :: " // real_var% kind_name( 1: kind_name_len) // &
         "_k = selected_real_kind(", real_var% max_precision, real_var% max_range

   else                                                              ! has ?kind? reals

!  no ?kind? real

      select case( real_var% kind_value)                             ! select error code

      case( -1)                                                      ! select no such precision
         write( unit= pd_unit, fmt= fmtpr) &
            "integer, public, parameter :: " // real_var% kind_name( 1: kind_name_len) // "_k = -1"

      case( -2)                                                      ! select no such range
         write( unit= pd_unit, fmt= fmtpr) &
            "integer, public, parameter :: " // real_var% kind_name( 1: kind_name_len) // "_k = -2"

      case( -3)                                                      ! select no such real
         write( unit= pd_unit, fmt= fmtpr) &
            "integer, public, parameter :: " // real_var% kind_name( 1: kind_name_len) // "_k = -3"

      case default                                                   ! non-standard error code or single_k not unique

!  ?kind? real is not unique

         if( real_var% kind_value > 0 )then                          ! ?kind? not unique

            write( unit= pd_unit, fmt= fmtpriip) &
               "integer, public, parameter :: " // real_var% kind_name( 1: kind_name_len) // "_k = selected_real_kind(", sp, sr

            write( unit= log_unit, fmt= fmtpr) "WARNING: " // real_var% kind_name( 1: kind_name_len) // "_k: not unique"

!  no ?kind? real

         else                                                        ! non-standard error code

            write( unit= pd_unit, fmt= fmtpri) &
               "integer, public, parameter :: " // real_var% kind_name( 1: kind_name_len) // "_k =", real_var% kind_value

            write( unit= log_unit, fmt= fmtpri) &
               "WARNING: " // real_var% kind_name( 1: kind_name_len) // "_k: non-standard error code:", real_var% kind_value
         endif                                                       ! ?kind? not unique

      end select                                                     ! select error code

   endif                                                             ! has ?kind? reals

return                                                               ! write_real_kind()

! **********************************************************************

!  write_real_kind()

end subroutine write_real_kind

! **********************************************************************

!  write_bit_size() writes a Method of Olagnon bit_size() function

subroutine write_bit_size( type_str, kind_str)

! **********************************************************************

!  write_bit_size() interface

character( len= *), intent( in) :: type_str                          ! name of type

character( len= *), intent( in) :: kind_str                          ! name of kind

! **********************************************************************

!  write_bit_size() local

   character( len= 2) :: arg_name                                    ! name bit_size() argument

! **********************************************************************

!  write_bit_size() text

continue                                                             ! write_bit_size()

!  form ?kind_bit_size() dummy argument name

   arg_name = kind_str( 1: 1) // type_str( 1: 1)                     ! kt

!  write ?kind_bit_size() header

   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) &
      "!  " // kind_str // "_bit_size() bit_size() for kind " // kind_str
   write( unit= pd_unit, fmt= *)

!  standards prior to f95 do not support elemental

   if( standard >= f95_std )then                                     ! add elemental if f95
      write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
   endif                                                             ! add elemental if f95

!  function declaration

   write( unit= pd_unit, fmt= fmtpr) &
      "integer function " // kind_str // "_bit_size( " // arg_name // ")"
   write( unit= pd_unit, fmt= *)

!  character types need a length

   if( type_str == 'character' )then
      write( unit= pd_unit, fmt= fmtpr) &
         "character( len= *, kind= " // kind_str // "_k), intent( in) :: " // arg_name

!  non character types have no length

   else
      write( unit= pd_unit, fmt= fmtpr) &
         type_str // "( kind= " // kind_str // "_k), intent( in) :: " // arg_name
   endif

   write( unit= pd_unit, fmt= *)

!  ?kind_bit_size() local

   write( unit= pd_unit, fmt= fmtpr) "!  " // kind_str // "_bit_size() local"
   write( unit= pd_unit, fmt= *)

!  character local must have length one

   if( type_str == 'character' )then
      write( unit= pd_unit, fmt= fmtpr) &
         "   character( len= 1, kind= " // kind_str // "_k), dimension( bit_size( 0)) :: tk"

!  non character local must have no length

   else
      write( unit= pd_unit, fmt= fmtpr) &
         "   " // type_str // "( kind= " // kind_str // "_k), dimension( bit_size( 0)) :: tk"
   endif

!  ?kind_bit_size() text

   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  " // kind_str // "_bit_size() text"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) &
      "continue                                                             ! bit_size()"
   write( unit= pd_unit, fmt= *)

!  compute by Method of Olagnon

   write( unit= pd_unit, fmt= fmtpr) "   " // kind_str // "_bit_size = size( transfer( tk, (/ 0/) ))"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) &
      "return                                                               ! bit_size()"
   write( unit= pd_unit, fmt= *)

!  ?kind_bit_size() end function

   write( unit= pd_unit, fmt= fmtpr) "!  " // kind_str // "_bit_size()"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "end function " // kind_str // "_bit_size"
   write( unit= pd_unit, fmt= *)

return                                                               ! write_bit_size()

! **********************************************************************

!  write_bit_size()

end subroutine write_bit_size

! **********************************************************************

!  write_include_file() writes the define include file in user specified format

subroutine write_include_file( istat)

! **********************************************************************

!  write_include_file() interface

integer, intent( out) :: istat                                       ! status

! **********************************************************************

!  write_include_file() text

continue                                                             ! write_include_file()

! open file defines include file

   open( unit= inc_unit, file= incname, iostat= istat, position= 'REWIND', &
         action= 'WRITE', status= 'REPLACE')

   open_error: if( istat > 0 )then                                   ! can't open defines include file

      write( unit= log_unit, fmt= fmtpr) cant_open // trim( incname)

      return                                                         ! write_include_file()

   endif open_error                                                  ! can't open defines include file

! ----------------------------------------------------------------------

!  select defines format

   select_fpp: select case( ppr_inc)                                 ! include file format

! ----------------------------------------------------------------------

!  write defined kinds in coco format

   case( coco_inc) select_fpp                                        ! coco

!  write timestamp for version checks

      write( unit= inc_unit, fmt= fmtpr) '?? macro :: timestamp = "' // timestamp // '"'

!  write compiler version for version checks

      write( unit= inc_unit, fmt= fmtpr) '?? macro :: compiler = "' // trim( com_vers) // '"'

!  write standard diagnostic

      write( unit= inc_unit, fmt= fmtpr) '?? integer, parameter :: PREF66 = 0'

      write( unit= inc_unit, fmt= fmtpr) '?? integer, parameter :: F66 = 1'

      write( unit= inc_unit, fmt= fmtpr) '?? integer, parameter :: F77 = 2'

      write( unit= inc_unit, fmt= fmtpr) '?? integer, parameter :: F90 = 3'

      write( unit= inc_unit, fmt= fmtpr) '?? integer, parameter :: F95 = 4'

      write( unit= inc_unit, fmt= fmtpr) '?? integer, parameter :: F03 = 5'

      write( unit= inc_unit, fmt= fmtpr) '?? integer, parameter :: F08 = 6'

      select_std_coco: select case( standard)

      case( f90_std) select_std_coco

         write( unit= inc_unit, fmt= fmtpr) '?? integer, parameter :: STD = F90'

      case( f95_std) select_std_coco

         write( unit= inc_unit, fmt= fmtpr) '?? integer, parameter :: STD = F95'

      case( f03_std) select_std_coco

         write( unit= inc_unit, fmt= fmtpr) '?? integer, parameter :: STD = F03'

      case( f08_std) select_std_coco

         write( unit= inc_unit, fmt= fmtpr) '?? integer, parameter :: STD = F08'

      end select select_std_coco

!  define module parameters

      strings_coco: if( strings )then                                ! iso_varying_string

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: ISO_VARYING_STRINGS_T = .true.'

      else strings_coco                                              ! iso_varying_string

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: ISO_VARYING_STRINGS_T = .false.'

      endif strings_coco                                             ! iso_varying_string

!  define integer kind parameters

      byte_coco: if( integer_kinds( byte_idx)% supported )then       ! integer byte

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: BYTE_K = .true.'

      else byte_coco                                                 ! integer byte

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: BYTE_K = .false.'

      endif byte_coco                                                ! integer byte

      short_coco: if( integer_kinds( short_idx)% supported )then     ! integer short

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: SHORT_K = .true.'

      else short_coco                                                ! integer short

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: SHORT_K = .false.'

      endif short_coco                                               ! integer short

      int_coco: if( integer_kinds( int_idx)% supported )then         ! integer int

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: INT_K = .true.'

      else int_coco                                                  ! integer int

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: INT_K = .false.'

      endif int_coco                                                 ! integer int

      long_coco: if( integer_kinds( long_idx)% supported )then       ! integer long

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: LONG_K = .true.'

      else long_coco                                                 ! integer long

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: LONG_K = .false.'

      endif long_coco                                                ! integer long

!  define real (& complex) kind parameters

      single_coco: if( real_kinds( single_idx)% supported )then      ! real single

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: SINGLE_K = .true.'

      else single_coco                                               ! real single

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: SINGLE_K = .false.'

      endif single_coco                                              ! real single

      double_coco: if( real_kinds( double_idx)% supported )then      ! real double

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: DOUBLE_K = .true.'

      else double_coco                                               ! real double

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: DOUBLE_K = .false.'

      endif double_coco                                              ! real double

      quad_coco: if( real_kinds( quad_idx)% supported )then          ! real quad

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: QUAD_K = .true.'

      else quad_coco                                                 ! real quad

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: QUAD_K = .false.'

      endif quad_coco                                                ! real quad

!  define logical kind parameters

      l_byte_coco: if( byte_logical% supported )then                 ! logical byte

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: L_BYTE_K = .true.'

      else l_byte_coco                                               ! logical byte

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: L_BYTE_K = .false.'

      endif l_byte_coco                                              ! logical byte

      l_short_coco: if( short_logical% supported )then               ! logical short

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: L_SHORT_K = .true.'

      else l_short_coco                                              ! logical short

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: L_SHORT_K = .false.'

      endif l_short_coco                                             ! logical short

      l_int_coco: if( int_logical% supported )then                   ! logical int

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: L_INT_K = .true.'

      else l_int_coco                                                ! logical int

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: L_INT_K = .false.'

      endif l_int_coco                                               ! logical int

      l_long_coco: if( long_logical% supported )then                 ! logical long

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: L_LONG_K = .true.'

      else l_long_coco                                               ! logical long

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: L_LONG_K = .false.'

      endif l_long_coco                                              ! logical long

!  define character kind parameters

      ascii_coco: if( ascii_character% supported )then              ! character ascii

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: ASCII_K = .true.'

      else ascii_coco                                                ! character ascii

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: ASCII_K = .false.'

      endif ascii_coco                                               ! character ascii

      ebcdic_coco: if( ebcdic_character% supported )then             ! character ebcdic

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: EBCDIC_K = .true.'

      else ebcdic_coco                                               ! character ebcdic

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: EBCDIC_K = .false.'

      endif ebcdic_coco                                              ! character ebcdic

      iso_10646_coco: if( iso_10646_character% supported )then       ! character iso_10646

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: ISO_10646_K = .true.'

      else iso_10646_coco                                            ! character iso_10646

         write( unit= inc_unit, fmt= fmtpr) '?? logical, parameter :: ISO_10646_K = .false.'

      endif iso_10646_coco                                           ! character iso_10646

! ----------------------------------------------------------------------

!  write defined kinds in f90ppr format

   case( f90ppr_inc) select_fpp                                      ! f90ppr

!  write standard diagnostic

      write( unit= inc_unit, fmt= fmtpr) '$define PREF66 = 0'

      write( unit= inc_unit, fmt= fmtpr) '$define F66 = 1'

      write( unit= inc_unit, fmt= fmtpr) '$define F77 = 2'

      write( unit= inc_unit, fmt= fmtpr) '$define F90 = 3'

      write( unit= inc_unit, fmt= fmtpr) '$define F95 = 4'

      write( unit= inc_unit, fmt= fmtpr) '$define F03 = 5'

      write( unit= inc_unit, fmt= fmtpr) '$define F08 = 6'

      select_std_f90ppr: select case( standard)

      case( f90_std) select_std_f90ppr

         write( unit= inc_unit, fmt= fmtpr) '$define STD = F90'

      case( f95_std) select_std_f90ppr

         write( unit= inc_unit, fmt= fmtpr) '$define STD = F95'

      case( f03_std) select_std_f90ppr

         write( unit= inc_unit, fmt= fmtpr) '$define STD = F03'

      case( f08_std) select_std_f90ppr

         write( unit= inc_unit, fmt= fmtpr) '$define STD = F08'

      end select select_std_f90ppr

!  define module parameters

      strings_f90ppr: if( strings )then                              ! iso_varying_string

         write( unit= inc_unit, fmt= fmtpr) '$define ISO_VARYING_STRINGS_T'

      else strings_f90ppr                                            ! iso_varying_string

         write( unit= inc_unit, fmt= fmtpr) '!define ISO_VARYING_STRINGS_T'

      endif strings_f90ppr                                           ! iso_varying_string

!  define (or not) integer kind parameters

      byte_f90ppr: if( integer_kinds( byte_idx)% supported )then     ! integer byte

         write( unit= inc_unit, fmt= fmtpr) '$define BYTE_K'

      else byte_f90ppr                                               ! integer byte

         write( unit= inc_unit, fmt= fmtpr) '!define BYTE_K'

      endif byte_f90ppr                                              ! integer byte

      short_f90ppr: if( integer_kinds( short_idx)% supported )then   ! integer short

         write( unit= inc_unit, fmt= fmtpr) '$define SHORT_K'

      else short_f90ppr                                              ! integer short

         write( unit= inc_unit, fmt= fmtpr) '!define SHORT_K'

      endif short_f90ppr                                             ! integer short

      int_f90ppr: if( integer_kinds( int_idx)% supported )then       ! integer int

         write( unit= inc_unit, fmt= fmtpr) '$define INT_K'

      else int_f90ppr                                                ! integer int

         write( unit= inc_unit, fmt= fmtpr) '!define INT_K'

      endif int_f90ppr                                               ! integer int

      long_f90ppr: if( integer_kinds( long_idx)% supported )then     ! integer long

         write( unit= inc_unit, fmt= fmtpr) '$define LONG_K'

      else long_f90ppr                                               ! integer long

         write( unit= inc_unit, fmt= fmtpr) '!define LONG_K'

      endif long_f90ppr                                              ! integer long

!  define (or not) real (& complex) kind parameters

      single_f90ppr: if( real_kinds( single_idx)% supported )then    ! real single

         write( unit= inc_unit, fmt= fmtpr) '$define SINGLE_K'

      else single_f90ppr                                             ! real single

         write( unit= inc_unit, fmt= fmtpr) '!define SINGLE_K'

      endif single_f90ppr                                            ! real single

      double_f90ppr: if( real_kinds( double_idx)% supported )then    ! real double

         write( unit= inc_unit, fmt= fmtpr) '$define DOUBLE_K'

      else double_f90ppr                                             ! real double

         write( unit= inc_unit, fmt= fmtpr) '!define DOUBLE_K'

      endif double_f90ppr                                            ! real double

      quad_f90ppr: if( real_kinds( quad_idx)% supported )then        ! real quad

         write( unit= inc_unit, fmt= fmtpr) '$define QUAD_K'

      else quad_f90ppr                                               ! real quad

         write( unit= inc_unit, fmt= fmtpr) '!define QUAD_K'

      endif quad_f90ppr                                              ! real quad

!  define (or not) logical kind parameters

      l_byte_f90ppr: if( byte_logical% supported )then               ! logical byte

         write( unit= inc_unit, fmt= fmtpr) '$define L_BYTE_K'

      else l_byte_f90ppr                                             ! logical byte

         write( unit= inc_unit, fmt= fmtpr) '!define L_BYTE_K'

      endif l_byte_f90ppr                                            ! logical byte

      l_short_f90ppr: if( short_logical% supported )then             ! logical short

         write( unit= inc_unit, fmt= fmtpr) '$define L_SHORT_K'

      else l_short_f90ppr                                            ! logical short

         write( unit= inc_unit, fmt= fmtpr) '!define L_SHORT_K'

      endif l_short_f90ppr                                           ! logical short

      l_int_f90ppr: if( int_logical% supported )then                 ! logical int

         write( unit= inc_unit, fmt= fmtpr) '$define L_INT_K'

      else l_int_f90ppr                                              ! logical int

         write( unit= inc_unit, fmt= fmtpr) '!define L_INT_K'

      endif l_int_f90ppr                                             ! logical int

      l_long_f90ppr: if( long_logical% supported )then               ! logical long

         write( unit= inc_unit, fmt= fmtpr) '$define L_LONG_K'

      else l_long_f90ppr                                             ! logical long

         write( unit= inc_unit, fmt= fmtpr) '!define L_LONG_K'

      endif l_long_f90ppr                                            ! logical long

!  define (or not) character kind parameters

      ascii_f90ppr: if( ascii_character% supported )then            ! character ascii

         write( unit= inc_unit, fmt= fmtpr) '$define ASCII_K'

      else ascii_f90ppr                                              ! character ascii

         write( unit= inc_unit, fmt= fmtpr) '!define ASCII_K'

      endif ascii_f90ppr                                             ! character ascii

      ebcdic_f90ppr: if( ebcdic_character% supported )then           ! character ebcdic

         write( unit= inc_unit, fmt= fmtpr) '$define EBCDIC_K'

      else ebcdic_f90ppr                                             ! character ebcdic

         write( unit= inc_unit, fmt= fmtpr) '!define EBCDIC_K'

      endif ebcdic_f90ppr                                            ! character ebcdic

      iso_10646_f90ppr: if( iso_10646_character% supported )then     ! character iso_10646

         write( unit= inc_unit, fmt= fmtpr) '$define ISO_10646_K'

      else iso_10646_f90ppr                                          ! character iso_10646

         write( unit= inc_unit, fmt= fmtpr) '!define ISO_10646_K'

      endif iso_10646_f90ppr                                         ! character iso_10646

! ----------------------------------------------------------------------

!  write defined kinds in fpp/cpp format

   case( fpp_inc) select_fpp                                         ! fpp/cpp

!  write standard diagnostic

      write( unit= inc_unit, fmt= fmtpr) '#define PREF66 0'

      write( unit= inc_unit, fmt= fmtpr) '#define F66 1'

      write( unit= inc_unit, fmt= fmtpr) '#define F77 2'

      write( unit= inc_unit, fmt= fmtpr) '#define F90 3'

      write( unit= inc_unit, fmt= fmtpr) '#define F95 4'

      write( unit= inc_unit, fmt= fmtpr) '#define F03 5'

      write( unit= inc_unit, fmt= fmtpr) '#define F08 6'

      select_std_fpp: select case( standard)

      case( f90_std) select_std_fpp

         write( unit= inc_unit, fmt= fmtpr) '#define STD F90'

      case( f95_std) select_std_fpp

         write( unit= inc_unit, fmt= fmtpr) '#define STD F95'

      case( f03_std) select_std_fpp

         write( unit= inc_unit, fmt= fmtpr) '#define STD F03'

      case( f08_std) select_std_fpp

         write( unit= inc_unit, fmt= fmtpr) '#define STD F08'

      end select select_std_fpp

!  define module parameters

      strings_fpp: if( strings )then                                 ! iso_varying_string

         write( unit= inc_unit, fmt= fmtpr) '#define ISO_VARYING_STRINGS_T'

      else strings_fpp                                               ! iso_varying_string

         write( unit= inc_unit, fmt= fmtpr) '!define ISO_VARYING_STRINGS_T'

      endif strings_fpp                                              ! iso_varying_string

!  define (or not) integer kind parameters

      byte_fpp: if( integer_kinds( byte_idx)% supported )then        ! integer byte

         write( unit= inc_unit, fmt= fmtpr) '#define BYTE_K'

      else byte_fpp                                                  ! integer byte

         write( unit= inc_unit, fmt= fmtpr) '!define BYTE_K'

      endif byte_fpp                                                 ! integer byte

      short_fpp: if( integer_kinds( short_idx)% supported )then      ! integer short

         write( unit= inc_unit, fmt= fmtpr) '#define SHORT_K'

      else short_fpp                                                 ! integer short

         write( unit= inc_unit, fmt= fmtpr) '!define SHORT_K'

      endif short_fpp                                                ! integer short

      int_fpp: if( integer_kinds( int_idx)% supported )then          ! integer int

         write( unit= inc_unit, fmt= fmtpr) '#define INT_K'

      else int_fpp                                                   ! integer int

         write( unit= inc_unit, fmt= fmtpr) '!define INT_K'

      endif int_fpp                                                  ! integer int

      long_fpp: if( integer_kinds( long_idx)% supported )then        ! integer long

         write( unit= inc_unit, fmt= fmtpr) '#define LONG_K'

      else long_fpp                                                  ! integer long

         write( unit= inc_unit, fmt= fmtpr) '!define LONG_K'

      endif long_fpp                                                 ! integer long

!  define (or not) real (& complex) kind parameters

      single_fpp: if( real_kinds( single_idx)% supported )then       ! real single

         write( unit= inc_unit, fmt= fmtpr) '#define SINGLE_K'

      else single_fpp                                                ! real single

         write( unit= inc_unit, fmt= fmtpr) '!define SINGLE_K'

      endif single_fpp                                               ! real single

      double_fpp: if( real_kinds( double_idx)% supported )then       ! real double

         write( unit= inc_unit, fmt= fmtpr) '#define DOUBLE_K'

      else double_fpp                                                ! real double

         write( unit= inc_unit, fmt= fmtpr) '!define DOUBLE_K'

      endif double_fpp                                               ! real double

      quad_fpp: if( real_kinds( quad_idx)% supported )then           ! real quad

         write( unit= inc_unit, fmt= fmtpr) '#define QUAD_K'

      else quad_fpp                                                  ! real quad

         write( unit= inc_unit, fmt= fmtpr) '!define QUAD_K'

      endif quad_fpp                                                 ! real quad

!  define (or not) logical kind parameters

      l_byte_fpp: if( byte_logical% supported )then                  ! logical byte

         write( unit= inc_unit, fmt= fmtpr) '#define L_BYTE_K'

      else l_byte_fpp                                                ! logical byte

         write( unit= inc_unit, fmt= fmtpr) '!define L_BYTE_K'

      endif l_byte_fpp                                               ! logical byte

      l_short_fpp: if( short_logical% supported )then                ! logical short

         write( unit= inc_unit, fmt= fmtpr) '#define L_SHORT_K'

      else l_short_fpp                                               ! logical short

         write( unit= inc_unit, fmt= fmtpr) '!define L_SHORT_K'

      endif l_short_fpp                                              ! logical short

      l_int_fpp: if( int_logical% supported )then                    ! logical int

         write( unit= inc_unit, fmt= fmtpr) '#define L_INT_K'

      else l_int_fpp                                                 ! logical int

         write( unit= inc_unit, fmt= fmtpr) '!define L_INT_K'

      endif l_int_fpp                                                ! logical int

      l_long_fpp: if( long_logical% supported )then                  ! logical long

         write( unit= inc_unit, fmt= fmtpr) '#define L_LONG_K'

      else l_long_fpp                                                ! logical long

         write( unit= inc_unit, fmt= fmtpr) '!define L_LONG_K'

      endif l_long_fpp                                               ! logical long

!  define (or not) character kind parameters

      ascii_fpp: if( ascii_character% supported )then                ! character ascii

         write( unit= inc_unit, fmt= fmtpr) '#define ASCII_K'

      else ascii_fpp                                                 ! character ascii

         write( unit= inc_unit, fmt= fmtpr) '!define ASCII_K'

      endif ascii_fpp                                                ! character ascii

      ebcdic_fpp: if( ebcdic_character% supported )then              ! character ebcdic

         write( unit= inc_unit, fmt= fmtpr) '#define EBCDIC_K'

      else ebcdic_fpp                                                ! character ebcdic

         write( unit= inc_unit, fmt= fmtpr) '!define EBCDIC_K'

      endif ebcdic_fpp                                               ! character ebcdic

      iso_10646_fpp: if( iso_10646_character% supported )then        ! character iso_10646

         write( unit= inc_unit, fmt= fmtpr) '#define ISO_10646_K'

      else iso_10646_fpp                                             ! character iso_10646

         write( unit= inc_unit, fmt= fmtpr) '!define ISO_10646_K'

      endif iso_10646_fpp                                            ! character iso_10646

! ----------------------------------------------------------------------

!  unknown preprocessor include filename

   case default select_fpp                                           ! catch errors

      write( unit= log_unit, fmt= fmtpr) ' WARNING: unknown preprocessor file format'

! ----------------------------------------------------------------------

   end select select_fpp                                             ! include file format

! ----------------------------------------------------------------------

!  close define include file

   close( unit= inc_unit, iostat= istat, status= 'KEEP')             ! (tried to) read all groups

   close_error: if( istat > 0 )then                                  ! trouble closing

      write( unit= log_unit, fmt= fmtpr) 'trouble closing ' // trim( incname)

   endif close_error                                                 ! trouble closing

return                                                               ! write_include_file()

! **********************************************************************

!  write_include_file()

end subroutine write_include_file

! **********************************************************************

!  write_processor_dependencies() writes customized standard_type module

subroutine write_processor_dependencies( istat)

! **********************************************************************

!  write_processor_dependencies() interface

integer, intent( out) :: istat                                       ! status

! **********************************************************************

!  write_processor_dependencies() local

   integer :: rs_size                                                ! random seed size

   integer :: io_len = 0                                             ! test file storage units

   integer :: i                                                      ! integer, real kinds loop index

! **********************************************************************

!  write_processor_dependencies() text

continue                                                             ! write_processor_dependencies()

!  write a new processor_dependencies

   open( unit= pd_unit, file= pdname, iostat= istat, action= 'WRITE', status= 'REPLACE')

   open_error: if( istat > 0 )then                                   ! can't open stdtype.f90

      write( unit= log_unit, fmt= fmtpr) cant_open // trim( pdname)

      return                                                         ! write_processor_dependencies()

   endif open_error                                                  ! can't open stdtype.f90

!  write stdtype.f90

   write( unit= pd_unit, fmt= fmtpr) "! bof"
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= fmtpr) "! Fortran 95 module processor_dependencies"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= fmtpr) "! Source Control Strings"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "! $Id: make_pm.f90 2.2 2005/05/30 15:47:01Z Dan Release $"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= fmtpr) "!  Copyright 2004 Purple Sage Computing Solutions, Inc."
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!   This library is free software; you can redistribute it and/or"
   write( unit= pd_unit, fmt= fmtpr) "!   modify it under the terms of the GNU Library General Public"
   write( unit= pd_unit, fmt= fmtpr) "!   License as published by the Free Software Foundation; either"
   write( unit= pd_unit, fmt= fmtpr) "!   version 2 of the License, or (at your option) any later version."
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!   This library is distributed in the hope that it will be useful,"
   write( unit= pd_unit, fmt= fmtpr) "!   but WITHOUT ANY WARRANTY; without even the implied warranty of"
   write( unit= pd_unit, fmt= fmtpr) "!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU"
   write( unit= pd_unit, fmt= fmtpr) "!   Library General Public License for more details."
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!   You should have received a copy of the GNU Library General Public"
   write( unit= pd_unit, fmt= fmtpr) "!   License along with this library; if not, write to the Free"
   write( unit= pd_unit, fmt= fmtpr) "!   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA."
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "! To report bugs, suggest enhancements, etc. to the Authors,"
   write( unit= pd_unit, fmt= fmtpr) "! Contact:"
   write( unit= pd_unit, fmt= fmtpr) "!    Purple Sage Computing Solutions, Inc."
   write( unit= pd_unit, fmt= fmtpr) "!                               send email to dnagle@erols.com"
   write( unit= pd_unit, fmt= fmtpr) "!                                   or fax to 703 471 0684 (USA)"
   write( unit= pd_unit, fmt= fmtpr) "!                                  or mail to 12142 Purple Sage Ct."
   write( unit= pd_unit, fmt= fmtpr) "!                                             Reston, VA 20194-5621 USA"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= fmtpr) "! processor_dependencies describes the processor:  kinds, processor dependencies, &c"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "! Module processor_dependencies provides standard definitions"
   write( unit= pd_unit, fmt= fmtpr) "! of Fortran type kind parameters, and other Fortran"
   write( unit= pd_unit, fmt= fmtpr) "! processor dependent quantities.  The porting of Fortran"
   write( unit= pd_unit, fmt= fmtpr) "! programs from processor to processor is eased because symbolic,"
   write( unit= pd_unit, fmt= fmtpr) "! rather than 'magic number' constants and conditions may be used"
   write( unit= pd_unit, fmt= fmtpr) "! by the programmer.  The understanding of Fortran programs is enhanced"
   write( unit= pd_unit, fmt= fmtpr) "! because constants and subprograms have easily undertsood names."
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "! The compiler to which this file applies and the RCS strings of this"
   write( unit= pd_unit, fmt= fmtpr) "! file are available as default-kind character parameters."
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "! A set of kinds is defined, each a parameter whose name is <kind>_k."
   write( unit= pd_unit, fmt= fmtpr) "! The number of bits per numeric storage unit, per character storage"
   write( unit= pd_unit, fmt= fmtpr) "! and per file storage unit are defined."
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "! The intrinsic bit_size() is extended to all types and kinds (a complex"
   write( unit= pd_unit, fmt= fmtpr) "! kind is by definition twice the same real kind, but bit_size() should"
   write( unit= pd_unit, fmt= fmtpr) "! exist for all kinds).  The intrinsics tiny() and huge() are extended to"
   write( unit= pd_unit, fmt= fmtpr) "! character and complex kinds.  The intrinsic epsilon() is extended to"
   write( unit= pd_unit, fmt= fmtpr) "! complex kinds."
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "! Quantities defined include the input and output units preconnected,"
   write( unit= pd_unit, fmt= fmtpr) "! and whether there is a preconnected error unit.  The size of the"
   write( unit= pd_unit, fmt= fmtpr) "! processor's random number generator seed is defined to allow static"
   write( unit= pd_unit, fmt= fmtpr) "! declaration of the seed.  A flag is defined whether the processor"
   write( unit= pd_unit, fmt= fmtpr) "! uses ieee 754 floating point format.  A flag is defined whether the"
   write( unit= pd_unit, fmt= fmtpr) "! processor uses twos compliment integer arithmetic."
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "! Suggestions for other behavior of Fortran processors which could be"
   write( unit= pd_unit, fmt= fmtpr) "! codified in this module are very much welcomed by the author."
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "! A summary listing of defined parameters, and a listing of defined"
   write( unit= pd_unit, fmt= fmtpr) "! procedures follows."
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= fmtpr) "!  use no modules"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  processor_dependencies uses"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!     none"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  *** all parameters are default kind ***"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  processor_dependencies constants"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!     processor_dependencies_rcs_id= this file's RCS Identifier"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!     compiler_version= compiler to which this file applies"
   write( unit= pd_unit, fmt= fmtpr) "!     compiler_serial= compiler license serial"
   write( unit= pd_unit, fmt= fmtpr) "!     os_version= operating system to which this file applies"
   write( unit= pd_unit, fmt= fmtpr) "!     compiler_standard= Fortran standard supported by the compiler"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!     has_varying_strings- true if processor supports iso_varying_string"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!     ascii_k, ebcdic_k, iso_10646_k= character kinds"
   write( unit= pd_unit, fmt= fmtpr) "!     byte_k, short_k, int_k, long_k= integer kinds"
   write( unit= pd_unit, fmt= fmtpr) "!     l_byte_k, l_short_k, l_int_k, l_long_k= logical kinds"
   write( unit= pd_unit, fmt= fmtpr) "!     single_k, double_k, quad_k= real kinds (& thf complex kinds)"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!     byte_int, short_int, int_int, long_int= type( integer_inquiry_t)"
   write( unit= pd_unit, fmt= fmtpr) "!     single_real, double_real, quad_real= type( real_inquiry_t)"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!     numeric_storage_size= bits per numeric storage unit"
   write( unit= pd_unit, fmt= fmtpr) "!     character_storage_size= bits per character storage unit"
   write( unit= pd_unit, fmt= fmtpr) "!     file_storage_size= bits per file storage unit"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!     is_big_endian= true if processor is big endian"
   write( unit= pd_unit, fmt= fmtpr) "!     is_little_endian= true if processor is little endian"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!     default_filename= filename of a unit opened without a name"
   write( unit= pd_unit, fmt= fmtpr) "!     opt_plus_sign= true if optional plus with i, e, f, g formats"
   write( unit= pd_unit, fmt= fmtpr) "!     leading_zero_f= true if optional leading zero with f format"
   write( unit= pd_unit, fmt= fmtpr) "!     leading_zero_e= true if optional leading zero with e format"
   write( unit= pd_unit, fmt= fmtpr) "!     ld_fmt_fmin= smallest exponent list-directed format writes with f format"
   write( unit= pd_unit, fmt= fmtpr) "!     ld_fmt_fmax= largest exponent list-directed format writes with f format"
   write( unit= pd_unit, fmt= fmtpr) "!     da_undefined_record= iostat value returned when reading an undefined record"
   write( unit= pd_unit, fmt= *)

   write( unit= pd_unit, fmt= fmtpr) "!  processor_dependencies types"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!     none"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  processor_dependencies variables"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!     none"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  processor_dependencies operators"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!     none"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  *** all integer and logical functions return default kind ***"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  *** some functions use their argument only to select specific ***"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  processor_dependencies library"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!     bit_size() for character, logical, real, complex types"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!     int() for logical returns 1 or 0 for T or F"
   write( unit= pd_unit, fmt= fmtpr) "!     logical() for integers, returns T or F for non-0 or 0"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!     huge() for character kinds ( largest character code)"
   write( unit= pd_unit, fmt= fmtpr) "!     tiny() for character kinds ( smallest character code)"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!     huge() for complex kinds ( returns sqrt( real huge) )"
   write( unit= pd_unit, fmt= fmtpr) "!     tiny() for complex kinds ( returns sqrt( real tiny) )"
   write( unit= pd_unit, fmt= fmtpr) "!     epsilon() for complex kinds ( returns sqrt( real epsilon) )"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!     max_exact_int() largest integer a real kind increments (by one) exactly"
   write( unit= pd_unit, fmt= *)

! **********************************************************************

!  write processor_dependencies module declarations

! **********************************************************************

   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  processor_dependencies"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "module processor_dependencies"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  use none"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  explicit declaration of all names"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "implicit none"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  all variables are static"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "save"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  explicit export of all module names"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "private"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  processor_dependencies RCS strings"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  module source filename supplied by RCS"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "character( len= *), public, parameter :: processor_dependencies_rcs_id = &"
   write( unit= pd_unit, fmt= fmtpr) "   '$Id: make_pm.f90 2.2 2005/05/30 15:47:01Z Dan Release $'"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "character( len= *), public, parameter :: processor_depends_timestamp = &"
   write( unit= pd_unit, fmt= fmtpr) "   '" // trim( timestamp) // "'"
   write( unit= pd_unit, fmt= *)

! **********************************************************************

!  write processor_dependencies constant declarations

! **********************************************************************

   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  processor_dependencies data"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  compiler & OS version to which this file applies"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  compiler version used to generate this file"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "character( len= *), public, parameter :: compiler_version = &"
   write( unit= pd_unit, fmt= fmtpr) "   '" // trim( com_vers) // "'"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  compiler serial number used to generate this file"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "character( len= *), public, parameter :: compiler_serial = &"
   write( unit= pd_unit, fmt= fmtpr) "   '" // trim( com_sern) // "'"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  operating system version used to generate this file"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "character( len= *), public, parameter :: os_version = &"
   write( unit= pd_unit, fmt= fmtpr) "   '" // trim( os_vers) // "'"
   write( unit= pd_unit, fmt= *)

   write( unit= pd_unit, fmt= fmtpr) "!  standard supported by this compiler"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "character( len= *), public, parameter :: standard_version = &"

   select_std_coco: select case( standard)
   case( f90_std) select_std_coco
      write( unit= pd_unit, fmt= fmtpr) "   'Fortran 90'"
   case( f95_std) select_std_coco
      write( unit= pd_unit, fmt= fmtpr) "   'Fortran 95'"
   case( f03_std) select_std_coco
      write( unit= pd_unit, fmt= fmtpr) "   'Fortran 2003'"
   case( f08_std) select_std_coco
      write( unit= pd_unit, fmt= fmtpr) "   'Fortran 2008'"
   case default select_std_coco
      write( unit= pd_unit, fmt= fmtpr) "   'Fortran'"
   end select select_std_coco
   write( unit= pd_unit, fmt= *)

   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  *** all parameters are of default kind ***"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  modules available to the processor"
   write( unit= pd_unit, fmt= *)

   if( strings )then
      write( unit= pd_unit, fmt= fmtpr) "logical, public, parameter :: has_varying_strings = .true."
   else
      write( unit= pd_unit, fmt= fmtpr) "logical, public, parameter :: has_varying_strings = .false."
   endif

   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "! The first section defines the kind parameters supported by the processor."
   write( unit= pd_unit, fmt= fmtpr) "! First, the kind parameters are defined."
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  processor dependent kinds"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner

! **********************************************************************

!  if defining character kinds

   if( define_characters )then                                       ! want character kinds
      write( unit= pd_unit, fmt= *) 
      write( unit= pd_unit, fmt= fmtpr) "!  processor character kinds"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  ascii characters (seven bit codes stored in eight bit bytes)"
      write( unit= pd_unit, fmt= *)

!  select ascii characters starting with f03

      if( standard >= f03_std )then                                  ! has ascii
         write( unit= pd_unit, fmt= fmtpr) "integer, public, parameter :: ascii_k = selected_char_kind( 'ASCII')"

!  use input value before f03

      elseif( ascii_character% supported )then                       ! has ascii
         write( unit= pd_unit, fmt= fmtpri) "integer, public, parameter :: ascii_k =", ascii_character% kind_value

!  no ascii

      else                                                           ! has ascii
         write( unit= pd_unit, fmt= fmtpr) "integer, public, parameter :: ascii_k = -1"
      endif                                                          ! has ascii

!  ebcdic characters

      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  ebcdic characters (eight bit bytes)"
      write( unit= pd_unit, fmt= *)

!  use input value only

      if( ebcdic_character% supported )then                          ! has ebcdic
         write( unit= pd_unit, fmt= fmtpri) &
            "integer, public, parameter :: ebcdic_k =", ebcdic_character% kind_value

!  no ebcdic

      else                                                           ! has ebcdic
         write( unit= pd_unit, fmt= fmtpr) "integer, public, parameter :: ebcdic_k = -1"
      endif                                                          ! has ebcdic

      write( unit= pd_unit, fmt= *) 
      write( unit= pd_unit, fmt= fmtpr) "!  iso_10646 characters"
      write( unit= pd_unit, fmt= *)

!  select iso_10646 characters starting with f03

      if( standard >= f03_std )then                                  ! has iso_10646
         write( unit= pd_unit, fmt= fmtpr) &
            "integer, public, parameter :: iso_10646_k = selected_char_kind( 'ISO_10646')"
         write( unit= pd_unit, fmt= *)

!  use input value

      else                                                           ! has iso_10646

!  use input value requests iso_10646

         if( iso_10646_character% supported )then
            write( unit= pd_unit, fmt= fmtpri) &
               "integer, public, parameter :: iso_10646_k =", iso_10646_character% kind_value

!  no iso_10646

         else
            write( unit= pd_unit, fmt= fmtpr) "integer, public, parameter :: iso_10646_k = -1"
         endif
      endif                                                          ! iso_10646
   endif                                                             ! want character kinds

   write( unit= pd_unit, fmt= *)

! **********************************************************************

!  processor integers

   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  processor integer kinds"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  8-bit signed integer ( 1.3+2 = huge() )"
   write( unit= pd_unit, fmt= *)

!  byte integers

   call write_integer_kind( integer_kinds( byte_idx))

   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  16-bit signed integer ( 3.3+4 = huge() )"
   write( unit= pd_unit, fmt= *)

!  short integers

   call write_integer_kind( integer_kinds( short_idx))

   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  32-bit signed integer ( 2.1+9 = huge() )"
   write( unit= pd_unit, fmt= *)

!  int integers

   call write_integer_kind( integer_kinds(int_idx))

   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  64-bit signed integer ( 4.2+18 = huge() )"
   write( unit= pd_unit, fmt= *)

!  long integers

   call write_integer_kind( integer_kinds( long_idx))

   write( unit= pd_unit, fmt= *)

!  any unusual integers

   do i = 1, number_of_integers

      if( .not. ( i == byte_idx .or. i == short_idx .or. i == int_idx .or. i == long_idx ) )then

         if( integer_kinds( i)% supported )then
            
            call write_integer_kind( integer_kinds( i))
      
         endif
      
      endif
   
   enddo

! **********************************************************************

!  if defining logical kinds

   if( define_logicals )then                                         ! want logical kinds
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  processor logical kinds"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) star_banner

!  byte logicals

      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  logical kind using same storage as byte"
      write( unit= pd_unit, fmt= *)

      if( byte_logical% supported )then                              ! has byte logical
         write( unit= pd_unit, fmt= fmtpri) "integer, public, parameter :: l_byte_k =", byte_logical% kind_value

      else                                                           ! has byte logical

!  no byte logical

         write( unit= pd_unit, fmt= fmtpr) "integer, public, parameter :: l_byte_k = -1"
      endif                                                          ! has byte logical

!  short logical

      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  logical kind using same storage as short"
      write( unit= pd_unit, fmt= *)

      if( short_logical% supported )then                             ! has short logical
         write( unit= pd_unit, fmt= fmtpri) "integer, public, parameter :: l_short_k =", short_logical% kind_value
      else                                                           ! has short logical

!  no short logical

         write( unit= pd_unit, fmt= fmtpr) "integer, public, parameter :: l_short_k = -1"
      endif                                                          ! has short logical

!  int logical

      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  logical kind using same storage as int"
      write( unit= pd_unit, fmt= *)

      if( int_logical% supported )then                               ! has int logical
         write( unit= pd_unit, fmt= fmtpri) "integer, public, parameter :: l_int_k =", int_logical% kind_value
      else                                                           ! has int logical

!  no int logical

         write( unit= pd_unit, fmt= fmtpr) "integer, public, parameter :: l_int_k = -1"
      endif                                                          ! has int logical

!  long logical

      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  logical kind using same storage as long"
      write( unit= pd_unit, fmt= *)

      if( long_logical% supported )then                              ! has long logical
         write( unit= pd_unit, fmt= fmtpri) "integer, public, parameter :: l_long_k =", long_logical% kind_value

!  no long logical

      else                                                           ! has long logical
         write( unit= pd_unit, fmt= fmtpr) "integer, public, parameter :: l_long_k = -1"
      endif                                                          ! has long logical

      write( unit= pd_unit, fmt= *) 
   endif                                                             ! want logical kinds

! **********************************************************************

!  processor reals

   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  processor real ( & thf complex) kinds"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)

   call write_real_kind( real_kinds( single_idx))

   write( unit= pd_unit, fmt= *)

   call write_real_kind( real_kinds( double_idx))

   write( unit= pd_unit, fmt= *)

   call write_real_kind( real_kinds( quad_idx))

!  any unusual reals

   do i = 1, number_of_reals

      if( .not. ( i == single_idx .or. i == double_idx .or. i == quad_idx ) )then
      
         if( real_kinds( i)% supported )then
            
            call write_real_kind( real_kinds( i))

         endif
      
      endif
   
   enddo

! **********************************************************************

!  processor dependencies

   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  This section sets defines parameters specified by the"
   write( unit= pd_unit, fmt= fmtpr) "!  Fortran standard as being processor dependent.  These"
   write( unit= pd_unit, fmt= fmtpr) "!  parameters describe hardware, I/O, and the intrinsic"
   write( unit= pd_unit, fmt= fmtpr) "!  random number generator."
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  values specified by the processor"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)

!  storage sizes are defined in f03

   if( standard < f03_std )then

      write( unit= pd_unit, fmt= fmtpr) "!  bits per numeric storage unit"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpri) "integer, public, parameter :: numeric_storage_size =", wordsize
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  bits per character storage unit"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpri) "integer, public, parameter :: character_storage_size =", bytesize
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  bits per file storage unit"
      write( unit= pd_unit, fmt= *)

!  number of file storage units per default integer

      inquire( iolength= io_len) io_len                              ! length of one word record

!  compute bits per file storage unit

      write( unit= pd_unit, fmt= fmtpri) "integer, public, parameter :: file_storage_size = bit_size( 0) /", io_len
      write( unit= pd_unit, fmt= *)

   endif

   write( unit= pd_unit, fmt= fmtpr) "!  signal whether hardware addresses are byte addresses or word addresses"
   write( unit= pd_unit, fmt= *)

   if( ua_str == ba_str )then
      write( unit= pd_unit, fmt= fmtpr) "logical, public, parameter :: is_byte_addressable = .true."
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "logical, public, parameter :: is_word_addressable = .false."
   else
      write( unit= pd_unit, fmt= fmtpr) "logical, public, parameter :: is_byte_addressable = .false."
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "logical, public, parameter :: is_word_addressable = .true."
   endif

   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  signal whether hardware is big endian or little endian"
   write( unit= pd_unit, fmt= *)

!  is big endian

   if( measured_big_endian )then                                     ! is big/little endian
      write( unit= pd_unit, fmt= fmtpr) "logical, public, parameter :: is_big_endian = .true."
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "logical, public, parameter :: is_little_endian = .false."

!  or is little endian

   else                                                              ! is big/little endian
      write( unit= pd_unit, fmt= fmtpr) "logical, public, parameter :: is_big_endian = .false."
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "logical, public, parameter :: is_little_endian = .true."
   endif                                                             ! is big/little endian
   write( unit= pd_unit, fmt= *)

!  defined in iso_fortran_env if f03 or later

   if( standard < f03_std )then                                      ! f03 uses iso_fortran_env
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  i/o units preconnected for input ( unit= *)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpri) "integer, public, parameter :: input_unit =", stdin
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  i/o units preconnected for output ( unit= *)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpri) "integer, public, parameter :: output_unit =", stdout
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  i/o unit preconnected for error messages ( output_unit if no other)"
      write( unit= pd_unit, fmt= *)

!  set stderr to stdout if no stderr

      if( stderr < 0 )then                                           ! has stderr
         write( unit= pd_unit, fmt= fmtpr) "integer, public, parameter :: error_unit = output_unit"

!  has a stderr

      else                                                           ! has stderr
         write( unit= pd_unit, fmt= fmtpri) "integer, public, parameter :: error_unit =", stderr
      endif                                                          ! has stderr
   endif                                                             ! f03 uses iso_fortran_env

   write( unit= pd_unit, fmt= *)

   if( standard < f03_std )then
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  end of file and end of record iostat= values"
      write( unit= pd_unit, fmt= *)

!  default filename determined by experiment in diagnose_input_output()

      write( unit= pd_unit, fmt= fmtpr) "!  end of file"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpri) "integer, parameter :: iostat_end =", eof_flag
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  end of record"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpri) "integer, parameter :: iostat_eor =", eor_flag
      write( unit= pd_unit, fmt= *)
   endif

   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  name of a file opened without a name"
   write( unit= pd_unit, fmt= *)

!  default filename determined by experiment in diagnose_input_output()

   write( unit= pd_unit, fmt= fmtpr) &
      "character( len= *), public, parameter :: default_filename = '" // trim( def_fn) // "'"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)

!  smallest list directed real F format exponent determined by experiment in diagnose_input_output()

   write( unit= pd_unit, fmt= fmtpr) "!  exponents beyond which list-directed format switches from f to e"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpri) "integer, public, parameter :: ld_fmt_fmin =", ld_min
   write( unit= pd_unit, fmt= *)

!  largest list directed real F format exponent determined by experiment in diagnose_input_output()

   write( unit= pd_unit, fmt= fmtpri) "integer, public, parameter :: ld_fmt_fmax =", ld_max
   write( unit= pd_unit, fmt= *)

!  comma separator in list directed format determined by experiment in diagnose_input_output()

   write( unit= pd_unit, fmt= fmtpr) "!  optional separator when using list directed format"
   write( unit= pd_unit, fmt= *)
   if( has_ld_sep )then
      write( unit= pd_unit, fmt= fmtpr) "logical, public, parameter :: ld_fmt_comma = .true."
   else
      write( unit= pd_unit, fmt= fmtpr) "logical, public, parameter :: ld_fmt_comma = .false."
   endif

   write( unit= pd_unit, fmt= *)

   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)

!  optional plus sign determined by experiment in diagnose_input_output()

   write( unit= pd_unit, fmt= fmtpr) "!  optional plus sign using numeric formats"
   write( unit= pd_unit, fmt= *)

   if( plus_flag )then
      write( unit= pd_unit, fmt= fmtpr) "logical, public, parameter :: opt_plus_sign = .true."
   else
      write( unit= pd_unit, fmt= fmtpr) "logical, public, parameter :: opt_plus_sign = .false."
   endif

   write( unit= pd_unit, fmt= *)

!  optional leading zero with F format determined by experiment in diagnose_input_output()

   write( unit= pd_unit, fmt= fmtpr) "!  optional leading zero using f and e formats"
   write( unit= pd_unit, fmt= *)

   if( lz_f_flag )then
      write( unit= pd_unit, fmt= fmtpr) "logical, public, parameter :: leading_zero_f = .true."
   else
      write( unit= pd_unit, fmt= fmtpr) "logical, public, parameter :: leading_zero_f = .false."
   endif

   write( unit= pd_unit, fmt= *)

!  optional leading zero with E format determined by experiment in diagnose_input_output()

   if( lz_e_flag )then
      write( unit= pd_unit, fmt= fmtpr) "logical, public, parameter :: leading_zero_e = .true."
   else
      write( unit= pd_unit, fmt= fmtpr) "logical, public, parameter :: leading_zero_e = .false."
   endif

   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner

!  maximum formatted record size found in diagnose_input_output()

   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  the maximum record length of a sequential record"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpri) "integer, public, parameter :: max_recl =", mrecl

!  undefined direct access record iostat

   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  the iostat value returned when attempting to read a record not yet written"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpri) "integer, public, parameter :: read_undefined_record =", da_missing
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  number of words in the random number generator seed array"
   write( unit= pd_unit, fmt= fmtpr) "!  this parameter may be used to statically allocate the seed array"
   write( unit= pd_unit, fmt= *)

!  determine random number generator seed size

   call random_seed( size= rs_size)                                  ! get random seed size
   write( unit= pd_unit, fmt= fmtpri) "integer, public, parameter :: random_seed_size =", rs_size

   write( unit= pd_unit, fmt= *) 
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  true if processor uses ieee 754 floating point format"
   write( unit= pd_unit, fmt= fmtpr) "!  the intent here is _format_, not all the roundings, exceptions, &c"
   write( unit= pd_unit, fmt= *)

!  verified by check_ieee_single() or check_ieee_double()

   if( ieeefp )then                                                  ! uses ieee fp
      write( unit= pd_unit, fmt= fmtpr) "logical, public, parameter :: is_ieee_fp = .true."
   else                                                              ! uses ieee fp
      write( unit= pd_unit, fmt= fmtpr) "logical, public, parameter :: is_ieee_fp = .false."
   endif                                                             ! uses ieee fp

   write( unit= pd_unit, fmt= *) 
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  true if processor uses 2's complement integer format"
   write( unit= pd_unit, fmt= *)

!  verified by check_hardware_values()

   if( twoscomp )then                                                ! uses 2's compliment integers
      write( unit= pd_unit, fmt= fmtpr) "logical, public, parameter :: is_2s_comp = .true."
   else                                                              ! uses 2's compliment integers
      write( unit= pd_unit, fmt= fmtpr) "logical, public, parameter :: is_2s_comp = .false."
   endif                                                             ! uses 2's compliment integers

   write( unit= pd_unit, fmt= *) 

! **********************************************************************

!  write processor_dependencies procedure interfaces

! **********************************************************************

   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  processor_dependencies library"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "! The following intrinsic procedures are extended to other types:"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "! bit_size() is extended to type character, logical, real & complex."
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "! int() is extended to type logical."
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "! logical() is extended to type integer."
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "! huge() & tiny() are extended to type character and complex."
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "! epsilon() is extended to type complex."
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "! The generic name is defined for each group of procedures and each"
   write( unit= pd_unit, fmt= fmtpr) "! specific name is made private; access is only thru the generic name."
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  bit_size() specific and generic names"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  bit_size() for kinds character, logical, real (& thf complex) kinds"
   write( unit= pd_unit, fmt= *)

!  standards before f95 don't support the ::

   if( standard >= f95_std )then                                     ! add :: if f95
      write( unit= pd_unit, fmt= fmtpr) "intrinsic :: bit_size                                                ! extend intrinsic"
   else                                                              ! add :: if f95
      write( unit= pd_unit, fmt= fmtpr) "intrinsic bit_size                                                   ! extend intrinsic"
   endif                                                             ! add :: if f95

   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "public :: bit_size                                                   ! generic"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "interface bit_size"

!  bit_size() for ascii characters

   if( ascii_character% supported )then                              ! has ascii
      write( unit= pd_unit, fmt= fmtpr) "   module procedure ascii_bit_size"
   endif                                                             ! has ascii

!  bit_size() for ebcdic characters

   if( ebcdic_character% supported )then                             ! has ebcdic
      write( unit= pd_unit, fmt= fmtpr) "   module procedure ebcdic_bit_size"
   endif                                                             ! has ebcdic

!  bit_size() for iso_10646 characters

   if( iso_10646_character% supported )then                          ! has iso_10646
      write( unit= pd_unit, fmt= fmtpr) "   module procedure iso_10646_bit_size"
   endif                                                             ! has iso_10646

!  if defining logical kinds

   if( define_logicals )then                                         ! has logicals

!  bit_size() for byte logicals

      if( byte_logical% supported )then                              ! has byte logical
         write( unit= pd_unit, fmt= fmtpr) "   module procedure l_byte_bit_size"
      endif                                                          ! has byte logical

!  bit_size() for short logicals

      if( short_logical% supported )then                             ! has short logical
         write( unit= pd_unit, fmt= fmtpr) "   module procedure l_short_bit_size"
      endif                                                          ! has short logical

!  bit_size() for int logicals

      if( int_logical% supported )then                               ! has int logical
         write( unit= pd_unit, fmt= fmtpr) "   module procedure l_int_bit_size"
      endif                                                          ! has int logical

!  bit_size() for long logicals

      if( long_logical% supported )then                              ! has long logical
         write( unit= pd_unit, fmt= fmtpr) "   module procedure l_long_bit_size"
      endif                                                          ! has long logical

!  bit_size() for default logicals

   else                                                              ! has logicals
      write( unit= pd_unit, fmt= fmtpr) "   module procedure l_def_bit_size"
   endif                                                             ! has logicals

!  bit_size() for single reals

   if( real_kinds( single_idx)% supported )then                      ! has single
      write( unit= pd_unit, fmt= fmtpr) "   module procedure single_bit_size"
   endif                                                             ! has single

!  bit_size() for double reals

   if( real_kinds( double_idx)% supported )then                      ! has double
      write( unit= pd_unit, fmt= fmtpr) "   module procedure double_bit_size"
   endif                                                             ! has double

!  bit_size() for quad reals

   if( real_kinds( quad_idx)% supported )then                        ! has quad
      write( unit= pd_unit, fmt= fmtpr) "   module procedure quad_bit_size"
   endif                                                             ! has quad

!  bit_size() for single complexs

   if( real_kinds( single_idx)% supported )then                      ! has single
      write( unit= pd_unit, fmt= fmtpr) "   module procedure single_complex_bit_size"
   endif                                                             ! has single

!  bit_size() for double complexs

   if( real_kinds( double_idx)% supported )then                      ! has double
      write( unit= pd_unit, fmt= fmtpr) "   module procedure double_complex_bit_size"
   endif                                                             ! has double

!  bit_size() for quad complexs

   if( real_kinds( quad_idx)% supported )then                        ! has quad
      write( unit= pd_unit, fmt= fmtpr) "   module procedure quad_complex_bit_size"
   endif                                                             ! has quad

   write( unit= pd_unit, fmt= fmtpr) "end interface"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  int() specific and generic names"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  int() for logical kinds"
   write( unit= pd_unit, fmt= *)

!  standards before f95 don't support the ::

   if( standard >= f95_std )then                                     ! add :: if f95
      write( unit= pd_unit, fmt= fmtpr) "intrinsic :: int                                                     ! extend intrinsic"
   else                                                              ! add :: if f95
      write( unit= pd_unit, fmt= fmtpr) "intrinsic int                                                        ! extend intrinsic"
   endif                                                             ! add :: if f95

   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "public :: int                                                        ! generic"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "interface int"

!  if defining logical kinds

   if( define_logicals )then                                         ! has logicals

!  int() for byte logicals

      if( byte_logical% supported )then                              ! has byte logical
         write( unit= pd_unit, fmt= fmtpr) "   module procedure l_byte_int"
      endif                                                          ! has byte logical

!  int() for short logicals

      if( short_logical% supported )then                             ! has short logical
         write( unit= pd_unit, fmt= fmtpr) "   module procedure l_short_int"
      endif                                                          ! has short logical

!  int() for int logicals

      if( int_logical% supported )then                               ! has int logical
         write( unit= pd_unit, fmt= fmtpr) "   module procedure l_int_int"
      endif                                                          ! has int logical

!  int() for long logicals

      if( long_logical% supported )then                              ! has long logical
         write( unit= pd_unit, fmt= fmtpr) "   module procedure l_long_int"
      endif                                                          ! has long logical

!  int() for default logicals

   else                                                              ! has logicals
      write( unit= pd_unit, fmt= fmtpr) "   module procedure l_def_int"
   endif                                                             ! has logicals

   write( unit= pd_unit, fmt= fmtpr) "end interface"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  logical() specific and generic names"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  logical() for integer kinds"
   write( unit= pd_unit, fmt= *)

!  standards before f95 don't support the ::

   if( standard >= f95_std )then                                     ! add :: if f95
      write( unit= pd_unit, fmt= fmtpr) "intrinsic :: logical                                                 ! extend intrinsic"
   else                                                              ! add :: if f95
      write( unit= pd_unit, fmt= fmtpr) "intrinsic logical                                                    ! extend intrinsic"
   endif                                                             ! add :: if f95

   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "public :: logical                                                    ! generic"
   write( unit= pd_unit, fmt= *)

   write( unit= pd_unit, fmt= fmtpr) "interface logical"

!  logical() for byte integers

   if( integer_kinds( byte_idx)% supported )then                     ! has byte int
      write( unit= pd_unit, fmt= fmtpr) "   module procedure byte_logical"
   endif                                                             ! has byte int

!  logical() for short integers

   if( integer_kinds( short_idx)% supported )then                    ! has short int
      write( unit= pd_unit, fmt= fmtpr) "   module procedure short_logical"
   endif                                                             ! has short int

!  logical() for int integers

   if( integer_kinds( int_idx)% supported )then                      ! has int int
      write( unit= pd_unit, fmt= fmtpr) "   module procedure int_logical"
   endif                                                             ! has int int

!  logical() for long integers

   if( integer_kinds( long_idx)% supported )then                     ! has long int
      write( unit= pd_unit, fmt= fmtpr) "   module procedure long_logical"
   endif                                                             ! has long int

   write( unit= pd_unit, fmt= fmtpr) "end interface"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  epsilon(), huge(), tiny() specific and generic names"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  huge for character & complex kinds"
   write( unit= pd_unit, fmt= *)

!  standards before f95 don't support the ::

   if( standard >= f95_std )then                                     ! add :: if f95
      write( unit= pd_unit, fmt= fmtpr) "intrinsic :: huge                                                    ! extend intrinsic"
   else                                                              ! add :: if f95
      write( unit= pd_unit, fmt= fmtpr) "intrinsic huge                                                       ! extend intrinsic"
   endif                                                             ! add :: if f95

   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "public :: huge                                                       ! generic"
   write( unit= pd_unit, fmt= *)

   write( unit= pd_unit, fmt= fmtpr) "interface huge"

!  huge() for ascii characters

   if( ascii_character% supported )then                              ! has ascii
      write( unit= pd_unit, fmt= fmtpr) "   module procedure ascii_huge"
   endif                                                             ! has ascii

!  huge() for ebcdic characters

   if( ebcdic_character% supported )then                             ! has ebcdic
      write( unit= pd_unit, fmt= fmtpr) "   module procedure ebcdic_huge"
   endif                                                             ! has ebcdic

!  huge() for iso_10646 characters

   if( iso_10646_character% supported )then                          ! has iso_10646
      write( unit= pd_unit, fmt= fmtpr) "   module procedure iso_10646_huge"
   endif                                                             ! has iso_10646

!  huge() for single complex

   if( real_kinds( single_idx)% supported )then                      ! has single
      write( unit= pd_unit, fmt= fmtpr) "   module procedure single_complex_huge"
   endif                                                             ! has single

!  huge() for double complex

   if( real_kinds( double_idx)% supported )then                      ! has double
      write( unit= pd_unit, fmt= fmtpr) "   module procedure double_complex_huge"
   endif                                                             ! has double

!  huge() for quad complex

   if( real_kinds( quad_idx)% supported )then                        ! has quad
      write( unit= pd_unit, fmt= fmtpr) "   module procedure quad_complex_huge"
   endif                                                             ! has quad

   write( unit= pd_unit, fmt= fmtpr) "end interface"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  tiny for character & complex kinds"
   write( unit= pd_unit, fmt= *)

!  standards before f95 don't support the ::

   if( standard >= f95_std )then                                     ! add :: if f95
      write( unit= pd_unit, fmt= fmtpr) "intrinsic :: tiny                                                    ! extend intrinsic"
   else                                                              ! add :: if f95
      write( unit= pd_unit, fmt= fmtpr) "intrinsic tiny                                                       ! extend intrinsic"
   endif                                                             ! add :: if f95

   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "public :: tiny                                                       ! generic"
   write( unit= pd_unit, fmt= *)

   write( unit= pd_unit, fmt= fmtpr) "interface tiny"

!  tiny() for ascii characters

   if( ascii_character% supported )then                              ! has ascii
      write( unit= pd_unit, fmt= fmtpr) "   module procedure ascii_tiny"
   endif                                                             ! has ascii

!  tiny() for ebcdic characters

   if( ebcdic_character% supported )then                             ! has ebcdic
      write( unit= pd_unit, fmt= fmtpr) "   module procedure ebcdic_tiny"
   endif                                                             ! has ebcdic

!  tiny() for iso_10646 characters

   if( iso_10646_character% supported )then                          ! has iso_10646
      write( unit= pd_unit, fmt= fmtpr) "   module procedure iso_10646_tiny"
   endif                                                             ! has iso_10646

!  tiny() for single complex

   if( real_kinds( single_idx)% supported )then                      ! has single
      write( unit= pd_unit, fmt= fmtpr) "   module procedure single_complex_tiny"
   endif                                                             ! has single

!  tiny() for double complex

   if( real_kinds( double_idx)% supported )then                      ! has double
      write( unit= pd_unit, fmt= fmtpr) "   module procedure double_complex_tiny"
   endif                                                             ! has double

!  tiny() for quad complex

   if( real_kinds( quad_idx)% supported )then                        ! has quad
      write( unit= pd_unit, fmt= fmtpr) "   module procedure quad_complex_tiny"
   endif                                                             ! has quad

   write( unit= pd_unit, fmt= fmtpr) "end interface"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  epsilon for complex kinds"
   write( unit= pd_unit, fmt= *)

!  standards before f95 don't support the ::

   if( standard >= f95_std )then                                     ! add :: if f95
      write( unit= pd_unit, fmt= fmtpr) "intrinsic :: epsilon                                                 ! extend intrinsic"
   else                                                              ! add :: if f95
      write( unit= pd_unit, fmt= fmtpr) "intrinsic epsilon                                                    ! extend intrinsic"
   endif                                                             ! add :: if f95

   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "public :: epsilon                                                    ! generic"
   write( unit= pd_unit, fmt= *)

   write( unit= pd_unit, fmt= fmtpr) "interface epsilon"

!  epsilon() for single complex

   if( real_kinds( single_idx)% supported )then                      ! has single
      write( unit= pd_unit, fmt= fmtpr) "   module procedure single_complex_epsilon"
   endif                                                             ! has single

!  epsilon() for double complex

   if( real_kinds( double_idx)% supported )then                      ! has double
      write( unit= pd_unit, fmt= fmtpr) "   module procedure double_complex_epsilon"
   endif                                                             ! has double

!  epsilon() for quad complex

   if( real_kinds( quad_idx)% supported )then                        ! has quad
      write( unit= pd_unit, fmt= fmtpr) "   module procedure quad_complex_epsilon"
   endif                                                             ! has quad

   write( unit= pd_unit, fmt= fmtpr) "end interface"
   write( unit= pd_unit, fmt= *)

   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  max_exact_int() is the largest integer a real kind"
   write( unit= pd_unit, fmt= fmtpr) "!  can exactly increment by 1.0"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  max_exact_int() for real kinds"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "public :: max_exact_int                                              ! generic"
   write( unit= pd_unit, fmt= *)

   write( unit= pd_unit, fmt= fmtpr) "interface max_exact_int"

!  max_exact_int() for single real

   if( real_kinds( single_idx)% supported )then                      ! has single
      write( unit= pd_unit, fmt= fmtpr) "   module procedure single_max_exact_int"
   endif                                                             ! has single

!  max_exact_int() for double real

   if( real_kinds( double_idx)% supported )then                      ! has double
      write( unit= pd_unit, fmt= fmtpr) "   module procedure double_max_exact_int"
   endif                                                             ! has double

!  max_exact_int() for quad real

   if( real_kinds( quad_idx)% supported )then                        ! has quad
      write( unit= pd_unit, fmt= fmtpr) "   module procedure quad_max_exact_int"
   endif                                                             ! has quad

   write( unit= pd_unit, fmt= fmtpr) "end interface"
   write( unit= pd_unit, fmt= *)

!  defined in iso_fortran_env in f03 and later

   if( standard < f03_std )then
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  defined in iso_fortran_env with Fortran 2003"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  interpret iostat values"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "public :: is_iostat_end                                               ! export"
      write( unit= pd_unit, fmt= fmtpr) "public :: is_iostat_eor                                               ! export"
      write( unit= pd_unit, fmt= *)
   endif

!  standards before f95 might not support cpu_time()

   need_cputime: if( standard < f95_std )then                                      ! add cpu_time() if f90

      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  cpu_time() for f90 processors"
      write( unit= pd_unit, fmt= *)

      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "public :: cpu_time                                                   ! generic"
      write( unit= pd_unit, fmt= *)

      write( unit= pd_unit, fmt= fmtpr) "interface cpu_time"

!  cpu_time() for single real

      single_cputime: if( real_kinds( single_idx)% supported )then   ! has single
         write( unit= pd_unit, fmt= fmtpr) "   module procedure single_cpu_time"
      endif single_cputime                                           ! has single

!  cpu_time() for double real

      double_cputime: if( real_kinds( double_idx)% supported )then   ! has double
         write( unit= pd_unit, fmt= fmtpr) "   module procedure double_cpu_time"
      endif double_cputime                                           ! has double

!  cpu_time() for quad real

      quad_cputime: if( real_kinds( quad_idx)% supported )then       ! has quad
         write( unit= pd_unit, fmt= fmtpr) "   module procedure quad_cpu_time"
      endif quad_cputime                                             ! has quad

      write( unit= pd_unit, fmt= fmtpr) "end interface"
      write( unit= pd_unit, fmt= *)
   endif need_cputime                                                ! add cpu_time() if f90

! **********************************************************************

!  write processor_dependencies procedure declarations

! **********************************************************************

   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  module procedures"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "contains                                                             ! processor_dependencies"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  *** all integer and logical functions return default kinds ***"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  *** some functions use their argument only to select specific ***"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "! These specific procedures extend bit_size() to character, logical,"
   write( unit= pd_unit, fmt= fmtpr) "! real and complex kinds."
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  bit_size() extend intrinsic bit_size() to all defined processor kinds"
   write( unit= pd_unit, fmt= *)

!  write bit_size() for ascii characters

   if( ascii_character% supported )then                              ! has ascii
      call write_bit_size( 'character', 'ascii')
   endif                                                             ! has ascii

!  write bit_size() for ebcdic characters

   if( ebcdic_character% supported )then                             ! has ebcdic
      call write_bit_size( 'character', 'ebcdic')
   endif                                                             ! has ebcdic

!  write bit_size() for iso_10646 characters

   if( iso_10646_character% supported )then                          ! has iso_10646
      call write_bit_size( 'character', 'iso_10646')
   endif                                                             ! has iso_10646

!  if defining logical kinds

   if( define_logicals )then                                         ! has logicals

!  if has byte logical

      if( byte_logical% supported )then                              ! has byte logical

!  default logical is byte

         if( byte_logical% default_kind )then                        ! default byte logical

!  bit_size() for byte logical (default)

            write( unit= pd_unit, fmt= fmtpr) star_banner
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) "!  l_byte_bit_size() bit_size() for kind byte logical"
            write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

            if( standard >= f95_std )then                            ! add pure if f95
               write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
            endif                                                    ! add pure if f95

            write( unit= pd_unit, fmt= fmtpr) "integer function l_byte_bit_size( bl)"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) &
               "logical( kind= l_byte_k), intent( in) :: bl                          ! selects specific bit_size()"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) "!  l_byte_bit_size() text"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) &
               "continue                                                             ! bit_size()"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) &
               "   l_byte_bit_size = bit_size( 0)                                    ! 1 l_byte per nsu"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) &
               "return                                                               ! bit_size()"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) "!  l_byte_bit_size()"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) "end function l_byte_bit_size"
            write( unit= pd_unit, fmt= *)

!  if byte logical is not default kind

         else                                                        ! default byte logical

!  bit_size() for byte logical

            call write_bit_size( 'logical', 'l_byte')
         endif                                                       ! default byte logical
      endif                                                          ! has byte logical

!  if has short logical

      if( short_logical% supported )then                             ! has short logical

!  default logical is short

         if( short_logical% default_kind )then                       ! default short logical

!  bit_size() for short logical (default)

            write( unit= pd_unit, fmt= fmtpr) star_banner
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) "!  l_short_bit_size() bit_size() for kind short logical"
            write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

            if( standard >= f95_std )then                            ! add pure if f95
               write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
            endif                                                    ! add pure if f95

            write( unit= pd_unit, fmt= fmtpr) "integer function l_short_bit_size( sl)"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) &
               "logical( kind= l_short_k), intent( in) :: sl                         ! selects specific bit_size()"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) "!  l_short_bit_size() text"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) &
               "continue                                                             ! bit_size()"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) &
               "   l_short_bit_size = bit_size( 0)                                   ! 1 l_short per nsu"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) &
               "return                                                               ! bit_size()"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) "!  l_short_bit_size()"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) "end function l_short_bit_size"
            write( unit= pd_unit, fmt= *)

!  if short logical is not default kind

         else                                                        ! default short logical

!  bit_size() for short logical

            call write_bit_size( 'logical', 'l_short')
         endif                                                       ! default short logical
      endif                                                          ! has short logical

!  if has int logical

      if( int_logical% supported )then                               ! has int logical

!  default logical is int

         if( int_logical% default_kind )then                         ! default int logical

!  bit_size() for int logical (default)

            write( unit= pd_unit, fmt= fmtpr) star_banner
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) "!  l_int_bit_size() bit_size() for kind int logical"
            write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

            if( standard >= f95_std )then                            ! add pure if f95
               write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
            endif                                                    ! add pure if f95

            write( unit= pd_unit, fmt= fmtpr) "integer function l_int_bit_size( il)"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) &
               "logical( kind= l_int_k), intent( in) :: il                           ! selects specific bit_size()"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) "!  l_int_bit_size() text"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) &
               "continue                                                             ! bit_size()"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) &
               "   l_int_bit_size = bit_size( 0)                                     ! 1 l_int per nsu"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) &
               "return                                                               ! bit_size()"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) "!  l_int_bit_size()"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) "end function l_int_bit_size"
            write( unit= pd_unit, fmt= *)

!  if int logical is not default kind

         else                                         ! default int logical

!  bit_size() for int logical

            call write_bit_size( 'logical', 'l_int')
         endif                                                       ! default int logical
      endif                                                          ! has int logical

!  has long logical

      if( long_logical% supported )then                              ! has long logical

!  default logical is long

         if( long_logical% default_kind )then                        ! default long logical

!  bit_size() for long logical (default)

            write( unit= pd_unit, fmt= fmtpr) star_banner
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) "!  l_long_bit_size() bit_size() for kind long logical"
            write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

            if( standard >= f95_std )then                            ! add pure if f95
               write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
            endif                                                    ! add pure if f95

            write( unit= pd_unit, fmt= fmtpr) "integer function l_long_bit_size( ll)"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) &
               "logical( kind= l_long_k), intent( in) :: ll                          ! selects specific bit_size()"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) "!  l_long_bit_size() text"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) &
               "continue                                                             ! bit_size()"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) &
               "   l_long_bit_size = bit_size( 0)                                    ! 1 l_long per default nsu"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) &
               "return                                                               ! bit_size()"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) "!  l_long_bit_size()"
            write( unit= pd_unit, fmt= *)
            write( unit= pd_unit, fmt= fmtpr) "end function l_long_bit_size"
            write( unit= pd_unit, fmt= *)

!  if long logical is not default kind

         else                                                        ! default long logical

!  bit_size() for long logical

            call write_bit_size( 'logical', 'l_long')
         endif                                                       ! default long logical
      endif                                                          ! has long logical

   else                                                              ! has logical

!  bit_size() for default logical

      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  l_def_bit_size() bit_size() for kind int logical"
      write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

      if( standard >= f95_std )then                                  ! add pure if f95
         write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
      endif                                                          ! add pure if f95

      write( unit= pd_unit, fmt= fmtpr) "integer function l_def_bit_size( dl)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "logical, intent( in) :: dl                                           ! selects specific bit_size()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  l_def_bit_size() text"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "continue                                                             ! bit_size()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "   l_def_bit_size = bit_size( 0)                                     ! default logical per default integer"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "return                                                               ! bit_size()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  l_def_bit_size()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "end function l_def_bit_size"
      write( unit= pd_unit, fmt= *)
   endif                                                             ! has logical

!  if has single real

   if( real_kinds( single_idx)% supported )then                      ! has single

!  if single real is default kind

      if( real_kinds( single_idx)% default_kind )then                ! default single

!  bit_size() for single real (default known)

         write( unit= pd_unit, fmt= fmtpr) star_banner
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  single_bit_size() bit_size() for kind single"
         write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

         if( standard >= f95_std )then                               ! add pure if f95
            write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
         endif                                                       ! add pure if f95

         write( unit= pd_unit, fmt= fmtpr) "integer function single_bit_size( sr)"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) &
            "real( kind= single_k), intent( in) :: sr                             ! selects specific bit_size()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  single_bit_size() text"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) &
            "continue                                                             ! bit_size()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) &
            "   single_bit_size = bit_size( 0)                                    ! 1 single per default integer"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) &
            "return                                                               ! bit_size()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  single_bit_size()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "end function single_bit_size"
         write( unit= pd_unit, fmt= *)

!  if single real is not default kind

      else                                                           ! default single

!  bit_size() for single real

         call write_bit_size( 'real', 'single')
      endif                                                          ! default single
   endif                                                             ! has single

!  if has double real

   if( real_kinds( double_idx)% supported )then                      ! has double

!  if single or double real is default kind

      if( real_kinds( single_idx)% default_kind .or. real_kinds( double_idx)% default_kind )then

!  bit_size() for double real (default known)

         write( unit= pd_unit, fmt= fmtpr) star_banner
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  double_bit_size() bit_size() for kind double"
         write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

         if( standard >= f95_std )then                               ! add pure if f95
            write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
         endif                                                       ! add pure if f95

         write( unit= pd_unit, fmt= fmtpr) "integer function double_bit_size( dr)"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) &
            "real( kind= double_k), intent( in) :: dr                             ! selects specific bit_size()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  double_bit_size() text"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) &
            "continue                                                             ! bit_size()"
         write( unit= pd_unit, fmt= *)

!  double is default

         if( real_kinds( double_idx)% default_kind )then             ! default is double
            write( unit= pd_unit, fmt= fmtpr) &
               "   double_bit_size = bit_size( 0)                                    ! 1 double per default integer"

!  single is default

         else                                                        ! single is default
            write( unit= pd_unit, fmt= fmtpr) &
               "   double_bit_size = 2 * bit_size( 0)                                ! 1 double per 2 default integers"
         endif                                                       ! corresponds to double

         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) &
            "return                                                               ! bit_size()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  double_bit_size()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "end function double_bit_size"
         write( unit= pd_unit, fmt= *)

!  single or double real is not the default kind

      else                                                           ! default single or double

!  bit_size() for double real

         call write_bit_size( 'real', 'double')
      endif                                                          ! default single or double
   endif                                                             ! default double

!  if has quad real

   if( real_kinds( quad_idx)% supported )then                        ! has quad

!  if default is double or quad real

      if( real_kinds( double_idx)% default_kind .or. real_kinds( quad_idx)% default_kind )then

!  bit_size() for quad real (default known)

         write( unit= pd_unit, fmt= fmtpr) star_banner
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  quad_bit_size() bit_size() for kind quad"
         write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

         if( standard >= f95_std )then                               ! add pure if f95
            write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
         endif                                                       ! add pure if f95

         write( unit= pd_unit, fmt= fmtpr) "integer function quad_bit_size( qr)"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) &
            "real( kind= quad_k), intent( in) :: qr                               ! selects specific bit_size()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  quad_bit_size() text"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) &
            "continue                                                             ! bit_size()"
         write( unit= pd_unit, fmt= *)

!  default is quad real

         if( real_kinds( quad_idx)% default_kind )then               ! quad is default
            write( unit= pd_unit, fmt= fmtpr) &
               "   quad_bit_size = bit_size( 0)                                      ! 1 quad per default integer"

!  default is double real

         else                                                        ! double is default
            write( unit= pd_unit, fmt= fmtpr) &
               "   quad_bit_size = 2 * bit_size( 0)                                  ! 1 quad per 2 default integers"
         endif                                                       ! default kind

         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) &
            "return                                                               ! bit_size()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  quad_bit_size()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "end function quad_bit_size"
         write( unit= pd_unit, fmt= *)

!  if default is not double or quad real

      else                                                           ! default double or quad

!  bit_size() for quad real

         call write_bit_size( 'real', 'quad')
      endif                                                          ! default double or quad
   endif                                                             ! has quad

!  has single complex

   if( real_kinds( single_idx)% supported )then                      ! has single
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  single_complex_bit_size() bit_size() for kind single_complex"
      write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

      if( standard >= f95_std .and. real_kinds( single_idx)% default_kind )then
         write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
      endif                                                          ! add pure if f95

      write( unit= pd_unit, fmt= fmtpr) "integer function single_complex_bit_size( sc)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "complex( kind= single_k), intent( in) :: sc                          ! selects specific bit_size()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  single_complex_bit_size()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "continue                                                             ! bit_size()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "   single_complex_bit_size = 2 * single_bit_size( 0.0_single_k)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "return                                                               ! bit_size()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  single_complex_bit_size()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "end function single_complex_bit_size"
      write( unit= pd_unit, fmt= *)
   endif                                                             ! has single

! has double complex

   if( real_kinds( double_idx)% supported )then                      ! has double
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  double_complex_bit_size() bit_size() for kind double_complex"
      write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

      if( standard >= f95_std .and. &
          ( real_kinds( single_idx)% default_kind .or. real_kinds( double_idx)% default_kind) )then
         write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
      endif                                                          ! add pure if f95

      write( unit= pd_unit, fmt= fmtpr) "integer function double_complex_bit_size( dc)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "complex( kind= double_k), intent( in) :: dc                          ! selects specific bit_size()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  double_complex_bit_size()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "continue                                                             ! bit_size()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "   double_complex_bit_size = 2 * double_bit_size( 0.0_double_k)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "return                                                               ! bit_size()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  double_complex_bit_size()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "end function double_complex_bit_size"
      write( unit= pd_unit, fmt= *)
   endif                                                             ! has double

!  has quad complex

   if( real_kinds( quad_idx)% supported )then                        ! has quad
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  quad_complex_bit_size() bit_size() for kind quad_complex"
      write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

      if( standard >= f95_std .and. real_kinds( double_idx)% default_kind )then
         write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
      endif                                                          ! add pure if f95

      write( unit= pd_unit, fmt= fmtpr) "integer function quad_complex_bit_size( qc)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "complex( kind= quad_k), intent( in) :: qc                            ! selects specific bit_size()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  quad_complex_bit_size()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "continue                                                             ! bit_size()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "   quad_complex_bit_size = 2 * quad_bit_size( 0.0_quad_k)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "return                                                               ! bit_size()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  quad_complex_bit_size()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "end function quad_complex_bit_size"
      write( unit= pd_unit, fmt= *)
   endif                                                             ! has quad

   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  int() extend intrinsic int() to all logical kinds"
   write( unit= pd_unit, fmt= *)

!  if defining logical kinds

   if( define_logicals )then                                         ! has logicals

!  has byte logical

      if( byte_logical% supported )then                              ! has l_byte
         write( unit= pd_unit, fmt= fmtpr) star_banner
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  l_byte_int() int() for kind l_byte"
         write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

         if( standard >= f95_std )then                               ! add elemental if f95
            write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
         endif                                                       ! add elemental if f95

         write( unit= pd_unit, fmt= fmtpr) "integer function l_byte_int( bl)"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "logical( kind= l_byte_k), intent( in) :: bl"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  l_byte_int()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) &
            "continue                                                             ! int()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "   true_or_false: select case( bl)                                   ! t or f"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "   case( .true._l_byte_k) true_or_false                              ! true"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "      l_byte_int = 1                                                 ! is 1"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "   case( .false._l_byte_k) true_or_false                             ! false"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "      l_byte_int = 0                                                 ! is 0"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "   end select true_or_false                                          ! t or f"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) &
            "return                                                               ! int()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  l_byte_int()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "end function l_byte_int"
         write( unit= pd_unit, fmt= *)
      endif                                                          ! has l_byte

!  has short logical

      if( short_logical% supported )then                             ! has l_short
         write( unit= pd_unit, fmt= fmtpr) star_banner
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  l_short_int() int() for kind l_short"
         write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

         if( standard >= f95_std )then                               ! add elemental if f95
            write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
         endif                                                       ! add elemental if f95

         write( unit= pd_unit, fmt= fmtpr) "integer function l_short_int( sl)"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "logical( kind= l_short_k), intent( in) :: sl"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  l_short_int()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) &
            "continue                                                             ! int()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "   true_or_false: select case( sl)                                   ! true"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "   case( .true._l_short_k) true_or_false                             ! true"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "      l_short_int = 1                                                ! is 1"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "   case( .false._l_short_k) true_or_false                            ! false"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "      l_short_int = 0                                                ! is 0"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "   end select true_or_false                                          ! t or f"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) &
            "return                                                               ! int()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  l_short_int()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "end function l_short_int"
         write( unit= pd_unit, fmt= *)
      endif                                                          ! has l_short

!  has int logical

      if( int_logical% supported )then                               ! has l_int
         write( unit= pd_unit, fmt= fmtpr) star_banner
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  l_int_int() int() for kind l_int"
         write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

         if( standard >= f95_std )then                               ! add elemental if f95
            write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
         endif                                                       ! add elemental if f95

         write( unit= pd_unit, fmt= fmtpr) "integer function l_int_int( il)"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "logical( kind= l_int_k), intent( in) :: il"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  l_int_int()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) &
            "continue                                                             ! int()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "   true_or_false: select case( il)                                   ! true"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "   case( .true._l_int_k) true_or_false                               ! true"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "      l_int_int = 1                                                  ! is 1"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "   case( .false._l_int_k) true_or_false                              ! false"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "      l_int_int = 0                                                  ! is 0"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "   end select true_or_false                                          ! t or f"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) &
            "return                                                               ! int()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  l_int_int()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "end function l_int_int"
         write( unit= pd_unit, fmt= *)
      endif                                                          ! has l_int

!  has long logical

      if( long_logical% supported )then                              ! has l_long
         write( unit= pd_unit, fmt= fmtpr) star_banner
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  l_long_int() int() for kind l_long"
         write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

         if( standard >= f95_std )then                               ! add elemental if f95
            write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
         endif                                                       ! add elemental if f95

         write( unit= pd_unit, fmt= fmtpr) "integer function l_long_int( ll)"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "logical( kind= l_long_k), intent( in) :: ll"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  l_long_int()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) &
            "continue                                                             ! int()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "   true_or_false: select case( ll)                                   ! true"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "   case( .true._l_long_k) true_or_false                              ! true"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "      l_long_int = 1                                                 ! is 1"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "   case( .false._l_long_k) true_or_false                             ! false"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "      l_long_int = 0                                                 ! is 0"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "   end select true_or_false                                          ! t or f"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) &
            "return                                                               ! int()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  l_long_int()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "end function l_long_int"
         write( unit= pd_unit, fmt= *)
      endif                                                          ! has l_long

!  if not defining logical make for default logical

   else                                                              ! has logicals
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  l_def_int() int() for kind l_def"
      write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

      if( standard >= f95_std )then                                  ! add elemental if f95
         write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
      endif                                                          ! add elemental if f95

      write( unit= pd_unit, fmt= fmtpr) "integer function l_def_int( dl)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "logical, intent( in) :: dl"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  l_def_int()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "continue                                                             ! int()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "   true_or_false: select case( dl)                                   ! true"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "   case( .true.) true_or_false                                       ! true"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "      l_def_int = 1                                                  ! is 1"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "   case( .false.) true_or_false                                      ! false"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "      l_def_int = 0                                                  ! is 0"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "   end select true_or_false                                          ! t or f"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "return                                                               ! int()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  l_def_int()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "end function l_def_int"
      write( unit= pd_unit, fmt= *)
   endif                                                             ! has logicals

   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  logical() extend intrinsic logical() to all integer kinds"
   write( unit= pd_unit, fmt= *)

!  has byte integer

   if( integer_kinds( byte_idx)% supported )then                     ! has byte
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  byte_logical() logical() for kind byte"
      write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

      if( standard >= f95_std )then                                  ! add elemental if f95
         write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
      endif                                                          ! add elemental if f95

      write( unit= pd_unit, fmt= fmtpr) "logical function byte_logical( ib)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "integer( kind= byte_k), intent( in) :: ib"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  byte_logical()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "continue                                                             ! logical()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "   byte_logical = ib /= 0_byte_k                                     ! 0 is false"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "return                                                               ! logical()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  byte_logical()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "end function byte_logical"
      write( unit= pd_unit, fmt= *)
   endif                                                             ! has byte

!  has short integer

   if( integer_kinds( short_idx)% supported )then                    ! has short
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  short_logical() logical() for kind short"
      write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

      if( standard >= f95_std )then                                  ! add elemental if f95
         write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
      endif                                                          ! add elemental if f95

      write( unit= pd_unit, fmt= fmtpr) "logical function short_logical( is)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "integer( kind= short_k), intent( in) :: is"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  short_logical()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "continue                                                             ! logical()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "   short_logical = is /= 0_short_k                                   ! 0 is false"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "return                                                               ! logical()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  short_logical()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "end function short_logical"
      write( unit= pd_unit, fmt= *)
   endif                                                             ! has short

!  has int integer

   if( integer_kinds( int_idx)% supported )then                      ! has int
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  int_logical() logical() for kind int"
      write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

      if( standard >= f95_std )then                                  ! add elemental if f95
         write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
      endif                                                          ! add elemental if f95

      write( unit= pd_unit, fmt= fmtpr) "logical function int_logical( ii)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "integer( kind= int_k), intent( in) :: ii"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  int_logical()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "continue                                                             ! logical()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "   int_logical = ii /= 0_int_k                                       ! 0 is false"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "return                                                               ! logical()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  int_logical()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "end function int_logical"
      write( unit= pd_unit, fmt= *)
   endif                                                             ! has int

!  has long integer

   if( integer_kinds( long_idx)% supported )then                     ! has long
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  long_logical() logical() for kind long"
      write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

      if( standard >= f95_std )then                                  ! add elemental if f95
         write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
      endif                                                          ! add elemental if f95

      write( unit= pd_unit, fmt= fmtpr) "logical function long_logical( il)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "integer( kind= long_k), intent( in) :: il"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  long_logical()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "continue                                                             ! logical()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "   long_logical = il /= 0_long_k                                     ! 0 is false"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) &
         "return                                                               ! logical()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  long_logical()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "end function long_logical"
      write( unit= pd_unit, fmt= *)
   endif                                                             ! has long

   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "! These specific procedures extend huge() and tiny() to character (if"
   write( unit= pd_unit, fmt= fmtpr) "! defined) and complex kinds.  The character huge() and tiny() return"
   write( unit= pd_unit, fmt= fmtpr) "! the characters with the largest and smallest character codes.  The"
   write( unit= pd_unit, fmt= fmtpr) "! complex huge() and tiny() return the square root of their real"
   write( unit= pd_unit, fmt= fmtpr) "! counterparts.  The choice of sqrts defines the range where magnitude"
   write( unit= pd_unit, fmt= fmtpr) "! comparisons are valid.  [ |z| = sqrt( r^2 + i^2) ]  So if two"
   write( unit= pd_unit, fmt= fmtpr) "! complex numbers have components within sqrt( real epsilon) of each"
   write( unit= pd_unit, fmt= fmtpr) "! other, they will compare with equal magnitude."
   write( unit= pd_unit, fmt= *)

!  if defining character kinds

   if( define_characters )then                                       ! wants character kinds
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  huge()/tiny() for character kinds"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  The ascii huge and tiny return characters with the largest and"
      write( unit= pd_unit, fmt= fmtpr) "!  smallest character codes."
      write( unit= pd_unit, fmt= *)

!  has ascii character

      if( ascii_character% supported )then                          ! has ascii
         write( unit= pd_unit, fmt= fmtpr) star_banner
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  ascii_huge() huge() for kind ascii"
         write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

         if( standard >= f95_std )then                               ! add pure if f95
            write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
         endif                                                       ! add pure if f95

         write( unit= pd_unit, fmt= fmtpr) "character( len= 1, kind= ascii_k) function ascii_huge( ac)"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "character( len= *, kind= ascii_k), intent( in) :: ac"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  ascii_huge()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "continue                                                             ! huge()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) &
            "   ascii_huge = achar( 127)                                          ! largest is del"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "return                                                               ! huge()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  ascii_huge()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "end function ascii_huge"
         write( unit= pd_unit, fmt= *)

         write( unit= pd_unit, fmt= fmtpr) star_banner
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  ascii_tiny() tiny() for kind ascii"
         write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

         if( standard >= f95_std )then                               ! add pure if f95
            write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
         endif                                                       ! add pure if f95

         write( unit= pd_unit, fmt= fmtpr) "character( len= 1, kind= ascii_k) function ascii_tiny( ac)"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "character( len= *, kind= ascii_k), intent( in) :: ac"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  ascii_tiny()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "continue                                                             ! tiny()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) &
            "   ascii_tiny = achar( 0)                                            ! smallest is nul"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "return                                                               ! tiny()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  ascii_tiny()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "end function ascii_tiny"
         write( unit= pd_unit, fmt= *)
      endif                                                          ! has ascii

!  has ebcdic

      if( ebcdic_character% supported )then                          ! has ebcdic
         write( unit= pd_unit, fmt= fmtpr) star_banner
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  ebcdic_huge() huge() for kind ebcdic"
         write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

         if( standard >= f95_std )then                               ! add pure if f95
            write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
         endif                                                       ! add pure if f95

         write( unit= pd_unit, fmt= fmtpr) "character( len= 1, kind= ebcdic_k) function ebcdic_huge( ic)"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "character( len= *, kind= ebcdic_k), intent( in) :: ic"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  ebcdic_huge()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "continue                                                             ! huge()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) &
            "   ebcdic_huge = char( 255, kind= ebcdic_k)                        ! largest is 255"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "return                                                               ! huge()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  ebcdic_huge()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "end function ebcdic_huge"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) star_banner
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  ebcdic_tiny() tiny() for kind ebcdic"
         write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

         if( standard >= f95_std )then                               ! add pure if f95
            write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
         endif                                                       ! add pure if f95

         write( unit= pd_unit, fmt= fmtpr) "character( len= 1, kind= ebcdic_k) function ebcdic_tiny( ic)"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "character( len= *, kind= ebcdic_k), intent( in) :: ic"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  ebcdic_tiny()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "continue                                                             ! tiny()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) &
            "   ebcdic_tiny = char( 0, kind= ebcdic_k)                          ! smallest is zero"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "return                                                               ! tiny()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  ebcdic_tiny()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "end function ebcdic_tiny"
         write( unit= pd_unit, fmt= *)
      endif                                                          ! has ebcdic

!  has iso_10646

      if( iso_10646_character% supported )then                       ! has iso_10646
         write( unit= pd_unit, fmt= fmtpr) star_banner
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  iso_10646_huge() huge() for kind iso_10646"
         write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

         if( standard >= f95_std )then                               ! add pure if f95
            write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
         endif                                                       ! add pure if f95

         write( unit= pd_unit, fmt= fmtpr) "character( len= 1, kind= iso_10646_k) function iso_10646_huge( uc)"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "character( len= *, kind= iso_10646_k), intent( in) :: uc"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  iso_10646_huge()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "continue                                                             ! huge()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) &
            "   iso_10646_huge = char( huge( 0), kind= iso_10646_k)                ! largest is huge( 0)"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "return                                                               ! huge()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  iso_10646_huge()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "end function iso_10646_huge"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) star_banner
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  iso_10646_tiny() tiny() for kind iso_10646"
         write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

         if( standard >= f95_std )then                               ! add pure if f95
            write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
         endif                                                       ! add pure if f95

         write( unit= pd_unit, fmt= fmtpr) "character( len= 1, kind= iso_10646_k) function iso_10646_tiny( uc)"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "character( len= *, kind= iso_10646_k), intent( in) :: uc"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  iso_10646_tiny()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "continue                                                             ! tiny()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) &
            "   iso_10646_tiny = char( 0, kind= iso_10646_k)                       ! smallest is zero"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "return                                                               ! tiny()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "!  iso_10646_tiny()"
         write( unit= pd_unit, fmt= *)
         write( unit= pd_unit, fmt= fmtpr) "end function iso_10646_tiny"
         write( unit= pd_unit, fmt= *)
      endif                                                          ! has iso_10646
   endif                                                             ! wants character kinds

   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "! These specific procedures extend huge() to type complex.  huge()"
   write( unit= pd_unit, fmt= fmtpr) "! returns the square root of its real counterpart.  The sqrt is returned"
   write( unit= pd_unit, fmt= fmtpr) "! so magnitude comparisons may be made [ sqrt( x^2 + y^2) = |z| < huge()"
   write( unit= pd_unit, fmt= fmtpr) "! or sqrt( x^2 + y^2) = |z| > tiny() ]"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  huge() for complex kinds"
   write( unit= pd_unit, fmt= *)

!  has single complex

   if( real_kinds( single_idx)% supported )then                      ! has single
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  single_complex_huge() huge() for kind single complex"
      write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

      if( standard >= f95_std )then                                  ! add pure if f95
         write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
      endif                                                          ! add pure if f95

      write( unit= pd_unit, fmt= fmtpr) "real( kind= single_k) function single_complex_huge( sc)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "complex( kind= single_k), intent( in) :: sc"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  single_complex_huge()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "continue                                                             ! huge()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "   single_complex_huge = sqrt( huge( real( sc)) )"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "return                                                               ! huge()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  single_complex_huge()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "end function single_complex_huge"
      write( unit= pd_unit, fmt= *)
   endif                                                             ! has single

!  has double complex

   if( real_kinds( double_idx)% supported )then                      ! has double
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  double_complex_huge() huge() for kind double complex"
      write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

      if( standard >= f95_std )then                                  ! add pure if f95
         write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
      endif                                                          ! add pure if f95

      write( unit= pd_unit, fmt= fmtpr) "real( kind= double_k) function double_complex_huge( dc)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "complex( kind= double_k), intent( in) :: dc"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  double_complex_huge()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "continue                                                             ! huge()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "   double_complex_huge = sqrt( huge( real( dc)) )"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "return                                                               ! huge()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  double_complex_huge()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "end function double_complex_huge"
      write( unit= pd_unit, fmt= *)
   endif                                                             ! has double

!  has quad complex

   if( real_kinds( quad_idx)% supported )then                        ! has quad
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  quad_complex_huge() huge() for kind quad complex"
      write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

      if( standard >= f95_std )then                                  ! add pure if f95
         write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
      endif                                                          ! add pure if f95

      write( unit= pd_unit, fmt= fmtpr) "real( kind= quad_k) function quad_complex_huge( qc)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "complex( kind= quad_k), intent( in) :: qc"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  quad_complex_huge()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "continue                                                             ! huge()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "   quad_complex_huge = sqrt( huge( real( qc)) )"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "return                                                               ! huge()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  quad_complex_huge()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "end function quad_complex_huge"
      write( unit= pd_unit, fmt= *)
   endif                                                             ! has quad

   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "! These specific procedures extend tiny() to type complex.  tiny()"
   write( unit= pd_unit, fmt= fmtpr) "! returns the square root of its real counterpart."
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  tiny() for complex kinds"
   write( unit= pd_unit, fmt= *)

!  has single complex

   if( real_kinds( single_idx)% supported )then                      ! has single
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  single_complex_tiny() tiny() for kind single complex"
      write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

      if( standard >= f95_std )then                                  ! add pure if f95
         write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
      endif                                                          ! add pure if f95

      write( unit= pd_unit, fmt= fmtpr) "real( kind= single_k) function single_complex_tiny( sc)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "complex( kind= single_k), intent( in) :: sc"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  single_complex_tiny()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "continue                                                             ! tiny()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "   single_complex_tiny = sqrt( tiny( real( sc)) )"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "return                                                               ! tiny()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  single_complex_tiny()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "end function single_complex_tiny"
      write( unit= pd_unit, fmt= *)
   endif                                                             ! has single

!  has double complex

   if( real_kinds( double_idx)% supported )then                      ! has double
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  double_complex_tiny() tiny() for kind double complex"
      write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

      if( standard >= f95_std )then                                  ! add pure if f95
         write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
      endif                                                          ! add pure if f95

      write( unit= pd_unit, fmt= fmtpr) "real( kind= double_k) function double_complex_tiny( dc)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "complex( kind= double_k), intent( in) :: dc"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  double_complex_tiny()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "continue                                                             ! tiny()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "   double_complex_tiny = sqrt( tiny( real( dc)) )"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "return                                                               ! tiny()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  double_complex_tiny()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "end function double_complex_tiny"
      write( unit= pd_unit, fmt= *)
   endif                                                             ! has double

!  has quad complex

   if( real_kinds( quad_idx)% supported )then                        ! has quad
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  quad_complex_tiny() tiny() for kind quad complex"
      write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

      if( standard >= f95_std )then                                  ! add pure if f95
         write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
      endif                                                          ! add pure if f95

      write( unit= pd_unit, fmt= fmtpr) "real( kind= quad_k) function quad_complex_tiny( qc)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "complex( kind= quad_k), intent( in) :: qc"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  quad_complex_tiny()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "continue                                                             ! tiny()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "   quad_complex_tiny = sqrt( tiny( real( qc)) )"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "return                                                               ! tiny()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  quad_complex_tiny()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "end function quad_complex_tiny"
      write( unit= pd_unit, fmt= *)
   endif                                                             ! has quad

   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "! These specific procedures extend epsilon() to type complex.  epsilon()"
   write( unit= pd_unit, fmt= fmtpr) "! returns the square root of its real counterpart.  The magnitude of"
   write( unit= pd_unit, fmt= fmtpr) "! two complex numbers differ if the numbers differ by more than epsilon()"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  epsilon() for complex kinds"
   write( unit= pd_unit, fmt= *)

!  has single complex

   if( real_kinds( single_idx)% supported )then                      ! has single
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  single_complex_epsilon() epsilon() for kind single complex"
      write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

      if( standard >= f95_std )then                                  ! add pure if f95
         write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
      endif                                                          ! add pure if f95

      write( unit= pd_unit, fmt= fmtpr) "real( kind= single_k) function single_complex_epsilon( sc)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "complex( kind= single_k), intent( in) :: sc"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  single_complex_epsilon()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "continue                                                             ! epsilon()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "   single_complex_epsilon = sqrt( epsilon( abs( sc)) )"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "return                                                               ! epsilon()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  single_complex_epsilon()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "end function single_complex_epsilon"
      write( unit= pd_unit, fmt= *)
   endif                                                             ! has single

!  has double complex

   if( real_kinds( double_idx)% supported )then                      ! has double
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  double_complex_epsilon() epsilon() for kind double complex"
      write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

      if( standard >= f95_std )then                                  ! add pure if f95
         write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
      endif                                                          ! add pure if f95

      write( unit= pd_unit, fmt= fmtpr) "real( kind= double_k) function double_complex_epsilon( dc)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "complex( kind= double_k), intent( in) :: dc"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  double_complex_epsilon()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "continue                                                             ! epsilon()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "   double_complex_epsilon = sqrt( epsilon( abs( dc)) )"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "return                                                               ! epsilon()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  double_complex_epsilon()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "end function double_complex_epsilon"
      write( unit= pd_unit, fmt= *)
   endif                                                             ! has double

!  has quad complex

   if( real_kinds( quad_idx)% supported )then                        ! has quad
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  quad_complex_epsilon() epsilon() for kind quad complex"
      write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

      if( standard >= f95_std )then                                  ! add pure if f95
         write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
      endif                                                          ! add pure if f95

      write( unit= pd_unit, fmt= fmtpr) "real( kind= quad_k) function quad_complex_epsilon( qc)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "complex( kind= quad_k), intent( in) :: qc"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  quad_complex_epsilon()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "continue                                                             ! epsilon()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "   quad_complex_epsilon = sqrt( epsilon( abs( qc)) )"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "return                                                               ! epsilon()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  quad_complex_epsilon()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "end function quad_complex_epsilon"
      write( unit= pd_unit, fmt= *)
   endif                                                             ! has quad

   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "! These specific procedures compute the largest integer"
   write( unit= pd_unit, fmt= fmtpr) "! which may be stored exactly in a real kind."
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  max_exact_int() for real kinds"
   write( unit= pd_unit, fmt= *)

!  has single real

   if( real_kinds( single_idx)% supported )then                      ! has single
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  single_max_exact_int() max_exact_int() for kind single real"
      write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

      if( standard >= f95_std )then                                  ! add pure if f95
         write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
      endif                                                          ! add pure if f95

      write( unit= pd_unit, fmt= fmtpr) "real( kind= single_k) function single_max_exact_int( sr)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "real( kind= single_k), intent( in) :: sr"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  single_max_exact_int()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "continue                                                             ! max_exact_int()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "   single_max_exact_int = nearest( real( radix( sr), kind= single_k) &"
      write( unit= pd_unit, fmt= fmtpr) "                       ** real( digits( sr), kind= single_k), -1.0_single_k)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "return                                                               ! max_exact_int()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  single_max_exact_int()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "end function single_max_exact_int"
      write( unit= pd_unit, fmt= *)
   endif                                                             ! has single

!  has double real

   if( real_kinds( double_idx)% supported )then                      ! has double
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  double_max_exact_int() max_exact_int() for kind double real"
      write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

      if( standard >= f95_std )then                                  ! add pure if f95
         write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
      endif                                                          ! add pure if f95

      write( unit= pd_unit, fmt= fmtpr) "real( kind= double_k) function double_max_exact_int( dr)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "real( kind= double_k), intent( in) :: dr"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  double_max_exact_int()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "continue                                                             ! max_exact_int()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "   double_max_exact_int = nearest( real( radix( dr), kind= double_k) &"
      write( unit= pd_unit, fmt= fmtpr) "                       ** real( digits( dr), kind= double_k), -1.0_double_k)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "return                                                               ! max_exact_int()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  double_max_exact_int()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "end function double_max_exact_int"
      write( unit= pd_unit, fmt= *)
   endif                                                             ! has double

!  has quad real

   if( real_kinds( quad_idx)% supported )then                        ! has quad
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  quad_max_exact_int() epsilon() for kind quad real"
      write( unit= pd_unit, fmt= *)

!  standards before f95 don't support elemental

      if( standard >= f95_std )then                                  ! add pure if f95
         write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "elemental "
      endif                                                          ! add pure if f95

      write( unit= pd_unit, fmt= fmtpr) "real( kind= quad_k) function quad_max_exact_int( qr)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "real( kind= quad_k), intent( in) :: qr"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  quad_max_exact_int()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "continue                                                             ! max_exact_int()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "   quad_max_exact_int = nearest( real( radix( qr), kind= quad_k) &"
      write( unit= pd_unit, fmt= fmtpr) "                     ** real( digits( qr), kind= quad_k), -1.0_quad_k)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "return                                                               ! max_exact_int()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  quad_max_exact_int()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "end function quad_max_exact_int"
      write( unit= pd_unit, fmt= *)
   endif                                                             ! has quad

!  these are part of iso_fortran_env starting with f03

   if( standard < f03_std )then
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "! The following functions detect the conditions indicated according"
      write( unit= pd_unit, fmt= fmtpr) "! to rules specified by the Fortran standard.  The iostat return"
      write( unit= pd_unit, fmt= fmtpr) "! value is required to be an error code, an end code, or a success code."
      write( unit= pd_unit, fmt= *)

      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  is_iostat_eof(), is_iostat_eor() detect standard conditions"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  is_iostat_end() true if iostat indicates error"
      write( unit= pd_unit, fmt= *)

!  standards before f95 don't support pure

      if( standard >= f95_std )then                                  ! add pure if f95
         write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "pure "
      endif                                                          ! add pure if f95

      write( unit= pd_unit, fmt= fmtpr) "logical function is_iostat_end( iostat)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "integer, intent( in) :: iostat"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  is_iostat_end()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "continue                                                             ! is_iostat_end()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "   is_iostat_end = any( (/ iostat_end /) == iostat)              ! eof condition"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "return                                                               ! is_iostat_end()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  is_iostat_end()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "end function is_iostat_end"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) star_banner
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  is_iostat_eor() true if iostat indicates error"
      write( unit= pd_unit, fmt= *)

!  standards before f95 don't support pure

      if( standard >= f95_std )then                                  ! add pure if f95
         write( unit= pd_unit, fmt= fmtpr, advance= 'NO') "pure "
      endif                                                          ! add pure if f95

      write( unit= pd_unit, fmt= fmtpr) "logical function is_iostat_eor( iostat)"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "integer, intent( in) :: iostat"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  is_iostat_eor()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "continue                                                             ! is_iostat_eor()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "   is_iostat_eor = any( (/ iostat_eor /) == iostat)              ! eor condition"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "return                                                               ! is_iostat_eor()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "!  is_iostat_eor()"
      write( unit= pd_unit, fmt= *)
      write( unit= pd_unit, fmt= fmtpr) "end function is_iostat_eor"
      write( unit= pd_unit, fmt= *)
   endif

   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "!  processor_dependencies"
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "! $Id: make_pm.f90 2.2 2005/05/30 15:47:01Z Dan Release $"
   write( unit= pd_unit, fmt= fmtpr) star_banner
   write( unit= pd_unit, fmt= *)
   write( unit= pd_unit, fmt= fmtpr) "end module processor_dependencies                                            ! eof"

!  end of stdtype.f90

   close( unit= pd_unit, iostat= istat, status= 'KEEP')

   close_error: if( istat > 0 )then                                  ! error closing stdtype.f90

      write( unit= log_unit, fmt= fmtpr) 'trouble closing ' // trim( pdname)

   endif close_error                                                 ! error closing stdtype.f90

return                                                               ! write_processor_dependencies()

!  write_processor_dependencies()

end subroutine write_processor_dependencies

! **********************************************************************

!  write_processor_model() writes prototype program_model

subroutine write_processor_model( istat)

! **********************************************************************

!  write_processor_model() interface

integer, intent( out) :: istat                                       ! status

! **********************************************************************

!  write_processor_model() text

continue                                                             ! write_processor_model()

!  write a new processor_model

   open( unit= pm_unit, file= pmname, iostat= istat, action= 'WRITE', status= 'REPLACE')

   open_error: if( istat > 0 )then                                   ! can't open pm.f90

      write( unit= log_unit, fmt= fmtpr) cant_open // trim( pmname)

      return                                                         ! write_processor_model()

   endif open_error                                                  ! can't open pm.f90

!  write pm.f90

   write( unit= pm_unit, fmt= fmtpr) "! bof"
   write( unit= pm_unit, fmt= fmtpr) star_banner
   write( unit= pm_unit, fmt= fmtpr) "! Fortran 95 program processor_model"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) star_banner
   write( unit= pm_unit, fmt= fmtpr) "! Source Control Strings"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "! $Id: make_pm.f90 2.2 2005/05/30 15:47:01Z Dan Release $"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) star_banner
   write( unit= pm_unit, fmt= fmtpr) "!  Copyright 2004 Purple Sage Computing Solutions, Inc."
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) star_banner
   write( unit= pm_unit, fmt= fmtpr) "! Summary of License"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!   This program is free software; you can redistribute it and/or modify"
   write( unit= pm_unit, fmt= fmtpr) "!   it under the terms of the GNU General Public License as published by"
   write( unit= pm_unit, fmt= fmtpr) "!   the Free Software Foundation; either version 2 of the License, or"
   write( unit= pm_unit, fmt= fmtpr) "!   (at your option) any later version."
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!   This program is distributed in the hope that it will be useful,"
   write( unit= pm_unit, fmt= fmtpr) "!   but WITHOUT ANY WARRANTY; without even the implied warranty of"
   write( unit= pm_unit, fmt= fmtpr) "!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
   write( unit= pm_unit, fmt= fmtpr) "!   GNU General Public License for more details."
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!   You should have received a copy of the GNU General Public License"
   write( unit= pm_unit, fmt= fmtpr) "!   along with this program; if not, write to the Free Software"
   write( unit= pm_unit, fmt= fmtpr) "!   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA."
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "! To report bugs, suggest enhancements, etc. to the Authors,"
   write( unit= pm_unit, fmt= fmtpr) "! Contact:"
   write( unit= pm_unit, fmt= fmtpr) "!    Purple Sage Computing Solutions, Inc."
   write( unit= pm_unit, fmt= fmtpr) "!                               send email to dnagle@erols.com"
   write( unit= pm_unit, fmt= fmtpr) "!                                   or fax to 703 471 0684 (USA)"
   write( unit= pm_unit, fmt= fmtpr) "!                                  or mail to 12142 Purple Sage Ct."
   write( unit= pm_unit, fmt= fmtpr) "!                                             Reston, VA 20194-5621 USA"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) star_banner
   write( unit= pm_unit, fmt= fmtpr) "! prints a description of the processor model to output_unit"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) star_banner
   write( unit= pm_unit, fmt= fmtpr) "! use processor_dependencies"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) star_banner
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  processor_model uses"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!     processor_dependencies describes the processor"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  processor_model reads"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!     none"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  processor_model writes"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!     output_unit all output"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  processor_model library"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!     none"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) star_banner
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  processor model"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) star_banner
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "program processor_model"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) star_banner
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  use standard kind definitions, &c"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "use processor_dependencies"

   if( standard >= f03_std )then
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "use, intrinsic :: iso_fortran_env"
   endif

   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) star_banner
   write( unit= pm_unit, fmt= fmtpr) "!  declare all variables"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "implicit none"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) star_banner
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  processor_model RCS strings"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) star_banner
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  program source filename supplied by RCS"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "character (len= *), parameter :: processor_model_rcs_id = &"
   write( unit= pm_unit, fmt= fmtpr) "   '$Id: make_pm.f90 2.2 2005/05/30 15:47:01Z Dan Release $'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) star_banner
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "character( len= *), parameter :: &"
   write( unit= pm_unit, fmt= fmtpr) "   processor_model_timestamp = " // "'" // trim( timestamp) // "'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) star_banner
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  processor_model data"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) star_banner
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  date_and_time() character buffers"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   character( len= 8) :: dbuff"
   write( unit= pm_unit, fmt= fmtpr) "   character( len= 10) :: tbuff"
   write( unit= pm_unit, fmt= fmtpr) "   character( len= 5) :: zbuff"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  date_and_time() values array"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   integer, dimension( 8) :: val"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  system_clock() arguments"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   integer :: im, ir, ic"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  cpu_time() argument"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   real :: ct"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  random_number() seed array"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   integer, dimension( random_seed_size) :: iseed"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  logical true and false"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   logical, parameter :: ltrue = .true."
   write( unit= pm_unit, fmt= fmtpr) "   logical, parameter :: lfalse = .false."
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  integer true and false"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   integer :: itrue, ifalse"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) star_banner
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  processor_model text"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "continue                                                             ! processor_model"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) star_banner
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  banner"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *)"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'Processor Model'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *)"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) star_banner
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  version control"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'Version Control'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *)"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  report processor_model version strings"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'processor_model ', processor_model_rcs_id"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *)"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'timestamp ', processor_model_timestamp"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *)"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  report compiler version strings"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'compiler_version ', compiler_version"
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'compiler_serial ', compiler_serial"
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'os_version ', os_version"
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'standard_version ', standard_version"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *)"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  report processor_dependencies version strings"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'processor_dependencies ', processor_dependencies_rcs_id"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *)"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) star_banner
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  detect mismatch between processor_dependencies and processor_model"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( processor_dependencies_rcs_id /= processor_model_rcs_id )then"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'processor_dependencies - processor_model mismatch'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) write_ln
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  detect mismatch between processor_dependencies and processor_model"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( processor_depends_timestamp /= processor_model_timestamp )then"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'processor_dependencies - processor_model mismatch'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) write_ln
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) star_banner
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  processor dependent parameters"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'Processor Memory Parameters'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *)"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'size of a numeric storage unit', numeric_storage_size"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'size of a character storage unit', character_storage_size"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'size of a file storage unit', file_storage_size"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( is_big_endian ) write( unit= *, fmt= *) 'big endian'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( is_little_endian ) write( unit= *, fmt= *) 'little endian'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *)"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'Processor Software Parameters'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *)"
   write( unit= pm_unit, fmt= *)

   select case( standard)
   case( f90_std)
      write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'processor supports Fortran 90 Standard'"
   case( f95_std)
      write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'processor supports Fortran 95 Standard'"
   case( f03_std)
      write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'processor supports Fortran 2003 Standard'"
   end select
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) &
      "   if( has_varying_strings ) write( unit= *, fmt= *) 'processor supports iso_varying_string'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *)"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'Processor Input/Output Parameters'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *)"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'preconnected input unit', input_unit"
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'preconnected output unit', output_unit"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( output_unit == error_unit )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'no preconnected error unit'"
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'preconnected error unit', error_unit"
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'default filename ', default_filename"

   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( opt_plus_sign )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'writes optional plus sign'"
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'does not write optional plus sign'"
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)

   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( leading_zero_f )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'writes leading zero using f format'"
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'does not write leading zero using f format'"
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)

   write( unit= pm_unit, fmt= fmtpr) "   if( leading_zero_e )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'writes leading zero using e format'"
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'does not write leading zero using f format'"
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)

   write( unit= pm_unit, fmt= fmtpr) &
      "   write( unit= *, fmt= *) 'smallest decimal exponemt with list-directed f format', ld_fmt_fmin"
   write( unit= pm_unit, fmt= fmtpr) &
      "   write( unit= *, fmt= *) 'largest deciaml exponent with list-directed f format', ld_fmt_fmax"

   write( unit= pm_unit, fmt= fmtpr) "   if( ld_fmt_comma )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'writes comma using list directed format'"
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'does not write comma using list directed format'"
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)

   write( unit= pm_unit, fmt= fmtpri) "   write( unit= *, fmt= *) 'end of record iostat',", eor_flag
   write( unit= pm_unit, fmt= fmtpri) "   write( unit= *, fmt= *) 'end of file iostat',", eof_flag
   write( unit= pm_unit, fmt= fmtpri) "   write( unit= *, fmt= *) 'direct access read undefined record iostat',", da_missing
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'size of a file storage unit', file_storage_size"
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'maximum record length', max_recl"
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *)"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'Miscelleanous Processor Dependent Parameters'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *)"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'random seed size', random_seed_size"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   call random_seed( get= iseed)"
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'initial random seed', iseed"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   itrue = transfer( ltrue, itrue)"
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'integer value of true', itrue"
   write( unit= pm_unit, fmt= fmtpr) "   ifalse = transfer( lfalse, ifalse)"
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'integer value of false', ifalse"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( is_ieee_fp )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'fp uses ieee 754 format'"
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'fp uses non-ieee format'"
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( is_2s_comp )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'integers are twos complement'"
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'integers are not twos complement'"
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *)"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'Processor Clock'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *)"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   call system_clock( ic, ir, im)"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( ic == -huge( 0) )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'processor has no clock'"
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'clock rate', ir"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'clock max', im"
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   call date_and_time( date= dbuff)"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( dbuff == ' ' )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'date not available'"
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'date ', dbuff"
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   call date_and_time( time= tbuff)"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( tbuff == ' ' )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'time not available'"
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'time ', tbuff"
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   call date_and_time( zone= zbuff)"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( zbuff == ' ' )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'zone not available'"
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'zone ', zbuff"
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   call date_and_time( values= val)"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( val( 1) == -huge( 0) )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'value year not available'"
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'value year', val( 1)"
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( val( 2) == -huge( 0) )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'value month not available'"
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'value month', val( 2)"
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( val( 3) == -huge( 0) )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'value day not available'"
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'value day', val( 3)"
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( val( 4) == -huge( 0) )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'value utc not available'"
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'value utc', val( 4)"
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( val( 5) == -huge( 0) )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'value hour not available'"
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'value hour', val( 5)"
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( val( 6) == -huge( 0) )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'value minute not available'"
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'value minute', val( 6)"
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( val( 7) == -huge( 0) )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'value second not available'"
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'value second', val( 7)"
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( val( 8) == -huge( 0) )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'value msec not available'"
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'value msec', val( 8)"
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   call cpu_time( ct)"
   write( unit= pm_unit, fmt= fmtpr) "   if( ct >= 0.0 )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'has cpu time'"
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'no cpu time'"
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *)"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) star_banner
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  report kinds summary"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'Kinds Summary'"
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *)"
   write( unit= pm_unit, fmt= *)

   if( define_characters )then                                       ! wants character kinds
      write( unit= pm_unit, fmt= fmtpri) &
         "   write( unit= *, fmt= *) 'character kinds ',", count( (/ ascii_character% supported, &
             ebcdic_character% supported, iso_10646_character% supported /) )
   endif                                                             ! wants character kinds

   write( unit= pm_unit, fmt= fmtpri) &
      "   write( unit= *, fmt= *) 'integer kinds ',", count( integer_kinds% supported )

   if( define_logicals )then                                         ! wants logical kinds
      write( unit= pm_unit, fmt= fmtpri) &
         "   write( unit= *, fmt= *) 'logical kinds ',", count( (/ byte_logical% supported, short_logical% supported, &
             int_logical% supported, long_logical% supported /) )
   endif                                                             ! wants logical kinds

   write( unit= pm_unit, fmt= fmtpri) &
      "   write( unit= *, fmt= *) 'real kinds ',", count( real_kinds% supported )
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *)"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'default character ', kind( ' ')"
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'default integer ', kind( 0)"
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'default logical ', kind( .true.)"
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'default real ', kind( 0.0)"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *)"
   write( unit= pm_unit, fmt= *)

   if( define_characters )then                                       ! wants character kinds
      write( unit= pm_unit, fmt= fmtpr) star_banner
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "!  report character kinds"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'Character Kinds'"
      write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "   if( ascii_k > 0 )then"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'ascii character'"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'kind ', ascii_k"

      if( ascii_character% supported )then                          ! has ascii
         write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'parameter ascii_k'"
         write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'bit_size ', bit_size( ascii_k_' ')"
         write( unit= pm_unit, fmt= *)
         write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'huge', ichar( huge( ascii_k_' ') )"
         write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'tiny', ichar( tiny( ascii_k_' ') )"
      endif                                                          ! has ascii

      write( unit= pm_unit, fmt= *) 
      write( unit= pm_unit, fmt= fmtpr) write_ln
      write( unit= pm_unit, fmt= fmtpr) "   else"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'no ascii character'"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) write_ln
      write( unit= pm_unit, fmt= fmtpr) "   endif"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "   if( ebcdic_k > 0 )then"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'ebcdic character'"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'kind ', ebcdic_k"

      if( ebcdic_character% supported )then                          ! has ebcdic
         write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'parameter ebcdic_k'"
         write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'bit_size ', bit_size( ebcdic_k_' ')"
         write( unit= pm_unit, fmt= *)
         write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'huge', ichar( huge( ebcdic_k_' ') )"
         write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'tiny', ichar( tiny( ebcdic_k_' ') )"
      endif                                                          ! has ebcdic

      write( unit= pm_unit, fmt= *) 
      write( unit= pm_unit, fmt= fmtpr) write_ln
      write( unit= pm_unit, fmt= fmtpr) "   else"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'no ebcdic character'"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) write_ln
      write( unit= pm_unit, fmt= fmtpr) "   endif"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "   if( iso_10646_k > 0 )then"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'iso_10646 character'"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'kind ', iso_10646_k"

      if( iso_10646_character% supported )then                       ! has iso_10646
         write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'parameter iso_10646_k'"
         write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'bit_size ', bit_size( iso_10646_k_' ')"
         write( unit= pm_unit, fmt= *)
         write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'huge', ichar( huge( iso_10646_k_' ') )"
         write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'tiny', ichar( tiny( iso_10646_k_' ') )"
      endif                                                          ! has iso_10646

      write( unit= pm_unit, fmt= *) 
      write( unit= pm_unit, fmt= fmtpr) write_ln
      write( unit= pm_unit, fmt= fmtpr) "   else"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'no iso_10646 character'"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) write_ln
      write( unit= pm_unit, fmt= fmtpr) "   endif"
      write( unit= pm_unit, fmt= *)
   endif                                                             ! wants character kinds

   write( unit= pm_unit, fmt= fmtpr) star_banner
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  report integer kinds"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'Integer Kinds'"
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *)"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( byte_k > 0 )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'byte integer'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'kind ', byte_k"

   if( integer_kinds( byte_idx)% supported )then                     ! has byte
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'parameter byte_k'"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'bit_size ', bit_size( 0_byte_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'huge ', huge( 0_byte_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'range ', range( 0_byte_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'digits ', digits( 0_byte_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'radix ', radix( 0_byte_k)"
      write( unit= pm_unit, fmt= *)
   endif                                                             ! has byte

   write( unit= pm_unit, fmt= fmtpr) write_ln
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'no byte integer'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) write_ln
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( short_k > 0 )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'short integer'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'kind ', short_k"

   if( integer_kinds( short_idx)% supported )then                    ! has short
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'parameter short_k'"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'bit_size ', bit_size( 0_short_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'huge ', huge( 0_short_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'range ', range( 0_short_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'digits ', digits( 0_short_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'radix ', radix( 0_short_k)"
      write( unit= pm_unit, fmt= *)
   endif                                                             ! has short

   write( unit= pm_unit, fmt= fmtpr) write_ln
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'no short integer'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) write_ln
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( int_k > 0 )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'int integer'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'kind ', int_k"

   if( integer_kinds( int_idx)% supported )then                      ! has int
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'parameter int_k'"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'bit_size ', bit_size( 0_int_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'huge ', huge( 0_int_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'range ', range( 0_int_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'digits ', digits( 0_int_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'radix ', radix( 0_int_k)"
      write( unit= pm_unit, fmt= *)
   endif                                                             ! has int

   write( unit= pm_unit, fmt= fmtpr) write_ln
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'no int integer'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) write_ln
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( long_k > 0 )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'long integer'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'kind ', long_k"

   if( integer_kinds( long_idx)% supported )then                     ! has long
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'parameter long_k'"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'bit_size ', bit_size( 0_long_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'huge ', huge( 0_long_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'range ', range( 0_long_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'digits ', digits( 0_long_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'radix ', radix( 0_long_k)"
      write( unit= pm_unit, fmt= *)
   endif                                                             ! has long

   write( unit= pm_unit, fmt= fmtpr) write_ln
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'no long integer'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) write_ln
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)

   if( define_logicals )then                                         ! wants logical kinds
      write( unit= pm_unit, fmt= fmtpr) star_banner
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "!  report logical kinds"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'Logical Kinds'"
      write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *)"
      write( unit= pm_unit, fmt= *)

      write( unit= pm_unit, fmt= fmtpr) "   if( l_byte_k > 0 )then"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'byte logical'"
      write( unit= pm_unit, fmt= *)

      if( byte_logical% supported )then                              ! has byte logical
         write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'kind ', l_byte_k"
         write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'parameter l_byte_k'"
         write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'bit_size ', bit_size( .true._l_byte_k)"
         write( unit= pm_unit, fmt= *)
      endif                                                          ! has byte logical

      write( unit= pm_unit, fmt= fmtpr) write_ln
      write( unit= pm_unit, fmt= fmtpr) "   else"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'no byte logical'"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) write_ln
      write( unit= pm_unit, fmt= fmtpr) "   endif"
      write( unit= pm_unit, fmt= *)

      write( unit= pm_unit, fmt= fmtpr) "   if( l_short_k > 0 )then"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'short logical'"
      write( unit= pm_unit, fmt= *)

      if( short_logical% supported )then                             ! has short logical
         write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'kind ', l_short_k"
         write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'parameter l_short_k'"
         write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'bit_size ', bit_size( .true._l_short_k)"
         write( unit= pm_unit, fmt= *)
      endif                                                          ! has short logical

      write( unit= pm_unit, fmt= fmtpr) write_ln
      write( unit= pm_unit, fmt= fmtpr) "   else"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'no short logical'"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) write_ln
      write( unit= pm_unit, fmt= fmtpr) "   endif"
      write( unit= pm_unit, fmt= *)

      write( unit= pm_unit, fmt= fmtpr) "   if( l_int_k > 0 )then"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'int logical'"
      write( unit= pm_unit, fmt= *)

      if( int_logical% supported )then                               ! has int logical
         write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'kind ', l_int_k"
         write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'parameter l_int_k'"
         write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'bit_size ', bit_size( .true._l_int_k)"
         write( unit= pm_unit, fmt= *)
      endif                                                          ! has int logical

      write( unit= pm_unit, fmt= fmtpr) write_ln
      write( unit= pm_unit, fmt= fmtpr) "   else"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'no int logical'"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) write_ln
      write( unit= pm_unit, fmt= fmtpr) "   endif"
      write( unit= pm_unit, fmt= *)

      write( unit= pm_unit, fmt= fmtpr) "   if( l_long_k > 0 )then"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'long logical'"
      write( unit= pm_unit, fmt= *)

      if( long_logical% supported )then                              ! has long logical
         write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'kind ', l_long_k"
         write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'parameter l_long_k'"
         write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'bit_size ', bit_size( .true._l_long_k)"
         write( unit= pm_unit, fmt= *)
      endif                                                          ! has long logical

      write( unit= pm_unit, fmt= fmtpr) write_ln
      write( unit= pm_unit, fmt= fmtpr) "   else"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'no long logical'"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) write_ln
      write( unit= pm_unit, fmt= fmtpr) "   endif"
      write( unit= pm_unit, fmt= *)

   endif                                                             ! wants logical kinds

   write( unit= pm_unit, fmt= fmtpr) star_banner
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  report real kinds"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'Real Kinds'"
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *)"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( single_k > 0 )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'single real'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'kind ', single_k"

   if( real_kinds( single_idx)% supported )then                      ! has single
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'parameter single_k'"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'bit_size ', bit_size( 1.0_single_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'huge ', huge( 1.0_single_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'tiny ', tiny( 1.0_single_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'max_exact_int ', max_exact_int( 1.0_single_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'minexponent ', minexponent( 1.0_single_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'maxexponent ', maxexponent( 1.0_single_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'range ', range( 1.0_single_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'digits ', digits( 1.0_single_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'radix ', radix( 1.0_single_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'exponent ', exponent( 1.0_single_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'fraction ', fraction( 1.0_single_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'epsilon ', epsilon( 1.0_single_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'precision ', precision( 1.0_single_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'nearest ', nearest( 1.0_single_k, 2.0_single_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'rrspacing ', rrspacing( 1.0_single_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'spacing ', spacing( 1.0_single_k)"
   endif                                                             ! has single

   write( unit= pm_unit, fmt= *) 
   write( unit= pm_unit, fmt= fmtpr) write_ln
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'no single real'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) write_ln
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( double_k > 0 )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'double real'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'kind ', double_k"

   if( real_kinds( double_idx)% supported )then                      ! has double
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'parameter double_k'"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'bit_size ', bit_size( 1.0_double_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'huge ', huge( 1.0_double_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'tiny ', tiny( 1.0_double_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'max_exact_int ', max_exact_int( 1.0_double_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'minexponent ', minexponent( 1.0_double_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'maxexponent ', maxexponent( 1.0_double_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'range ', range( 1.0_double_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'digits ', digits( 1.0_double_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'radix ', radix( 1.0_double_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'exponent ', exponent( 1.0_double_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'fraction ', fraction( 1.0_double_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'epsilon ', epsilon( 1.0_double_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'precision ', precision( 1.0_double_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'nearest ', nearest( 1.0_double_k, 2.0_double_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'rrspacing ', rrspacing( 1.0_double_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'spacing ', spacing( 1.0_double_k)"
   endif                                                             ! has double

   write( unit= pm_unit, fmt= *) 
   write( unit= pm_unit, fmt= fmtpr) write_ln
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'no double real'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) write_ln
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( quad_k > 0 )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'quad real'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'kind ', quad_k"

   if( real_kinds( quad_idx)% supported )then                        ! has quad
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'parameter quad_k'"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'bit_size ', bit_size( 1.0_quad_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'huge ', huge( 1.0_quad_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'tiny ', tiny( 1.0_quad_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'minexponent ', minexponent( 1.0_quad_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'maxexponent ', maxexponent( 1.0_quad_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'max_exact_int ', max_exact_int( 1.0_quad_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'range ', range( 1.0_quad_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'digits ', digits( 1.0_quad_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'radix ', radix( 1.0_quad_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'exponent ', exponent( 1.0_quad_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'fraction ', fraction( 1.0_quad_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'epsilon ', epsilon( 1.0_quad_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'precision ', precision( 1.0_quad_k)"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'nearest ', nearest( 1.0_quad_k, 2.0_quad_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'rrspacing ', rrspacing( 1.0_quad_k)"
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'spacing ', spacing( 1.0_quad_k)"
   endif                                                             ! has quad

   write( unit= pm_unit, fmt= *) 
   write( unit= pm_unit, fmt= fmtpr) write_ln
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'no quad real'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) write_ln
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) star_banner
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "!  report complex kinds"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *) 'Complex Kinds'"
   write( unit= pm_unit, fmt= fmtpr) "   write( unit= *, fmt= *)"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( single_k > 0 )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'single complex'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'kind ', single_k"

   if( real_kinds( single_idx)% supported )then                      ! has single
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'parameter single_k'"
      write( unit= pm_unit, fmt= fmtpr) &
         "      write( unit= *, fmt= *) 'bit_size ', bit_size( cmplx( 0.0, 0.0, kind= single_k) )"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) &
         "      write( unit= *, fmt= *) 'epsilon ', epsilon( cmplx( 0.0, 0.0, kind= single_k) )"
      write( unit= pm_unit, fmt= fmtpr) &
         "      write( unit= *, fmt= *) 'huge ', huge( cmplx( 0.0, 0.0, kind= single_k) )"
      write( unit= pm_unit, fmt= fmtpr) &
         "      write( unit= *, fmt= *) 'tiny ', tiny( cmplx( 0.0, 0.0, kind= single_k) )"
   endif                                                             ! has single

   write( unit= pm_unit, fmt= *) 
   write( unit= pm_unit, fmt= fmtpr) write_ln
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'no single complex'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) write_ln
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( double_k > 0 )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'double complex'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'kind ', double_k"

   if( real_kinds( double_idx)% supported )then                      ! has double
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'parameter double_k'"
      write( unit= pm_unit, fmt= fmtpr) &
         "      write( unit= *, fmt= *) 'bit_size ', bit_size( cmplx( 0.0, 0.0, kind= double_k) )"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) &
         "      write( unit= *, fmt= *) 'epsilon ', epsilon( cmplx( 0.0, 0.0, kind= double_k) )"
      write( unit= pm_unit, fmt= fmtpr) &
         "      write( unit= *, fmt= *) 'huge ', huge( cmplx( 0.0, 0.0, kind= double_k) )"
      write( unit= pm_unit, fmt= fmtpr) &
         "      write( unit= *, fmt= *) 'tiny ', tiny( cmplx( 0.0, 0.0, kind= double_k) )"
   endif                                                             ! has double

   write( unit= pm_unit, fmt= *) 
   write( unit= pm_unit, fmt= fmtpr) write_ln
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'no double complex'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) write_ln
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "   if( quad_k > 0 )then"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'quad complex'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'kind ', quad_k"

   if( real_kinds( quad_idx)% supported )then                        ! has quad
      write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'parameter quad_k'"
      write( unit= pm_unit, fmt= fmtpr) &
         "      write( unit= *, fmt= *) 'bit_size ', bit_size( cmplx( 0.0, 0.0, kind= quad_k) )"
      write( unit= pm_unit, fmt= *)
      write( unit= pm_unit, fmt= fmtpr) &
         "      write( unit= *, fmt= *) 'epsilon ', epsilon( cmplx( 0.0, 0.0, kind= quad_k) )"
      write( unit= pm_unit, fmt= fmtpr) &
         "      write( unit= *, fmt= *) 'huge ', huge( cmplx( 0.0, 0.0, kind= quad_k) )"
      write( unit= pm_unit, fmt= fmtpr) &
         "      write( unit= *, fmt= *) 'tiny ', tiny( cmplx( 0.0, 0.0, kind= quad_k) )"
   endif                                                             ! has quad

   write( unit= pm_unit, fmt= *) 
   write( unit= pm_unit, fmt= fmtpr) write_ln
   write( unit= pm_unit, fmt= fmtpr) "   else"
   write( unit= pm_unit, fmt= fmtpr) "      write( unit= *, fmt= *) 'no quad complex'"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) write_ln
   write( unit= pm_unit, fmt= fmtpr) "   endif"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "stop 'processor_model'                                               ! processor_model"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) star_banner
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "! processor_model"
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "! $Id: make_pm.f90 2.2 2005/05/30 15:47:01Z Dan Release $"
   write( unit= pm_unit, fmt= fmtpr) star_banner
   write( unit= pm_unit, fmt= *)
   write( unit= pm_unit, fmt= fmtpr) "end program processor_model                                          ! eof"

!  write pm.f90

   close( unit= pm_unit, iostat= istat, status= 'KEEP')

   close_error: if( istat > 0 )then                                  ! error closing pm.f90

      write( unit= log_unit, fmt= fmtpr) 'trouble closing ' // trim( pmname)

   endif close_error                                                 ! error closing pm.f90

return                                                               ! write_processor_model()

!  write_processor_model()

end subroutine write_processor_model

! **********************************************************************

!  make_processor_model

! $Id: make_pm.f90 2.2 2005/05/30 15:47:01Z Dan Release $
! **********************************************************************

end program make_processor_model                                     ! eof
