! bof
! **********************************************************************
! Fortran 95 module processor_dependencies

! **********************************************************************
! Source Control Strings

! $Id: make_pm.f90 2.2 2005/05/30 15:47:01Z Dan Release $

! **********************************************************************
!  Copyright 2004 Purple Sage Computing Solutions, Inc.

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
!                               send email to dnagle@erols.com
!                                   or fax to 703 471 0684 (USA)
!                                  or mail to 12142 Purple Sage Ct.
!                                             Reston, VA 20194-5621 USA

! **********************************************************************
! processor_dependencies describes the processor:  kinds, processor dependencies, &c

! **********************************************************************

! Module processor_dependencies provides standard definitions
! of Fortran type kind parameters, and other Fortran
! processor dependent quantities.  The porting of Fortran
! programs from processor to processor is eased because symbolic,
! rather than 'magic number' constants and conditions may be used
! by the programmer.  The understanding of Fortran programs is enhanced
! because constants and subprograms have easily undertsood names.

! The compiler to which this file applies and the RCS strings of this
! file are available as default-kind character parameters.

! A set of kinds is defined, each a parameter whose name is <kind>_k.
! The number of bits per numeric storage unit, per character storage
! and per file storage unit are defined.

! The intrinsic bit_size() is extended to all types and kinds (a complex
! kind is by definition twice the same real kind, but bit_size() should
! exist for all kinds).  The intrinsics tiny() and huge() are extended to
! character and complex kinds.  The intrinsic epsilon() is extended to
! complex kinds.

! Quantities defined include the input and output units preconnected,
! and whether there is a preconnected error unit.  The size of the
! processor's random number generator seed is defined to allow static
! declaration of the seed.  A flag is defined whether the processor
! uses ieee 754 floating point format.  A flag is defined whether the
! processor uses twos compliment integer arithmetic.

! Suggestions for other behavior of Fortran processors which could be
! codified in this module are very much welcomed by the author.

! A summary listing of defined parameters, and a listing of defined
! procedures follows.

! **********************************************************************
!  use no modules

! **********************************************************************

!  processor_dependencies uses

!     none

!  *** all parameters are default kind ***

!  processor_dependencies constants

!     processor_dependencies_rcs_id= this file's RCS Identifier

!     compiler_version= compiler to which this file applies
!     compiler_serial= compiler license serial
!     os_version= operating system to which this file applies
!     compiler_standard= Fortran standard supported by the compiler

!     has_varying_strings- true if processor supports iso_varying_string

!     ascii_k, ebcdic_k, iso_10646_k= character kinds
!     byte_k, short_k, int_k, long_k= integer kinds
!     l_byte_k, l_short_k, l_int_k, l_long_k= logical kinds
!     single_k, double_k, quad_k= real kinds (& thf complex kinds)

!     byte_int, short_int, int_int, long_int= type( integer_inquiry_t)
!     single_real, double_real, quad_real= type( real_inquiry_t)

!     numeric_storage_size= bits per numeric storage unit
!     character_storage_size= bits per character storage unit
!     file_storage_size= bits per file storage unit

!     is_big_endian= true if processor is big endian
!     is_little_endian= true if processor is little endian

!     default_filename= filename of a unit opened without a name
!     opt_plus_sign= true if optional plus with i, e, f, g formats
!     leading_zero_f= true if optional leading zero with f format
!     leading_zero_e= true if optional leading zero with e format
!     ld_fmt_fmin= smallest exponent list-directed format writes with f format
!     ld_fmt_fmax= largest exponent list-directed format writes with f format
!     da_undefined_record= iostat value returned when reading an undefined record

!  processor_dependencies types

!     none

!  processor_dependencies variables

!     none

!  processor_dependencies operators

!     none

!  *** all integer and logical functions return default kind ***

!  *** some functions use their argument only to select specific ***

!  processor_dependencies library

!     bit_size() for character, logical, real, complex types

!     int() for logical returns 1 or 0 for T or F
!     logical() for integers, returns T or F for non-0 or 0

!     huge() for character kinds ( largest character code)
!     tiny() for character kinds ( smallest character code)

!     huge() for complex kinds ( returns sqrt( real huge) )
!     tiny() for complex kinds ( returns sqrt( real tiny) )
!     epsilon() for complex kinds ( returns sqrt( real epsilon) )

!     max_exact_int() largest integer a real kind increments (by one) exactly

! **********************************************************************

!  processor_dependencies

! **********************************************************************

module processor_dependencies

! **********************************************************************

!  use none

! **********************************************************************

!  explicit declaration of all names

implicit none

! **********************************************************************

!  all variables are static

save

! **********************************************************************

!  explicit export of all module names

private

! **********************************************************************

!  processor_dependencies RCS strings

! **********************************************************************

!  module source filename supplied by RCS

character( len= *), public, parameter :: processor_dependencies_rcs_id = &
   '$Id: make_pm.f90 2.2 2005/05/30 15:47:01Z Dan Release $'

! **********************************************************************

character( len= *), public, parameter :: processor_depends_timestamp = &
   '20111115  091932.178'

! **********************************************************************

!  processor_dependencies data

! **********************************************************************

!  compiler & OS version to which this file applies

! **********************************************************************

!  compiler version used to generate this file

character( len= *), public, parameter :: compiler_version = &
   'gfortran 4.6'

!  compiler serial number used to generate this file

character( len= *), public, parameter :: compiler_serial = &
   'GPL'

!  operating system version used to generate this file

character( len= *), public, parameter :: os_version = &
   'Mac OS X 10.7'

!  standard supported by this compiler

character( len= *), public, parameter :: standard_version = &
   'Fortran 95'

! **********************************************************************

!  *** all parameters are of default kind ***

! **********************************************************************

!  modules available to the processor

logical, public, parameter :: has_varying_strings = .false.

! **********************************************************************

! The first section defines the kind parameters supported by the processor.
! First, the kind parameters are defined.

! **********************************************************************

!  processor dependent kinds

! **********************************************************************

!  processor character kinds

! **********************************************************************

!  ascii characters (seven bit codes stored in eight bit bytes)

integer, public, parameter :: ascii_k =                    1

!  ebcdic characters (eight bit bytes)

integer, public, parameter :: ebcdic_k = -1

!  iso_10646 characters

integer, public, parameter :: iso_10646_k = -1

! **********************************************************************

!  processor integer kinds

! **********************************************************************

!  8-bit signed integer ( 1.3+2 = huge() )

!  byte signed integer

integer, public, parameter :: byte_k = selected_int_kind(     2)

!  16-bit signed integer ( 3.3+4 = huge() )

!  short signed integer

integer, public, parameter :: short_k = selected_int_kind(     4)

!  32-bit signed integer ( 2.1+9 = huge() )

!  int signed integer

integer, public, parameter :: int_k = selected_int_kind(     9)

!  64-bit signed integer ( 4.2+18 = huge() )

!  long signed integer

integer, public, parameter :: long_k = selected_int_kind(    18)

!  int38 signed integer

integer, public, parameter :: int38_k = selected_int_kind(    38)
! **********************************************************************

!  processor logical kinds

! **********************************************************************

!  logical kind using same storage as byte

integer, public, parameter :: l_byte_k =                    1

!  logical kind using same storage as short

integer, public, parameter :: l_short_k =                    2

!  logical kind using same storage as int

integer, public, parameter :: l_int_k =                    4

!  logical kind using same storage as long

integer, public, parameter :: l_long_k =                    8

! **********************************************************************

!  processor real ( & thf complex) kinds

! **********************************************************************

!  IEEE 754 single real kind

integer, public, parameter :: single_k = selected_real_kind(     6,    37)

!  IEEE 754 double real kind

integer, public, parameter :: double_k = selected_real_kind(    15,   307)

!  IEEE 754 quad real kind

integer, public, parameter :: quad_k = selected_real_kind(    33,  4931)
!  IEEE 754 r18 real kind

integer, public, parameter :: r18_k = selected_real_kind(    18,  4931)

! **********************************************************************

!  This section sets defines parameters specified by the
!  Fortran standard as being processor dependent.  These
!  parameters describe hardware, I/O, and the intrinsic
!  random number generator.

! **********************************************************************

!  values specified by the processor

! **********************************************************************

!  bits per numeric storage unit

integer, public, parameter :: numeric_storage_size =                   32

!  bits per character storage unit

integer, public, parameter :: character_storage_size =                    8

!  bits per file storage unit

integer, public, parameter :: file_storage_size = bit_size( 0) /                    4

!  signal whether hardware addresses are byte addresses or word addresses

logical, public, parameter :: is_byte_addressable = .true.

logical, public, parameter :: is_word_addressable = .false.

!  signal whether hardware is big endian or little endian

logical, public, parameter :: is_big_endian = .false.

logical, public, parameter :: is_little_endian = .true.


! **********************************************************************

!  i/o units preconnected for input ( unit= *)

integer, public, parameter :: input_unit =                    5

!  i/o units preconnected for output ( unit= *)

integer, public, parameter :: output_unit =                    6

!  i/o unit preconnected for error messages ( output_unit if no other)

integer, public, parameter :: error_unit =                    0

! **********************************************************************

!  end of file and end of record iostat= values

!  end of file

integer, parameter :: iostat_end =                   -1

!  end of record

integer, parameter :: iostat_eor =                   -2

! **********************************************************************

!  name of a file opened without a name

character( len= *), public, parameter :: default_filename = 'fort.#'

! **********************************************************************

!  exponents beyond which list-directed format switches from f to e

integer, public, parameter :: ld_fmt_fmin =                   -1

integer, public, parameter :: ld_fmt_fmax =                    8

!  optional separator when using list directed format

logical, public, parameter :: ld_fmt_comma = .false.

! **********************************************************************

!  optional plus sign using numeric formats

logical, public, parameter :: opt_plus_sign = .false.

!  optional leading zero using f and e formats

logical, public, parameter :: leading_zero_f = .true.

logical, public, parameter :: leading_zero_e = .true.

! **********************************************************************

!  the maximum record length of a sequential record

integer, public, parameter :: max_recl =                   -1

!  the iostat value returned when attempting to read a record not yet written

integer, public, parameter :: read_undefined_record =                 5002

! **********************************************************************

!  number of words in the random number generator seed array
!  this parameter may be used to statically allocate the seed array

integer, public, parameter :: random_seed_size =                   12

! **********************************************************************

!  true if processor uses ieee 754 floating point format
!  the intent here is _format_, not all the roundings, exceptions, &c

logical, public, parameter :: is_ieee_fp = .true.

! **********************************************************************

!  true if processor uses 2's complement integer format

logical, public, parameter :: is_2s_comp = .true.

! **********************************************************************

!  processor_dependencies library

! **********************************************************************

! The following intrinsic procedures are extended to other types:

! bit_size() is extended to type character, logical, real & complex.

! int() is extended to type logical.

! logical() is extended to type integer.

! huge() & tiny() are extended to type character and complex.

! epsilon() is extended to type complex.

! The generic name is defined for each group of procedures and each
! specific name is made private; access is only thru the generic name.

! **********************************************************************

!  bit_size() specific and generic names

! **********************************************************************

!  bit_size() for kinds character, logical, real (& thf complex) kinds

intrinsic :: bit_size                                                ! extend intrinsic

public :: bit_size                                                   ! generic

interface bit_size
   module procedure ascii_bit_size
   module procedure l_byte_bit_size
   module procedure l_short_bit_size
   module procedure l_int_bit_size
   module procedure l_long_bit_size
   module procedure single_bit_size
   module procedure double_bit_size
   module procedure quad_bit_size
   module procedure single_complex_bit_size
   module procedure double_complex_bit_size
   module procedure quad_complex_bit_size
end interface

! **********************************************************************

!  int() specific and generic names

! **********************************************************************

!  int() for logical kinds

intrinsic :: int                                                     ! extend intrinsic

public :: int                                                        ! generic

interface int
   module procedure l_byte_int
   module procedure l_short_int
   module procedure l_int_int
   module procedure l_long_int
end interface

! **********************************************************************

!  logical() specific and generic names

! **********************************************************************

!  logical() for integer kinds

intrinsic :: logical                                                 ! extend intrinsic

public :: logical                                                    ! generic

interface logical
   module procedure byte_logical
   module procedure short_logical
   module procedure int_logical
   module procedure long_logical
end interface

! **********************************************************************

!  epsilon(), huge(), tiny() specific and generic names

! **********************************************************************

!  huge for character & complex kinds

intrinsic :: huge                                                    ! extend intrinsic

public :: huge                                                       ! generic

interface huge
   module procedure ascii_huge
   module procedure single_complex_huge
   module procedure double_complex_huge
   module procedure quad_complex_huge
end interface

! **********************************************************************

!  tiny for character & complex kinds

intrinsic :: tiny                                                    ! extend intrinsic

public :: tiny                                                       ! generic

interface tiny
   module procedure ascii_tiny
   module procedure single_complex_tiny
   module procedure double_complex_tiny
   module procedure quad_complex_tiny
end interface

! **********************************************************************

!  epsilon for complex kinds

intrinsic :: epsilon                                                 ! extend intrinsic

public :: epsilon                                                    ! generic

interface epsilon
   module procedure single_complex_epsilon
   module procedure double_complex_epsilon
   module procedure quad_complex_epsilon
end interface

! **********************************************************************

!  max_exact_int() is the largest integer a real kind
!  can exactly increment by 1.0

! **********************************************************************

!  max_exact_int() for real kinds

public :: max_exact_int                                              ! generic

interface max_exact_int
   module procedure single_max_exact_int
   module procedure double_max_exact_int
   module procedure quad_max_exact_int
end interface

! **********************************************************************

!  defined in iso_fortran_env with Fortran 2003

! **********************************************************************

!  interpret iostat values

public :: is_iostat_end                                               ! export
public :: is_iostat_eor                                               ! export

! **********************************************************************

!  module procedures

! **********************************************************************

contains                                                             ! processor_dependencies

! **********************************************************************

!  *** all integer and logical functions return default kinds ***

!  *** some functions use their argument only to select specific ***

! **********************************************************************

! These specific procedures extend bit_size() to character, logical,
! real and complex kinds.

! **********************************************************************

!  bit_size() extend intrinsic bit_size() to all defined processor kinds

! **********************************************************************

!  ascii_bit_size() bit_size() for kind ascii

elemental integer function ascii_bit_size( ac)

character( len= *, kind= ascii_k), intent( in) :: ac

!  ascii_bit_size() local

   character( len= 1, kind= ascii_k), dimension( bit_size( 0)) :: tk

!  ascii_bit_size() text

continue                                                             ! bit_size()

   ascii_bit_size = size( transfer( tk, (/ 0/) ))

return                                                               ! bit_size()

!  ascii_bit_size()

end function ascii_bit_size

! **********************************************************************

!  l_byte_bit_size() bit_size() for kind l_byte

elemental integer function l_byte_bit_size( ll)

logical( kind= l_byte_k), intent( in) :: ll

!  l_byte_bit_size() local

   logical( kind= l_byte_k), dimension( bit_size( 0)) :: tk

!  l_byte_bit_size() text

continue                                                             ! bit_size()

   l_byte_bit_size = size( transfer( tk, (/ 0/) ))

return                                                               ! bit_size()

!  l_byte_bit_size()

end function l_byte_bit_size

! **********************************************************************

!  l_short_bit_size() bit_size() for kind l_short

elemental integer function l_short_bit_size( ll)

logical( kind= l_short_k), intent( in) :: ll

!  l_short_bit_size() local

   logical( kind= l_short_k), dimension( bit_size( 0)) :: tk

!  l_short_bit_size() text

continue                                                             ! bit_size()

   l_short_bit_size = size( transfer( tk, (/ 0/) ))

return                                                               ! bit_size()

!  l_short_bit_size()

end function l_short_bit_size

! **********************************************************************

!  l_int_bit_size() bit_size() for kind int logical

elemental integer function l_int_bit_size( il)

logical( kind= l_int_k), intent( in) :: il                           ! selects specific bit_size()

!  l_int_bit_size() text

continue                                                             ! bit_size()

   l_int_bit_size = bit_size( 0)                                     ! 1 l_int per nsu

return                                                               ! bit_size()

!  l_int_bit_size()

end function l_int_bit_size

! **********************************************************************

!  l_long_bit_size() bit_size() for kind l_long

elemental integer function l_long_bit_size( ll)

logical( kind= l_long_k), intent( in) :: ll

!  l_long_bit_size() local

   logical( kind= l_long_k), dimension( bit_size( 0)) :: tk

!  l_long_bit_size() text

continue                                                             ! bit_size()

   l_long_bit_size = size( transfer( tk, (/ 0/) ))

return                                                               ! bit_size()

!  l_long_bit_size()

end function l_long_bit_size

! **********************************************************************

!  single_bit_size() bit_size() for kind single

elemental integer function single_bit_size( sr)

real( kind= single_k), intent( in) :: sr                             ! selects specific bit_size()

!  single_bit_size() text

continue                                                             ! bit_size()

   single_bit_size = bit_size( 0)                                    ! 1 single per default integer

return                                                               ! bit_size()

!  single_bit_size()

end function single_bit_size

! **********************************************************************

!  double_bit_size() bit_size() for kind double

elemental integer function double_bit_size( dr)

real( kind= double_k), intent( in) :: dr                             ! selects specific bit_size()

!  double_bit_size() text

continue                                                             ! bit_size()

   double_bit_size = 2 * bit_size( 0)                                ! 1 double per 2 default integers

return                                                               ! bit_size()

!  double_bit_size()

end function double_bit_size

! **********************************************************************

!  quad_bit_size() bit_size() for kind quad

elemental integer function quad_bit_size( qr)

real( kind= quad_k), intent( in) :: qr

!  quad_bit_size() local

   real( kind= quad_k), dimension( bit_size( 0)) :: tk

!  quad_bit_size() text

continue                                                             ! bit_size()

   quad_bit_size = size( transfer( tk, (/ 0/) ))

return                                                               ! bit_size()

!  quad_bit_size()

end function quad_bit_size

! **********************************************************************

!  single_complex_bit_size() bit_size() for kind single_complex

elemental integer function single_complex_bit_size( sc)

complex( kind= single_k), intent( in) :: sc                          ! selects specific bit_size()

!  single_complex_bit_size()

continue                                                             ! bit_size()

   single_complex_bit_size = 2 * single_bit_size( 0.0_single_k)

return                                                               ! bit_size()

!  single_complex_bit_size()

end function single_complex_bit_size

! **********************************************************************

!  double_complex_bit_size() bit_size() for kind double_complex

elemental integer function double_complex_bit_size( dc)

complex( kind= double_k), intent( in) :: dc                          ! selects specific bit_size()

!  double_complex_bit_size()

continue                                                             ! bit_size()

   double_complex_bit_size = 2 * double_bit_size( 0.0_double_k)

return                                                               ! bit_size()

!  double_complex_bit_size()

end function double_complex_bit_size

! **********************************************************************

!  quad_complex_bit_size() bit_size() for kind quad_complex

integer function quad_complex_bit_size( qc)

complex( kind= quad_k), intent( in) :: qc                            ! selects specific bit_size()

!  quad_complex_bit_size()

continue                                                             ! bit_size()

   quad_complex_bit_size = 2 * quad_bit_size( 0.0_quad_k)

return                                                               ! bit_size()

!  quad_complex_bit_size()

end function quad_complex_bit_size

! **********************************************************************

!  int() extend intrinsic int() to all logical kinds

! **********************************************************************

!  l_byte_int() int() for kind l_byte

elemental integer function l_byte_int( bl)

logical( kind= l_byte_k), intent( in) :: bl

!  l_byte_int()

continue                                                             ! int()

   true_or_false: select case( bl)                                   ! t or f

   case( .true._l_byte_k) true_or_false                              ! true

      l_byte_int = 1                                                 ! is 1

   case( .false._l_byte_k) true_or_false                             ! false

      l_byte_int = 0                                                 ! is 0

   end select true_or_false                                          ! t or f

return                                                               ! int()

!  l_byte_int()

end function l_byte_int

! **********************************************************************

!  l_short_int() int() for kind l_short

elemental integer function l_short_int( sl)

logical( kind= l_short_k), intent( in) :: sl

!  l_short_int()

continue                                                             ! int()

   true_or_false: select case( sl)                                   ! true

   case( .true._l_short_k) true_or_false                             ! true

      l_short_int = 1                                                ! is 1

   case( .false._l_short_k) true_or_false                            ! false

      l_short_int = 0                                                ! is 0

   end select true_or_false                                          ! t or f

return                                                               ! int()

!  l_short_int()

end function l_short_int

! **********************************************************************

!  l_int_int() int() for kind l_int

elemental integer function l_int_int( il)

logical( kind= l_int_k), intent( in) :: il

!  l_int_int()

continue                                                             ! int()

   true_or_false: select case( il)                                   ! true

   case( .true._l_int_k) true_or_false                               ! true

      l_int_int = 1                                                  ! is 1

   case( .false._l_int_k) true_or_false                              ! false

      l_int_int = 0                                                  ! is 0

   end select true_or_false                                          ! t or f

return                                                               ! int()

!  l_int_int()

end function l_int_int

! **********************************************************************

!  l_long_int() int() for kind l_long

elemental integer function l_long_int( ll)

logical( kind= l_long_k), intent( in) :: ll

!  l_long_int()

continue                                                             ! int()

   true_or_false: select case( ll)                                   ! true

   case( .true._l_long_k) true_or_false                              ! true

      l_long_int = 1                                                 ! is 1

   case( .false._l_long_k) true_or_false                             ! false

      l_long_int = 0                                                 ! is 0

   end select true_or_false                                          ! t or f

return                                                               ! int()

!  l_long_int()

end function l_long_int

! **********************************************************************

!  logical() extend intrinsic logical() to all integer kinds

! **********************************************************************

!  byte_logical() logical() for kind byte

elemental logical function byte_logical( ib)

integer( kind= byte_k), intent( in) :: ib

!  byte_logical()

continue                                                             ! logical()

   byte_logical = ib /= 0_byte_k                                     ! 0 is false

return                                                               ! logical()

!  byte_logical()

end function byte_logical

! **********************************************************************

!  short_logical() logical() for kind short

elemental logical function short_logical( is)

integer( kind= short_k), intent( in) :: is

!  short_logical()

continue                                                             ! logical()

   short_logical = is /= 0_short_k                                   ! 0 is false

return                                                               ! logical()

!  short_logical()

end function short_logical

! **********************************************************************

!  int_logical() logical() for kind int

elemental logical function int_logical( ii)

integer( kind= int_k), intent( in) :: ii

!  int_logical()

continue                                                             ! logical()

   int_logical = ii /= 0_int_k                                       ! 0 is false

return                                                               ! logical()

!  int_logical()

end function int_logical

! **********************************************************************

!  long_logical() logical() for kind long

elemental logical function long_logical( il)

integer( kind= long_k), intent( in) :: il

!  long_logical()

continue                                                             ! logical()

   long_logical = il /= 0_long_k                                     ! 0 is false

return                                                               ! logical()

!  long_logical()

end function long_logical

! **********************************************************************

! These specific procedures extend huge() and tiny() to character (if
! defined) and complex kinds.  The character huge() and tiny() return
! the characters with the largest and smallest character codes.  The
! complex huge() and tiny() return the square root of their real
! counterparts.  The choice of sqrts defines the range where magnitude
! comparisons are valid.  [ |z| = sqrt( r^2 + i^2) ]  So if two
! complex numbers have components within sqrt( real epsilon) of each
! other, they will compare with equal magnitude.

! **********************************************************************

!  huge()/tiny() for character kinds

! **********************************************************************

!  The ascii huge and tiny return characters with the largest and
!  smallest character codes.

! **********************************************************************

!  ascii_huge() huge() for kind ascii

elemental character( len= 1, kind= ascii_k) function ascii_huge( ac)

character( len= *, kind= ascii_k), intent( in) :: ac

!  ascii_huge()

continue                                                             ! huge()

   ascii_huge = achar( 127)                                          ! largest is del

return                                                               ! huge()

!  ascii_huge()

end function ascii_huge

! **********************************************************************

!  ascii_tiny() tiny() for kind ascii

elemental character( len= 1, kind= ascii_k) function ascii_tiny( ac)

character( len= *, kind= ascii_k), intent( in) :: ac

!  ascii_tiny()

continue                                                             ! tiny()

   ascii_tiny = achar( 0)                                            ! smallest is nul

return                                                               ! tiny()

!  ascii_tiny()

end function ascii_tiny

! **********************************************************************

! These specific procedures extend huge() to type complex.  huge()
! returns the square root of its real counterpart.  The sqrt is returned
! so magnitude comparisons may be made [ sqrt( x^2 + y^2) = |z| < huge()
! or sqrt( x^2 + y^2) = |z| > tiny() ]

! **********************************************************************

!  huge() for complex kinds

! **********************************************************************

!  single_complex_huge() huge() for kind single complex

elemental real( kind= single_k) function single_complex_huge( sc)

complex( kind= single_k), intent( in) :: sc

!  single_complex_huge()

continue                                                             ! huge()

   single_complex_huge = sqrt( huge( real( sc)) )

return                                                               ! huge()

!  single_complex_huge()

end function single_complex_huge

! **********************************************************************

!  double_complex_huge() huge() for kind double complex

elemental real( kind= double_k) function double_complex_huge( dc)

complex( kind= double_k), intent( in) :: dc

!  double_complex_huge()

continue                                                             ! huge()

   double_complex_huge = sqrt( huge( real( dc)) )

return                                                               ! huge()

!  double_complex_huge()

end function double_complex_huge

! **********************************************************************

!  quad_complex_huge() huge() for kind quad complex

elemental real( kind= quad_k) function quad_complex_huge( qc)

complex( kind= quad_k), intent( in) :: qc

!  quad_complex_huge()

continue                                                             ! huge()

   quad_complex_huge = sqrt( huge( real( qc)) )

return                                                               ! huge()

!  quad_complex_huge()

end function quad_complex_huge

! **********************************************************************

! These specific procedures extend tiny() to type complex.  tiny()
! returns the square root of its real counterpart.

! **********************************************************************

!  tiny() for complex kinds

! **********************************************************************

!  single_complex_tiny() tiny() for kind single complex

elemental real( kind= single_k) function single_complex_tiny( sc)

complex( kind= single_k), intent( in) :: sc

!  single_complex_tiny()

continue                                                             ! tiny()

   single_complex_tiny = sqrt( tiny( real( sc)) )

return                                                               ! tiny()

!  single_complex_tiny()

end function single_complex_tiny

! **********************************************************************

!  double_complex_tiny() tiny() for kind double complex

elemental real( kind= double_k) function double_complex_tiny( dc)

complex( kind= double_k), intent( in) :: dc

!  double_complex_tiny()

continue                                                             ! tiny()

   double_complex_tiny = sqrt( tiny( real( dc)) )

return                                                               ! tiny()

!  double_complex_tiny()

end function double_complex_tiny

! **********************************************************************

!  quad_complex_tiny() tiny() for kind quad complex

elemental real( kind= quad_k) function quad_complex_tiny( qc)

complex( kind= quad_k), intent( in) :: qc

!  quad_complex_tiny()

continue                                                             ! tiny()

   quad_complex_tiny = sqrt( tiny( real( qc)) )

return                                                               ! tiny()

!  quad_complex_tiny()

end function quad_complex_tiny

! **********************************************************************

! These specific procedures extend epsilon() to type complex.  epsilon()
! returns the square root of its real counterpart.  The magnitude of
! two complex numbers differ if the numbers differ by more than epsilon()

! **********************************************************************

!  epsilon() for complex kinds

! **********************************************************************

!  single_complex_epsilon() epsilon() for kind single complex

elemental real( kind= single_k) function single_complex_epsilon( sc)

complex( kind= single_k), intent( in) :: sc

!  single_complex_epsilon()

continue                                                             ! epsilon()

   single_complex_epsilon = sqrt( epsilon( abs( sc)) )

return                                                               ! epsilon()

!  single_complex_epsilon()

end function single_complex_epsilon

! **********************************************************************

!  double_complex_epsilon() epsilon() for kind double complex

elemental real( kind= double_k) function double_complex_epsilon( dc)

complex( kind= double_k), intent( in) :: dc

!  double_complex_epsilon()

continue                                                             ! epsilon()

   double_complex_epsilon = sqrt( epsilon( abs( dc)) )

return                                                               ! epsilon()

!  double_complex_epsilon()

end function double_complex_epsilon

! **********************************************************************

!  quad_complex_epsilon() epsilon() for kind quad complex

elemental real( kind= quad_k) function quad_complex_epsilon( qc)

complex( kind= quad_k), intent( in) :: qc

!  quad_complex_epsilon()

continue                                                             ! epsilon()

   quad_complex_epsilon = sqrt( epsilon( abs( qc)) )

return                                                               ! epsilon()

!  quad_complex_epsilon()

end function quad_complex_epsilon

! **********************************************************************

! These specific procedures compute the largest integer
! which may be stored exactly in a real kind.

! **********************************************************************

!  max_exact_int() for real kinds

! **********************************************************************

!  single_max_exact_int() max_exact_int() for kind single real

elemental real( kind= single_k) function single_max_exact_int( sr)

real( kind= single_k), intent( in) :: sr

!  single_max_exact_int()

continue                                                             ! max_exact_int()

   single_max_exact_int = nearest( real( radix( sr), kind= single_k) &
                       ** real( digits( sr), kind= single_k), -1.0_single_k)

return                                                               ! max_exact_int()

!  single_max_exact_int()

end function single_max_exact_int

! **********************************************************************

!  double_max_exact_int() max_exact_int() for kind double real

elemental real( kind= double_k) function double_max_exact_int( dr)

real( kind= double_k), intent( in) :: dr

!  double_max_exact_int()

continue                                                             ! max_exact_int()

   double_max_exact_int = nearest( real( radix( dr), kind= double_k) &
                       ** real( digits( dr), kind= double_k), -1.0_double_k)

return                                                               ! max_exact_int()

!  double_max_exact_int()

end function double_max_exact_int

! **********************************************************************

!  quad_max_exact_int() epsilon() for kind quad real

elemental real( kind= quad_k) function quad_max_exact_int( qr)

real( kind= quad_k), intent( in) :: qr

!  quad_max_exact_int()

continue                                                             ! max_exact_int()

   quad_max_exact_int = nearest( real( radix( qr), kind= quad_k) &
                     ** real( digits( qr), kind= quad_k), -1.0_quad_k)

return                                                               ! max_exact_int()

!  quad_max_exact_int()

end function quad_max_exact_int

! **********************************************************************

! The following functions detect the conditions indicated according
! to rules specified by the Fortran standard.  The iostat return
! value is required to be an error code, an end code, or a success code.

! **********************************************************************

!  is_iostat_eof(), is_iostat_eor() detect standard conditions

! **********************************************************************

!  is_iostat_end() true if iostat indicates error

pure logical function is_iostat_end( iostat)

integer, intent( in) :: iostat

!  is_iostat_end()

continue                                                             ! is_iostat_end()

   is_iostat_end = any( (/ iostat_end /) == iostat)              ! eof condition

return                                                               ! is_iostat_end()

!  is_iostat_end()

end function is_iostat_end

! **********************************************************************

!  is_iostat_eor() true if iostat indicates error

pure logical function is_iostat_eor( iostat)

integer, intent( in) :: iostat

!  is_iostat_eor()

continue                                                             ! is_iostat_eor()

   is_iostat_eor = any( (/ iostat_eor /) == iostat)              ! eor condition

return                                                               ! is_iostat_eor()

!  is_iostat_eor()

end function is_iostat_eor

! **********************************************************************

!  processor_dependencies

! $Id: make_pm.f90 2.2 2005/05/30 15:47:01Z Dan Release $
! **********************************************************************

end module processor_dependencies                                            ! eof
