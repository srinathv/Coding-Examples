
!  write diagnostic information about the 32-bit and 64-bit integer
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Integers.f90 -o Integers
!  executes with
!  ./Integers

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program integers

!  access the standard output unit from the environment module
!  also access the kind values for 32-bit and 64-bit integers

use, intrinsic :: iso_fortran_env, only: output_unit, int32, int64

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Integers.f90,v 1.1 2012/05/22 18:03:36 dnagle Exp $'

!  use an explicit format to control the written line

character( len= *), parameter :: string_fmt = '( a)'

!  use an explicit format to write the diagnostic information

character( len= *), parameter :: label_value_fmt = '( a, 1x, i0)'

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  execute the procedure to write the identifying banner

   call write_identifier()

!  writes the 32-bit integer diagnostic information to the output unit

   write( unit= output_unit, fmt= label_value_fmt) '32-bit integer kind'

   write( unit= output_unit, fmt= label_value_fmt) 'radix is ', radix( 0_int32)

   write( unit= output_unit, fmt= label_value_fmt) 'number of radix digits supported is ', digits( 0_int32)

   write( unit= output_unit, fmt= label_value_fmt) 'decimal range supported is ', range( 0_int32)

   write( unit= output_unit, fmt= label_value_fmt) 'largest value is ', huge( 0_int32)

!  writes the 64-bit integer diagnostic information to the output unit

   write( unit= output_unit, fmt= label_value_fmt) '64-bit integer kind'

   write( unit= output_unit, fmt= label_value_fmt) 'radix is', radix( 0_int64)

   write( unit= output_unit, fmt= label_value_fmt) 'number of radix digits supported is', digits( 0_int64)

   write( unit= output_unit, fmt= label_value_fmt) 'decimal range supported is', range( 0_int64)

   write( unit= output_unit, fmt= label_value_fmt) 'largest value is', huge( 0_int64)

!  stop the program
!  the end statement would have stopped execution anyway
!  but it's nice to leave a message that completion is expected

stop 'normal exit'

!  introduces internal procedures
!  these procedures access names in the host via host association

contains

!  write the identifying header

subroutine write_identifier()

!  access the standard error unit from the environment module
!  and the intrinsic module procedures identifying the compiler and options used

use, intrinsic :: iso_fortran_env, only: error_unit, compiler_version, compiler_options

!  separate declarations from executables

continue

!  write the version id

   write( unit= error_unit, fmt= string_fmt) rcs_id

!  write the compiler used to compile this executable

   write( unit= error_unit, fmt= string_fmt) compiler_version()

!  write the compiler options used to compile this executable

   write( unit= error_unit, fmt= string_fmt) compiler_options()

!  return to the main program

return

!  the end of the procedure

end subroutine write_identifier

!  ends the program
!  the name must match the name on the program statement

end program integers
