
!  write diagnostic information about the 32-bit and 64-bit integer
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Reals.f90 -o Reals
!  executes with
!  ./Reals

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program reals

!  access the standard output unit from the environment module

use, intrinsic :: iso_fortran_env, only: output_unit, real32, real64

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Reals.f90,v 1.1 2012/05/22 18:09:06 dnagle Exp $'

!  use an explicit format to write the diagnostic information

character( len= *), parameter :: label_value_fmt = '( a, i0)'

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  execute the procedure to write the identifying banner

   call write_identifier()

!  writes the 32-bit integer diagnostic information to the output unit

   write( unit= output_unit, fmt= label_value_fmt) '32-bit real kind'

   write( unit= output_unit, fmt= label_value_fmt) 'radix is ', radix( 0.0_real32)

   write( unit= output_unit, fmt= label_value_fmt) 'number of digits supported is ', precision( 0.0_real32)

   write( unit= output_unit, fmt= label_value_fmt) 'decimal range supported is ', range( 0.0_real32)

   write( unit= output_unit, fmt= '( a, e16.8)') '32-bit epsilon is ', epsilon( 0.0_real32)

!  writes the 64-bit integer diagnostic information to the output unit

   write( unit= output_unit, fmt= label_value_fmt) '64-bit real kind'

   write( unit= output_unit, fmt= label_value_fmt) 'radix is ', radix( 0.0_real64)

   write( unit= output_unit, fmt= label_value_fmt) 'number of digits supported is ', precision( 0.0_real64)

   write( unit= output_unit, fmt= label_value_fmt) 'decimal range supported is ', range( 0.0_real64)

   write( unit= output_unit, fmt= '( a, e26.16)') '64-bit epsilon is ', epsilon( 0.0_real64)

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

!  use an explicit format to control the written line

character( len= *), parameter :: string_fmt = '( a)'

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

end program reals
