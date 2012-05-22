
!  write diagnostic information about the default real
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Reals.f90 -o Reals
!  executes with
!  ./Reals

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program reals

!  access the standard output unit from the environment module

use, intrinsic :: iso_fortran_env, only: output_unit

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Reals.f90,v 1.1 2012/05/22 18:08:24 dnagle Exp $'

!  use an explicit format to write the diagnostic information

character( len= *), parameter :: label_value_fmt = '( a, i0)'

!  use an explicit format to write the diagnostic information

character( len= *), parameter :: label_operators_fmt = '( a15, 3f10.5)'

!  use an explicit format to write the diagnostic information

character( len= *), parameter :: power_fmt = '( e18.8, 1x, es18.8, 1x, en18.8)'

!  a pair of test values

   real :: a = 3.141592654, b = -2.718281828

!  loop index

   integer :: i

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  execute the procedure to write the identifying banner

   call write_identifier()

!  writes the diagnostic information to the output unit

   write( unit= output_unit, fmt= label_value_fmt) 'default real kind'

   write( unit= output_unit, fmt= label_value_fmt) 'radix used is ', radix( 0.0)

   write( unit= output_unit, fmt= label_value_fmt) 'number of digits supported is ', precision( 0.0)

   write( unit= output_unit, fmt= label_value_fmt) 'decimal range supported is ', range( 0.0)

   write( unit= output_unit, fmt= '( a, e16.8)') 'epsilon is ', epsilon( 0.0)

!  write a few operations with a and b as operands

   write( unit= output_unit, fmt= label_operators_fmt) 'b and -b ', b, -b

   write( unit= output_unit, fmt= label_operators_fmt) 'a + b ', a, b, a + b

   write( unit= output_unit, fmt= label_operators_fmt) 'a - b ', a, b, a - b

   write( unit= output_unit, fmt= label_operators_fmt) 'a * b ', a, b, a * b

   write( unit= output_unit, fmt= label_operators_fmt) 'a / b ', a, b, a / b

   write( unit= output_unit, fmt= label_operators_fmt) 'a ** b ', a, b, a ** b

!  mod is a − int( a/b) * b

   write( unit= output_unit, fmt= label_operators_fmt) 'mod( a, b) ', a, b, mod( a, b)

!  modulo is a − floor( a/b) * b

   write( unit= output_unit, fmt= label_operators_fmt) 'modulo( a, b) ', a, b, modulo( a, b)

!  demonstrate the various e formats

   power_pi: do i = 1, 20

      write( unit= output_unit, fmt= power_fmt) a, a, a

      a = a * b

   end do power_pi

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
