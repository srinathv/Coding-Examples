
!  write diagnostic information about assignment
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Assignment.f90 -o Assignment
!  executes with
!  ./Assignment

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program assignment

!  access the standard output unit from the environment module

use, intrinsic :: iso_fortran_env, only: output_unit

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Assignment.f90,v 1.1 2012/05/22 18:11:48 dnagle Exp $'

!  use an explicit format to write the diagnostic information

character( len= *), parameter :: cast_fmt = '( a30, 1x, i9, 1x, es18.8, 1x, "(", es18.8, ",", es18.8, ")")'

!  an integer

   integer :: i

!  a real

   real :: a

!  and a complex

   complex :: z

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  execute the procedure to write the identifying banner

   call write_identifier()

!  assign a literal to an integer

   i = 42

   a = i

   z = a

!  write the results

   write( unit= output_unit, fmt= cast_fmt) 'start with an integer literal', i, a, z

!  assign a literal to a real

   a = 4.2

   i = a

   z = a

!  write the results

   write( unit= output_unit, fmt= cast_fmt) 'start with a real literal', i, a, z

!  assign a literal to a complex

   z = ( 4.2, -1.0)

   i = z

   a = z

!  write the results

   write( unit= output_unit, fmt= cast_fmt) 'start with a complex literal', i, a, z

!  assign a literal to a real

   a = 4.2

   i = ceiling( a)

   z = a

!  write the results

   write( unit= output_unit, fmt= cast_fmt) 'integer via ceiling', i, a, z

!  assign a literal to a real

   a = 4.2

   i = floor( a)

   z = a

!  write the results

   write( unit= output_unit, fmt= cast_fmt) 'integer via floor', i, a, z

!  assign a literal to a real

   a = 4.2

   i = nint( a)

   z = a

!  write the results

   write( unit= output_unit, fmt= cast_fmt) 'nearest integer', i, a, z

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

end program assignment
