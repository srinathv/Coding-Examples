
!  the traditional 'Hello, world!' first program
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Hello_World.f90 -o Hello_World
!  executes with
!  ./Hello_World

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program hello_world

!  access the standard output unit from the environment module

use, intrinsic :: iso_fortran_env, only: output_unit

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Hello_World.f90,v 1.1 2012/05/22 18:00:48 dnagle Exp $'

!  use a named constant for the greeting

character( len= *), parameter :: salute = 'Hello, world!'

!  use an explicit format to control the written line

character( len= *), parameter :: string_fmt = '( a)'

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  execute the procedure to write the identifying banner

   call write_identifier()

!  writes the string to the output unit
!  output_unit is the unit designator for stdout

   write( unit= output_unit, fmt= string_fmt) salute

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

end program hello_world
