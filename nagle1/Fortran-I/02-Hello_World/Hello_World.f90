
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

!  use a named constant for the greeting

character( len= *), parameter :: salute = 'Hello, world!'

!  use an explicit format to control the written line

character( len= *), parameter :: salute_fmt = '( a)'

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  writes the string to the output unit
!  output_unit is the unit designator for stdout

   write( unit= output_unit, fmt= salute_fmt) salute

!  stop the program
!  the end statement would have stopped execution anyway
!  but it's nice to leave a message that completion is expected

stop 'normal exit'

!  ends the program
!  the name must match the name on the program statement

end program hello_world
