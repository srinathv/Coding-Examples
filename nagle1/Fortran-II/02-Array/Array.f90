
!  write diagnostic information about array assignment
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Array.f90 -o Array
!  executes with
!  ./Array

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program array

!  access the standard output unit from the environment module

use, intrinsic :: iso_fortran_env, only: output_unit

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Array.f90,v 1.1 2012/05/23 15:22:30 dnagle Exp $'

!  label array format

character( len= *), parameter :: label_array_fmt = '( a, 10i5)'

!  set array size

integer, parameter :: n = 10

!  skip between elements

integer, parameter :: inc = 3

!  declare an array

   integer, dimension( n) :: a

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  identify the compiler and options

   call write_identifier()

!  define a with a broadcast scalar value

   a = 1

!  write all elements

   write( unit= output_unit, fmt=  label_array_fmt) 'a', a

!  assign 2 to 1st, 4th, 7th and 10th element

   a( 1: n: inc) = 2

!  write all elements

   write( unit= output_unit, fmt=  label_array_fmt) 'a', a

!  2nd element copied to 1st,
!  5th element copied to 4th and
!  8th element copied to 7th

   a( 1: 7: inc) = a( 2: 8: inc)

!  write all elements

   write( unit= output_unit, fmt=  label_array_fmt) 'a', a

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

!  the end of the program

end program array
