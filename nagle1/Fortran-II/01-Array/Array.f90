
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
   '$Id: Array.f90,v 1.1 2012/05/23 15:22:00 dnagle Exp $'

!  label array format

character( len= *), parameter :: label_array_fmt = '( a, 10i5)'

!  set array size

integer, parameter :: n = 10

!  skip between elements

integer, parameter :: inc = 2

!  declare an array

   integer, dimension( n) :: a

!  declare a loop index for the implied do loop

   integer :: i

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  identify the compiler and options

   call write_identifier()

!  define a with an array constructor

   a = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]

!  write inc

   write( unit= output_unit, fmt=  label_array_fmt) 'inc', inc

!  write size of a

   write( unit= output_unit, fmt=  label_array_fmt) 'size( a)', size( a)

!  write bounds of a

   write( unit= output_unit, fmt=  label_array_fmt) 'bounds', ubound( a, dim= 1), lbound( a, dim= 1)

!  write size of a

   write( unit= output_unit, fmt=  label_array_fmt) 'size( a)', max( ( ubound( a, dim= 1) - lbound( a, dim= 1) + 1) / 1, 0)

!  write shape of a

   write( unit= output_unit, fmt=  label_array_fmt) 'shape( a)', shape( a)

!  write all elements

   write( unit= output_unit, fmt=  label_array_fmt) 'a', a

!  write 1 through n/2

   write( unit= output_unit, fmt=  label_array_fmt) 'a( 1: n/inc)', a( 1: n/inc)

!  write 1st, 3rd, 5th ..

   write( unit= output_unit, fmt=  label_array_fmt) 'a( 1: n: inc)', a( 1: n: inc)

!  write nth, (n-2), ..

   write( unit= output_unit, fmt=  label_array_fmt) 'a( n: 1: -inc)', a( n: 1: -inc)

!  define a with an array constructor containing an implied-do

   a = [ ( i*i, i = 1, n) ]

!  write inc

   write( unit= output_unit, fmt=  label_array_fmt) 'inc', inc

!  write size of a

   write( unit= output_unit, fmt=  label_array_fmt) 'size( a)', size( a)

!  write all elements

   write( unit= output_unit, fmt=  label_array_fmt) 'a', a

!  write 1 through n/2

   write( unit= output_unit, fmt=  label_array_fmt) 'a( 1: n/inc)', a( 1: n/inc)

!  write 1st, 3rd, 5th ..

   write( unit= output_unit, fmt=  label_array_fmt) 'a( 1: n: inc)', a( 1: n: inc)

!  write nth, (n-2), ..

   write( unit= output_unit, fmt=  label_array_fmt) 'a( n: 1: -inc)', a( n: 1: -inc)

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
