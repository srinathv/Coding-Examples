
!  write diagnostic information about array assignment
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Intrinsics.f90 -o Intrinsics
!  executes with
!  ./Intrinsics

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program intrinsics

!  access the standard output unit and real kind value from the environment module

use, intrinsic :: iso_fortran_env, only: output_unit

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Intrinsics.f90,v 1.1 2012/05/23 15:35:21 dnagle Exp $'

!    set problem size
integer, parameter :: n = 10

   integer :: i

!  want real64 for the erf & erfc values

   real, dimension( n) :: a

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  identify the compiler and options

   call write_identifier()

!  create an array of values

   a = [ ( real( i), i = 1, n) ]

!  write a

   write( unit= output_unit, fmt= '( a, 10f8.4)') 'a: ', a

!  write the functions
   write( unit= output_unit, fmt=  '( a, f8.4)') 'the least value: ', minval( a)

   write( unit= output_unit, fmt=  '( a, f8.4)') 'the greatest value: ', maxval( a)

   write( unit= output_unit, fmt=  '( a, i3)') 'the number of values > 5: ', count( a > 5)

   write( unit= output_unit, fmt=  '( a, l1)') 'are all values > 5 ? ', all( a > 5)

   write( unit= output_unit, fmt=  '( a, l1)') 'are any values > 5 ? ', any( a > 5)

stop 'normal exit'

!  introduces internal intrinsics
!  these intrinsics access names in the host via host association

contains

!  write the identifying header

subroutine write_identifier()

!  access the standard error unit from the environment module
!  and the intrinsic module intrinsics identifying the compiler and options used

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

end program intrinsics
