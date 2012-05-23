
!  write diagnostic information about array assignment
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Procedures.f90 -o Procedures
!  executes with
!  ./Procedures

!-------------------------------------------
! Simple use of function which calculates
! average of an array of numbers
!-------------------------------------------

function average( x, n) result( ave)
implicit none
! dummy variable
integer, intent( in) :: n
! dummy variable
real, intent( in) :: x( n)
real :: ave
! local variable
   integer :: i
! local variable
   real :: total

continue

   total = 0.
   each_element: do i = 1, n
      total = total + x( i)
   end do each_element
   ave = total / real( n)

return

end function average

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program procedures

!  access the standard output unit and real kind value from the environment module

use, intrinsic :: iso_fortran_env, only: output_unit

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Procedures.f90,v 1.1 2012/05/23 15:33:13 dnagle Exp $'

integer, parameter :: n = 10

   integer :: i
   real :: x( n), average

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  identify the compiler and options

   call write_identifier()

   define_x: do i = 1, n
      x( i) = real( i)
   end do define_x
   write( unit= output_unit, fmt= '( a, es20.10)') 'Average is: ', average( x, n)

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

end program procedures
