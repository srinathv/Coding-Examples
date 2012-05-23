
!  write diagnostic information about array assignment
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Procedures.f90 -o Procedures
!  executes with
!  ./Procedures

!------------------------------------------------
! Example: the use of internal subprogram
!------------------------------------------------
subroutine stats( x, n, avg, sig)
implicit none
integer, intent( in) :: n
real, intent( in) :: x( n)
real, intent( out) :: avg, sig

!----------------------
! Calculates Avg here
!----------------------
   avg = sum( x) / real( n)
   sig = getsig()

contains
!-------------------------------
! Note x, n, avg are inherited
! Calculates stddev here
!-----------------------------
function getsig() result( sd)
implicit none

real :: xsqav, sd

continue

   xsqav = sum( x**2) / real( n - 1)
   sd = sqrt( xsqav - avg**2)

return

end function getsig

end subroutine stats

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
   '$Id: Procedures.f90,v 1.1 2012/05/23 15:34:41 dnagle Exp $'

integer, parameter :: n = 10

   real :: x( n), avg, sig

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  identify the compiler and options

   call write_identifier()

   call random_number( harvest= x( :))
   write( unit= output_unit, fmt= '( a, (3es20.10))') 'x ', x
   write( unit= output_unit, fmt= '( /)')
   call stats( x, n, avg, sig)
   write( unit= output_unit, fmt= '( a, es20.10, a, es20.10)') 'avg ', avg, ' sig ', sig

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
