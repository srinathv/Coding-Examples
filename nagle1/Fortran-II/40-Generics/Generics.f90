
!  write diagnostic information about array assignment
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Generics.f90 -o Generics
!  executes with
!  ./Generics

module distutil

! generic procedure dist
interface dist

   module procedure dist1d, dist2d

end interface dist


contains

subroutine dist1d( x1, x2, d )
! calculates distance in one dimension
implicit none

real, intent( in) :: x1, x2
real, intent( out) :: d

continue
   d = abs( x2 - x1)
return
end subroutine dist1d

subroutine dist2d( x1, y1, x2, y2, d )
! calculates distance in one dimension
implicit none
real, intent( in) :: x1, x2, y1, y2
real, intent( out) :: d

continue
   d = sqrt( ( x2 - x1)**2 + ( y2-y1)**2)
return
end subroutine dist2d

end module distutil

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program generics

!  access the standard output unit and real kind value from the environment module

use, intrinsic :: iso_fortran_env, only: output_unit
use :: distutil

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Generics.f90,v 1.1 2012/05/23 15:38:11 dnagle Exp $'

   real :: x1, x2, y1, y2, dx, dy, d

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  identify the compiler and options

   call write_identifier()

   x1 = 0.
   y1 = 0.
   x2 = 3.
   y2 = 4.
   call dist( x1, x2, dx )
   call dist( y1, y2, dy )
   call dist( x1, y1, x2, y2, d )
   write( unit= output_unit, fmt= '( a, es20.10)') 'Distance along X ', dx
   write( unit= output_unit, fmt= '( a, es20.10)') 'Distance along Y ', dy
   write( unit= output_unit, fmt= '( a, es20.10)') 'Euclidean distance in 2D ', d

stop 'normal exit'

!  introduces internal generics
!  these generics access names in the host via host association

contains

!  write the identifying header

subroutine write_identifier()

!  access the standard error unit from the environment module
!  and the intrinsic module generics identifying the compiler and options used

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

end program generics
