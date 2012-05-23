
!  write diagnostic information about array assignment
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Generics.f90 -o Generics
!  executes with
!  ./Generics

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program generics

!  access the standard output unit and real kind value from the environment module

use, intrinsic :: iso_fortran_env, only: output_unit, real32, real64

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Generics.f90,v 1.1 2012/05/23 15:37:51 dnagle Exp $'

   real( real32) :: x, x1( 2, 2)
   real( real64) :: y
   complex( real32) :: z
   complex( real64) :: z1

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  identify the compiler and options

   call write_identifier()

! The "_real32" implies 4-byte real
   x = 1.2345678901234567_real32
! The "_real64" implies 8-byte real
   y = 1.2345678901234567_real64
   z = cmplx( x, y)
   z1 = cmplx( x, y)
   call random_number( harvest= x1)

   write( unit= output_unit, fmt= '( a, es20.10, a, i4)') &
      'short precision real   ', sqrt( x), ' size of short precision real is ', kind( sqrt( x))
   write( unit= output_unit, fmt= '( a, es20.10, a, i4)') &
      'long precision real    ', sqrt( y), ' size of long precision real is ', kind( sqrt( y))
   write( unit= output_unit, fmt= '( a, 2es20.10, a,i4)') &
      'short precision complex', sqrt( z), ' size of short precision complex is ', kind( sqrt( z))
   write( unit= output_unit, fmt= '( a, 2es20.10, a, i4)') &
      'long precision complex ', sqrt( z1),' size of long precision complex is ', kind( sqrt( z1))
   write( unit= output_unit, fmt= '( a, 4es20.10, /, a, 2i4)') &
      'short precision 2 x 2 array ', sqrt( x1),'size and kind of short precision 2 x 2 array is ', size( x1), kind( sqrt( x1))

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
