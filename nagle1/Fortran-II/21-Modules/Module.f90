
!  write diagnostic information about modules
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Module.f90 -o Module
!  executes with
!  ./Module

module tst

integer, parameter :: nlon = 10, nlat = 10

   real :: t( nlat, nlon)

end module tst

subroutine settemp()
! first statements before any declarations
use :: tst, only: t

implicit none

! local variable
   integer :: i

continue

   each_lon: do i = 1, size( t( 1,:))
      call random_number( harvest= t( :, i))
   end do each_lon

return

end subroutine settemp

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program modules

!  access the standard output unit and real kind value from the environment module

use, intrinsic :: iso_fortran_env, only: output_unit

use :: tst, only: t

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Module.f90,v 1.1 2012/05/23 15:31:00 dnagle Exp $'

   integer :: i

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  identify the compiler and options

   call write_identifier()

   call settemp()

   each_lat: do i = 1, size( t( :, 1))
      write( unit= output_unit, fmt= '( 10( 1x, f4.3))') t( i, :)
   end do each_lat

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

end program modules
