
!  write diagnostic information about array pointers
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Pointers.f90 -o Pointers
!  executes with
!  ./Pointers

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program pointers

!  access the standard output unit and real kind value from the environment module

use, intrinsic :: iso_fortran_env, only: output_unit

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Pointers.f90,v 1.1 2012/05/23 15:28:39 dnagle Exp $'

!  label array format

character( len= *), parameter :: label_array_fmt = '( a, 10i5)'

!  set array size

integer, parameter :: n = 10

   type :: flddata
! temperature
      real :: t
! velocity along lon
      real :: u
! velocity along lat
      real :: v
! pressure
      real :: p
   end type flddata

   type( flddata), pointer :: f( :, :) => null()
   integer :: nlat, nlon, i, j
   real :: r( 4)

!  pointer to a real rank-2 array

   real, dimension( :, :), pointer :: p2 => null()

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  identify the compiler and options

   call write_identifier()

   nlat = 10
   nlon = 10

! allocate and

   allocate( f( nlat, nlon))

! initialize with some
! random numbers using
! f90 intrinsic random_number

   each_lon: do i = 1, nlon
      each_lat: do j = 1, nlat
         call random_number( harvest= r( 1: 4))
         f( j, i)% t = r( 1)
         f( j, i)% u = r( 2)
         f( j, i)% v = r( 3)
         f( j, i)% p = r( 4)
      end do each_lat
   end do each_lon

!  write the pressure array

   write( unit= output_unit, fmt= '( a)') '   write only the pressues'
   write_each_lat_p: do i = 1, nlat
      write( unit= output_unit, fmt= '( 10( 1x, f5.2))') f( i, :)% p
   end do write_each_lat_p

!  another way

   p2 => f% t

   write( unit= output_unit, fmt= '( a)') '   write only the temperatures'
   write_each_lat_t: do i = 1, nlat
      write( unit= output_unit, fmt= '( 10( 1x, f5.2))') p2( i, :)
   end do write_each_lat_t

   deallocate( f)

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

end program pointers
