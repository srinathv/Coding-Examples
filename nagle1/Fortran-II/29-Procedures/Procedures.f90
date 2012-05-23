
!  write diagnostic information about array assignment
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Procedures.f90 -o Procedures
!  executes with
!  ./Procedures

!------------------------------------
! Passing 2D array across subroutines
! (same as ex4.f90 but using assumed
! shape array)
!------------------------------------
subroutine jacobi( a)
!-----------------------------------------
! Note that array appearing inside this
! routine as a(3,3) array section and
! Jacobi iteration needs to be done with:
!
!             a(3,2)
!               |
! a(2,1)---   a(2,2) --- a(2,3)
!               |
!             a(1,2)
!
!-----------------------------------------
implicit none
! It is dummy defined at caller
real, intent( in out) :: a( :, :)

continue

   a( 2, 2) = ( a( 1, 2) + a( 3, 2) + a( 2, 1) + a( 2, 3) ) * 0.25

return

end subroutine jacobi

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
   '$Id: Procedures.f90,v 1.1 2012/05/23 15:34:15 dnagle Exp $'

interface
   subroutine jacobi( a)
      real, intent( in out) :: a( :, :)
   end subroutine jacobi
end interface

integer, parameter :: n = 10, m = 8

   real :: a( n, n)
   integer :: i, j

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  identify the compiler and options

   call write_identifier()

   a = 0.0
! Only interior points
! are initialized
   define_each_row: do i = 2, m+1
      define_each_col: do j = 2, m+1
         a( j, i) = real( i+j)
      end do define_each_col
   end do define_each_row

   write( unit= output_unit, fmt= '( a)') 'before jacobi'
   write_before: do i = 1, n
      write( unit= output_unit, fmt= '( "a( ", i2, ", :)", 10( 1x, f4.0))') i, a( i, :)
   end do write_before

   each_point_row: do i = 2, m - 1
      each_point_col: do j = 2, m - 1
! Assumed shape array
! section is passed
         call jacobi( a( i-1: i+1, j-1: j+1) )
      end do each_point_col
   end do each_point_row

   write( unit= output_unit, fmt= '( a)') 'after jacobi'
   write_after: do i = 1, n
      write( unit= output_unit, fmt= '( "a( ", i2, ", :)", 10( 1x, f4.0))') i, a( i, :)
   end do write_after

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
