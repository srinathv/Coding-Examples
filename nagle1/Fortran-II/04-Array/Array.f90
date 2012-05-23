
!  write diagnostic information about array assignment
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Array.f90 -o Array
!  executes with
!  ./Array

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program array

!  access the standard output unit and real kind value from the environment module

use, intrinsic :: iso_fortran_env, only: output_unit, real32

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Array.f90,v 1.1 2012/05/23 15:23:13 dnagle Exp $'

!  label array format

character( len= *), parameter :: label_array_fmt = '( a, 10i5)'

!  set array size

integer, parameter :: n = 10, m= 4

!  loop indexes

   integer :: i, j

!  declare arrays

   real( real32) :: a( n, n), b( m, m)

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  identify the compiler and options

   call write_identifier()

!-----------------------------
! initialize a with contiguous
! numbers starting from 1 in
! column major contiguous
! locations
!-----------------------------

   define_each_column: do i = 1, n
      define_each_row: do j = 1, n
         a( j, i) = real( j + ( i - 1) * n, real32)
      end do define_each_row
   end do define_each_column

!  write as a matrix

   write( unit= output_unit, fmt=  '( a)') 'Complete array a'
   write_cols_a: do i = 1, n
      write( unit= output_unit, fmt=  '( "a( ", i2, ", :)", 10( 1x, f4.0))') i, a( i, :)
   end do write_cols_a

! note that dimension of b is compatible

   b = a( 1: n: 3, 1: n: 3)
   write( unit= output_unit, fmt=  '( a)') 'Array section a( 1: n: 3, 1: n: 3)'
   write_cols_b: do i = 1, m
      write( unit= output_unit, fmt=  '( "b( ", i2, ", :)", 4( 1x, f4.0))') i, b( i, :)
   end do write_cols_b

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
