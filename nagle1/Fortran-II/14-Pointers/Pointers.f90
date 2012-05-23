
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
   '$Id: Pointers.f90,v 1.1 2012/05/23 15:28:14 dnagle Exp $'

!  label array format

character( len= *), parameter :: label_array_fmt = '( a, 10i5)'

!  set array size

integer, parameter :: n = 10

!  declare arrays

   integer :: i, j
   real, pointer :: b( :, :) => null(), c( :, :) => null()

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  identify the compiler and options

   call write_identifier()

!  allocate b and give it a value

   allocate( b( n, n))
   each_row: do i = 1, n
      each_col: do j = 1, n
         b( j, i) = real( j + (i-1) * n)
      end do each_col
   end do each_row

! c points to b
   c => b
   write( unit= output_unit, fmt= '( a)') 'now c points to b'
   rows_c: do i = 1, n
      write( unit= output_unit, fmt= '( 10( 1x, f4.0))') c( i, :)
   end do rows_c

! c points to array section of b
   c => b( 1: n: 2, 1: n: 2)
   write( unit= output_unit, fmt= '( a)') 'now c points to strided locations of b'
   rows_c_strides: do i = 1, n/2
      write( unit= output_unit, fmt= '( 10( 1x, f4.0))') c( i, :)
   end do rows_c_strides
   deallocate( b)

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
