
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
   '$Id: Array.f90,v 1.1 2012/05/23 15:23:42 dnagle Exp $'

!  label array format

character( len= *), parameter :: label_array_fmt = '( a, 10i5)'

!  set array size

integer, parameter :: n = 10

!  declare arrays

   real( real32) :: a( n, n),b( 3, 3),c( 3, 2)

   integer :: i, j, k, l( 3), m( 2)

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  identify the compiler and options

   call write_identifier()

   k = 1
   define_each_row: do i = 1, n
      define_each_col: do j = 1, n
         a( j, i) = real( k, real32)
         k = k + 1
      end do define_each_col
   end do define_each_row

   write( unit= output_unit, fmt= '( a)') 'Original a'
   write_original: do i = 1, n
      write( unit= output_unit, fmt= '( "a( ", i2, ", :)", 10( 1x, f4.0))') i, a( i, :)
   end do write_original

   b = a( [ 1, 4, 9 ], [ 5, 7, 3 ])
   write( unit= output_unit, fmt=  '( a)') 'a( [ 1, 4, 9 ], [ 5, 7, 3 ])'
   write_a_indexed: do i = 1, 3
      write( unit= output_unit, fmt= '( 10( 1x, f4.0))') b( i, :)
   end do write_a_indexed

!  define some rank-1 arrays

   l = [ 10, 5, 2 ]
   m = [ 3, 8 ]

!  and use them as vector subscripts

   c = a( l, m)

   write( unit= output_unit, fmt= '( a, 3i3, a, 2i3, a)') 'l = (', l, '), m = (', m, '), a( l, m)'
   write_c: do i = 1, size( l)
      write( unit= output_unit, fmt= '( 10( 1x, f4.0))') c( i, :)
   end do write_c

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
