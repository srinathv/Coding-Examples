
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
   '$Id: Pointers.f90,v 1.1 2012/05/23 15:29:31 dnagle Exp $'

interface
   subroutine myf77sub( b, c, n)
   integer :: n
   real, intent( in) :: b( n, n), c( n/2, n/2)
   end subroutine myf77sub
end interface

!  set array size

integer, parameter :: n = 5

integer :: i, j

real, pointer :: b( :, :) => null(), c( :, :) => null()

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  identify the compiler and options

   call write_identifier()

   allocate( b( n, n))
   define_rows_b: do i = 1, n
      define_cols_b: do j = 1, n
         b( j, i) = j + ( i-1) * n
      end do define_cols_b
   end do define_rows_b

   c => b( 1: n: 2, 1: n: 2)

   call myf77sub( b, c, n)
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

subroutine myf77sub( b, c, n)
use, intrinsic :: iso_fortran_env, only: output_unit
use, intrinsic :: iso_c_binding, only: c_loc
implicit none

integer :: n
real, target, intent( in) :: b( n, n), c( n/2, n/2)

   integer :: i, j, ib, jb

continue

   write( unit= output_unit, fmt= '( a)') 'now c points to strided locations of b'
   write_rows: do i = 1, n/2

      ib = 2 * ( i-1) + 1

      write_cols: do j = 1, n/2

         jb = 2 * ( j-1) + 1

         write( unit= output_unit, fmt= '( 3( a, i0))') 'row of b ', jb, ' col of b ', ib, &
            ' loc-b ', transfer( c_loc( b( jb, ib)), i)
         write( unit= output_unit, fmt= '( 3( a, i0))') 'row of c ', j, ' col of c ', i, &
            ' loc-c ', transfer( c_loc( c( j, i)), i)

      end do write_cols
   end do write_rows

return

end subroutine myf77sub
