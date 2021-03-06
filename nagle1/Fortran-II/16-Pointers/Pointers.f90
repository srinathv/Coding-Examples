
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

use, intrinsic :: iso_c_binding, only: c_loc

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Pointers.f90,v 1.1 2012/05/23 15:29:08 dnagle Exp $'

!  label array format

character( len= *), parameter :: label_array_fmt = '( a, 10i5)'

!  set array size

integer, parameter :: n = 5

integer :: i, j, ib, jb

real, pointer :: b( :, :) => null(), c( :, :) => null()

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  identify the compiler and options

   call write_identifier()

!  allocate b and give it a value

   allocate( b( n, n))
   define_b_rows: do i = 1, n
      define_b_cols: do j = 1, n
         b( j, i) = j + ( i-1) * n
      end do define_b_cols
   end do define_b_rows

!  use transfer to write addresses

   c => b
   write( unit= output_unit, fmt= '( a)') 'now c points to b'
   write_loc_cols: do i = 1, n
      write_loc_rows: do j = 1, n
         write( unit= output_unit, fmt= '( 4( a, i0))') 'row ', j, ' col ', i, &
            ' loc-b ', transfer( c_loc( b( j, i)), i), ' loc-c ', transfer( c_loc( c( j, i)), i)
      end do write_loc_rows
   end do write_loc_cols

!  alias strided locations

   c => b( 1: n: 2, 1: n: 2)
   write( unit= output_unit, fmt= '( a)') 'now c points to strided locations of b'
   write_stride_cols: do i = 1, n/2

      ib = 2 * ( i-1) + 1

      write_stride_rows: do j = 1, n/2

         jb = 2 * ( j-1) + 1

         write( unit= output_unit, fmt= '( 4( a, i0))') 'row of b ', jb, ' col of b ', ib, &
            ' loc-b ', transfer( c_loc( b( jb, ib)), i)
         write( unit= output_unit, fmt= '( 4( a, i0))') 'row of c ', j, ' col of c ', i, &
            ' loc-c ', transfer( c_loc( c( j, i)), i)

      end do write_stride_rows
   end do write_stride_cols
   deallocate( b)
   write( unit= output_unit, fmt= '( a, l1)') 'is c associated? ', associated( c, b)

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
