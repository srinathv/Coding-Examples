
!  write diagnostic information about array assignment
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Intrinsics.f90 -o Intrinsics
!  executes with
!  ./Intrinsics

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program intrinsics

!  access the standard output unit and real kind value from the environment module

use, intrinsic :: iso_fortran_env, only: output_unit, real64

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Intrinsics.f90,v 1.1 2012/05/23 15:36:15 dnagle Exp $'

!    set problem size
integer, parameter :: m = 7

real( real64), parameter :: n = 0.0_real64, s = 1.0_real64, e = 0.25_real64, w = 0.75_real64

   integer :: i
   real( real64), dimension( m, m) :: a

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  identify the compiler and options

   call write_identifier()

   call random_number( harvest= a)

!  write anm

   write_row_ac: do i = 1, m

      write( unit= output_unit, fmt= '( i4, 7f8.4)') i, a( i, :)

   end do write_row_ac

!  iterate with shifts

   write( unit= output_unit, fmt= '( a)') 'circular boundary shifts'

   circular_iteration: do i = 1, 100

      a = 0.25_real64 * ( cshift( a, shift= 1, dim= 1) &
                        + cshift( a, shift= -1, dim= 1) &
                        + cshift( a, shift= 1, dim= 2) &
                        + cshift( a, shift= -1, dim= 2) )

   end do circular_iteration

!  write a

   write_row_pc: do i = 1, m

      write( unit= output_unit, fmt= '( i4, 7f8.4)') i, a( i, :)

   end do write_row_pc

   a = 0.0_real64

   write( unit= output_unit, fmt= '( a)') 'set boundary shifts'

!  write anm

   write_row_ae: do i = 1, m

      write( unit= output_unit, fmt= '( i4, 7f8.4)') i, a( i, :)

   end do write_row_ae

!  iterate with shifts

   write( unit= output_unit, fmt= '( a)') 'end-off boundary shifts'

   endoff_iteration: do i = 1, 100

      a = 0.25_real64 * ( eoshift( a, shift= 1, dim= 1, boundary= n) &
                        + eoshift( a, shift= -1, dim= 1, boundary= s) &
                        + eoshift( a, shift= 1, dim= 2, boundary= e) &
                        + eoshift( a, shift= -1, dim= 2, boundary= w) )

   end do endoff_iteration

!  write a

   write_row_pe: do i = 1, m

      write( unit= output_unit, fmt= '( i4, 7f8.4)') i, a( i, :)

   end do write_row_pe

stop 'normal exit'

!  introduces internal intrinsics
!  these intrinsics access names in the host via host association

contains

!  write the identifying header

subroutine write_identifier()

!  access the standard error unit from the environment module
!  and the intrinsic module intrinsics identifying the compiler and options used

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

end program intrinsics
