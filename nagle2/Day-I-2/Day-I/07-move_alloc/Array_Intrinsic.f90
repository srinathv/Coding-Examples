
!  write diagnostic information about move_alloc and enlarging allocations
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Array_Intrinsic.f90 -o Array_Intrinsic
!  executes with
!  ./Array_Intrinsic

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program array_intrinsic

!  access the standard output unit and real kind value from the environment module

use, intrinsic :: iso_fortran_env, only: output_unit

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Array_Intrinsic.f90,v 1.1 2012/05/23 15:27:40 dnagle Exp $'

!  label array format

character( len= *), parameter :: label_array_fmt = '( a, 10i5)'

!  set array size

integer, parameter :: n = 10

!  declare arrays

   integer :: i, j
   integer, dimension( :, :), allocatable :: a, new_a

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  identify the compiler and options

   call write_identifier()

!  allocate a
   allocate( a( n, n))

!  define a
   define_a_rows: do j = 1, n
      define_a_cols: do i = 1, n
         a( i, j) = i*10 + j
      end do define_a_cols
   end do define_a_rows

!  write all elements
   write( unit= output_unit, fmt=  '( "original a"/ ( 10 i4))') ( a( i, :), i = 1, n)

!  allocate new_a to be four times as large
   allocate( new_a( 2*n, 2*n))

!  define the values of new_a
   new_a = 0

!  define new_a with values from a any way desired
   new_a( ::2, ::2) = a

!  move the allocation from new_a to a
   call move_alloc( from= new_a, to= a)

!  check allocation of new_a
   write( unit= output_unit, fmt=  '( "is new_a allocated? ", l1)') allocated( new_a)

!  write all elements
   write( unit= output_unit, fmt=  '( "new size a"/ ( 20 i4))') ( a( i, :), i = 1, 2*n)

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

end program array_intrinsic
