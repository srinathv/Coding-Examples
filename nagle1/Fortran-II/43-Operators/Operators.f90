
!  write diagnostic information about array assignment
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Operators.f90 -o Operators
!  executes with
!  ./Operators

!-------------------------------------------
! example use of creating operator to
! operate between user defined data type,
! in this case dot product between 2 vectors
!-------------------------------------------
module vector_ops
implicit none

type :: vector_t
   real :: x, y, z
end type vector_t

interface operator( .dot.)
   module procedure vector_dot
end interface

interface operator( +)
   module procedure vector_add
end interface

contains

function vector_dot( p, q ) result( dotp)
type( vector_t), intent( in) :: p, q
real :: dotp

continue

   dotp = p% x * q% x + p% y * q% y + p% z * q% z

return

end function vector_dot

function vector_add( p, q ) result( add)
type( vector_t), intent( in) :: p, q
type( vector_t) :: add

continue

   add = vector_t( p% x + q% x, p% y + q% y, p% z + q% z)

return

end function vector_add

end module vector_ops

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program operators

!  access the standard output unit and real kind value from the environment module

use, intrinsic :: iso_fortran_env, only: output_unit
use :: vector_ops

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Operators.f90,v 1.1 2012/05/23 15:39:10 dnagle Exp $'

   type( vector_t) :: u, v
   real :: prod

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  identify the compiler and options

   call write_identifier()

   u = vector_t( 1., 2., 3.)
   v = vector_t( 2., 3., 4.)

! dot product between vectors
   prod = u .dot. v

   write( unit= output_unit, fmt= '( a, 3es14.7)') 'u = ', u
   write( unit= output_unit, fmt= '( a, 3es14.7)') 'v = ', v
   write( unit= output_unit, fmt= '( a, 3es14.7)') 'u + v = ', u + v
   write( unit= output_unit, fmt= '( a, en14.7)') 'u . v = ', prod

stop 'normal exit'

!  introduces internal operators
!  these operators access names in the host via host association

contains

!  write the identifying header

subroutine write_identifier()

!  access the standard error unit from the environment module
!  and the intrinsic module operators identifying the compiler and options used

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

end program operators
