
!  write diagnostic information about array assignment
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Generics.f90 -o Generics
!  executes with
!  ./Generics

!   (1)matrix-vector multiply
!   (2)matrix-matrix multiply and
! to compile this program, you can use
! gfortran ex3.f90
module matrix_arith
implicit none

! number of rows / columns
integer, parameter :: n = 2

! depending on input type
interface operator (*)
! (1) both input matrix
! (2) 1-matrix, 1-vector
! compiler redirects the calls to
! appropriate functions
   module procedure mxm, mxv
end interface


type matrix
   real, dimension( n, n) :: d
end type matrix

type vector
   real, dimension( n) :: d
end type vector

contains

! matrix-matrix multiply
function mxm( a, b) result( c)
type( matrix), intent( in) :: a, b
type( matrix) :: c

continue
! can use intrinsic function matmul
   c% d = matmul( a% d, b% d)
return
end function mxm

! matrix-vector multiply
function mxv( a, v) result( x)
type( matrix), intent( in) :: a
type( vector), intent( in) :: v
type( vector) :: x

continue
! can use intrinsic function matmul
    x% d = matmul( a% d, v% d)
return
end function mxv

end module matrix_arith

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program generics

!  access the standard output unit and real kind value from the environment module

use, intrinsic :: iso_fortran_env, only: output_unit
use :: matrix_arith

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Generics.f90,v 1.1 2012/05/23 15:38:29 dnagle Exp $'

   type( matrix) :: a, b, c
   type( vector) :: v, x

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  identify the compiler and options

   call write_identifier()

! initialize matrix a
   a% d( 1, 1) = 1.0
   a% d( 1, 2) = 0.0
   a% d( 2, 1) = 0.0
   a% d( 2, 2) = 1.0

! initialize matrix b
   b% d( 1, 1) = 2.0
   b% d( 1, 2) = 0.0
   b% d( 2, 1) = 0.0
   b% d( 2, 2) = 3.0

! initialize vector v
   v% d( 1) = 4.0
   v% d( 2) = 5.0

! matrix-matrix multiply
   c = a * b

! matrix-vector multiply
  x = b * v

   write( unit= output_unit, fmt= '( "matrix multiply operation (*) produced result: ", / 2( 2( f4.1, 1x) /))') c
   write( unit= output_unit, fmt= '( "matrix-vector operation (*) produced result: ", / 2( f4.1 /))') x

stop 'normal exit'

!  introduces internal generics
!  these generics access names in the host via host association

contains

!  write the identifying header

subroutine write_identifier()

!  access the standard error unit from the environment module
!  and the intrinsic module generics identifying the compiler and options used

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

end program generics
