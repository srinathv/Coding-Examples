
!  write diagnostic information about array norm
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Array_Intrinsic.f90 -o Array_Intrinsic
!  executes with
!  ./Array_Intrinsic

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program array_intrinsic

!  access the standard output unit and real kind value from the environment module

use, intrinsic :: iso_fortran_env, only: output_unit, real64

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Array_Intrinsic.f90,v 1.1 2012/05/23 15:26:20 dnagle Exp $'

!  label array format

character( len= *), parameter :: label_array_fmt = '( a, 10i5)'

!  set array size

integer, parameter :: n = 10

!  declare arrays

   real( real64) :: a( n)

   integer :: i

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  identify the compiler and options

   call write_identifier()

!  define a
   a = [ ( real( i, real64), i=1, n) ]

!  write all elements
   write( unit= output_unit, fmt=  '( "real( a( :)) ", ( 10 f6.2))') a

!  write the norm of a
   write( unit= output_unit, fmt=  '( "norm of real( a) ", ( f6.2))') norm2( a)

!  write a normalized
   write( unit= output_unit, fmt=  '( "real( a) normalized ", ( 10 f6.2))') a / norm2( a)

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

end program array_intrinsic
