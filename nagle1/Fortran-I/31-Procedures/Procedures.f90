
!  write a recursive function
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Procedures.f90 -o Procedures
!  executes with
!  ./Procedures

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program procedures

!  access the standard input unit and the standard output unit from the environment module

use, intrinsic :: iso_fortran_env, only: input_unit, output_unit

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Procedures.f90,v 1.1 2012/05/22 18:29:18 dnagle Exp $'

!  format string output

character( len= *), parameter :: string_fmt = '( a)'

   integer :: i1

!  separate declarations from input_output
!  (there are now a few declarations)

continue

   write( unit= output_unit, fmt= *) 'input a small positive integer'

   read( unit= input_unit, fmt= *) i1

   write( unit= output_unit, fmt= *) factorial( i1)

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

!  separate declarations from procedures

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

!  write the identifying header

recursive function factorial( a) result( f)
integer :: f

integer, intent( in) :: a

!  separate declarations from procedures

continue

!  guard against nonsense

   guard: if( a < 0 )then

      stop 'nonsense in factorial'

   end if guard

   end_case: if( a == 0 )then

      f = 1

   else end_case

      f = a * factorial( a - 1)

   end if end_case

end function factorial

!  ends the program
!  the name must match the name on the program statement

end program procedures
