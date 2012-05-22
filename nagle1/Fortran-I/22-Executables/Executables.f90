
!  write information using loops and cases
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Executables.f90 -o Executables
!  executes with
!  ./Executables

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program executables

!  access the standard input unit and the standard output unit from the environment module

use, intrinsic :: iso_fortran_env, only: input_unit, output_unit

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Executables.f90,v 1.1 2012/05/22 18:19:07 dnagle Exp $'

!  use an explicit format to control the written line

character( len= *), parameter :: string_fmt = '( a)'

!  use an explicit format to control the written line

character( len= *), parameter :: minmax_fmt = '( a, 1x, i0, 1x, a)'

!  prompt for two integers

character( len= *), parameter :: prompt = 'enter two integers (or 0 0 to quit)'

!  declare an integer array of size five

   integer, save :: a = 0, b = 0

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  execute the procedure to write the identifying banner

   call write_identifier()

!  loop unitl quit is indicated

   ab: do

!  get the number of images and which image this one is

      write( unit= output_unit, fmt= string_fmt) prompt

!  use list directed format to allow / to end the read prematurely

      read( unit= input_unit, fmt= *) a, b

!  exit the loop when a and b are both zero

      if( a == 0 .and. b == 0 ) exit ab

!  write the larger of a and b

      max_ab: if( a > b )then

         write( unit= output_unit, fmt= minmax_fmt) 'a (', a, ') is greater'

      else if( b > a )then max_ab

         write( unit= output_unit, fmt= minmax_fmt) 'b (', b, ') is greater'

      else max_ab

         write( unit= output_unit, fmt= minmax_fmt) 'a and b (', b, ') are equal'

      end if max_ab

!  write the new value of the target

   end do ab

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

!  ends the program
!  the name must match the name on the program statement

end program executables
