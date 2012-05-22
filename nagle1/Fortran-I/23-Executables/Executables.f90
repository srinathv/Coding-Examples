
!  write diagnostic information using select case
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
   '$Id: Executables.f90,v 1.1 2012/05/22 18:20:13 dnagle Exp $'

!  use an explicit format to control the written line

character( len= *), parameter :: string_fmt = '( a)'

!  prompt for two integers

character( len= *), parameter :: prompt = 'enter a character (or " " to quit)'

!  the magic character

character( len= *), parameter :: blank = ' '

!  declare an integer array of size five

   character( len= 1), save :: a = ' '

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  execute the procedure to write the identifying banner

   call write_identifier()

!  loop unitl quit is indicated

   until_blank: do

!  get the number of images and which image this one is

      write( unit= output_unit, fmt= string_fmt) prompt

!  use list directed format to allow / to end the read prematurely

      read( unit= input_unit, fmt= *) a

!  exit the loop when a and b are both zero

      if( a == blank ) exit until_blank

!  classify the character

      class_a: select case( a)

      case( '0': '9') class_a

         write( unit= output_unit, fmt= string_fmt) a // ' is a digit'

      case( 'A': 'Z', 'a': 'z') class_a

         write( unit= output_unit, fmt= string_fmt) a // ' is a letter'

      case( '+', '-', '*', '/') class_a

         write( unit= output_unit, fmt= string_fmt) a // ' is an arithmetic operator'

      case( '=') class_a

         write( unit= output_unit, fmt= string_fmt) a // ' is an assignment'

      case default class_a

         write( unit= output_unit, fmt= string_fmt) a // ' is something else'

      end select class_a

!  write the new value of the target

   end do until_blank

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
