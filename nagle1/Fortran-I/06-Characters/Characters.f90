
!  write diagnostic information about the default characters
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Characters.f90 -o Characters
!  executes with
!  ./Characters

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program characters

!  access the standard output unit from the environment module

use, intrinsic :: iso_fortran_env, only: output_unit

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Characters.f90,v 1.1 2012/05/22 18:04:34 dnagle Exp $'

!  use an explicit format to control the written line

character( len= *), parameter :: string_fmt = '( a)'

!  use an explicit format to control the written line and show the end of the character variable

character( len= *), parameter :: string_end_fmt = '( a, "<")'

!  use an explicit format to write the diagnostic information

character( len= *), parameter :: character_code_fmt = '( i3, 1x, a)'

!  declare a long character variable and a short one
!  no kind parameter is necessary because the default character is almost always wanted

   character( len= 50) :: long_string
   character( len= 20) :: short_string


!  this temporary must be as long as either of the above

   character( len= max( len( long_string), len( short_string)) ) :: temp_string

!  loop index to iterate through printable character codes

   integer :: i

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  execute the procedure to write the identifying banner

   call write_identifier()

!  assign some values to the variables

   long_string = 'This is a long string with a lot of characters.'
   short_string = 'This string has few.'

   write( unit= output_unit, fmt= string_end_fmt) long_string
   write( unit= output_unit, fmt= string_end_fmt) short_string

!  swap the strings and write the result

   temp_string = long_string
   long_string = short_string
   short_string = temp_string

   write( unit= output_unit, fmt= string_end_fmt) long_string
   write( unit= output_unit, fmt= string_end_fmt) short_string

!  writes the default character diagnostic information to the output unit
!  only write the characters that have graphics

   write_each_code: do i = ichar( ' '), 126

      write( unit= output_unit, fmt= character_code_fmt) i, char( i)

   end do write_each_code

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

end program characters
