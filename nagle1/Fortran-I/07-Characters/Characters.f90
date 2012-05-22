
!  write basic operations of characters variables
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Characters.f90 -o Characters
!  executes with
!  ./Characters <Characters.in

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program characters

!  access the standard input unit and standard output unit from the environment module

use, intrinsic :: iso_fortran_env, only: input_unit, output_unit

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Characters.f90,v 1.1 2012/05/22 18:05:44 dnagle Exp $'

!  use an explicit format to read the input file

character( len= *), parameter :: string_fmt = '( a)'

!  use an explicit format to read the input file

character( len= *), parameter :: two_string_fmt = '( a, a, "<")'

!  use an explicit format to write the diagnostic information

character( len= *), parameter :: name_length_fmt = '( a, i3)'

!  a template for a variable format

character( len= *), parameter :: template_fmt = '( a##, a##, "<")'

!  variables with 20 characters each

   character( len= 20) :: i_say, hello_world

!  a variable with 40 characters

   character( len= len( template_fmt)) :: variable_fmt

!  a variable with 30 characters

   character( len= 30) :: whole_string

!  the character index of the special character in the format

   integer :: idx

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  execute the procedure to write the identifying banner

   call write_identifier()

!  read strings from the standard input unit

   read( unit= input_unit, fmt= string_fmt) i_say
   read( unit= input_unit, fmt= string_fmt) hello_world

!  write the strings just read

   write( unit= output_unit, fmt= two_string_fmt) i_say, hello_world

!  write a diagnostic of the character variable

   write( unit= output_unit, fmt= name_length_fmt) 'i_say', len( i_say)

!  write the strings just read

   write( unit= output_unit, fmt= name_length_fmt) 'hello_world', len( hello_world)

!  use trim to limit the length of each string actually printed

   write( unit= output_unit, fmt= two_string_fmt) trim( i_say), trim( hello_world)

!  create a variable content format

   variable_fmt = template_fmt

!  find the first special character

   idx = index( variable_fmt, '#')

!  overwrite the first two special characters with the length of the trimmed character value
!  (we'll add one to put a space between the two values)

   write( unit= variable_fmt( idx: idx+1), fmt= '( i2.2)') len_trim( i_say) + 1

!  find the next special character

   idx = index( variable_fmt, '#')

!  overwrite the second two special characters with the length of the trimmed character value

   write( unit= variable_fmt( idx: idx+1), fmt= '( i2.2)') len_trim( hello_world)

!  write the strings just read with the custom-made format

   write( unit= output_unit, fmt= variable_fmt) i_say, hello_world

!  assign both strings to make a whole string

   whole_string = i_say // hello_world

!  write the whole string to the output unit

   write( unit= output_unit, fmt= string_fmt) whole_string

!  assign the non-trailing -blank portions of both strings to make a whole string

   whole_string = trim( i_say) // trim( hello_world)

!  write the whole string to the output unit

   write( unit= output_unit, fmt= string_fmt) whole_string

!  find the length of the value of the string and one extra blank
!  reuse the integer idx for this task

   idx = len_trim( i_say) + 1

!  assign the non-trailing -blank portions of both strings to make a whole string
!  a 1 is understood as the biginning of the i_say substring

   whole_string = i_say( : idx) // hello_world

!  write the whole string to the output unit

   write( unit= output_unit, fmt= string_fmt) whole_string

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
