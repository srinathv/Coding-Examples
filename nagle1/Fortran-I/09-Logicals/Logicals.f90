
!  show the relational operators producing logical values
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Logicals.f90 -o Logicals
!  executes with
!  ./Logicals <Logicals.in

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program logicals

!  access the standard input unit and the standard output unit from the environment module

use, intrinsic :: iso_fortran_env, only: input_unit, output_unit

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Logicals.f90,v 1.1 2012/05/22 18:07:28 dnagle Exp $'

!  use an explicit format to write the diagnostic information

character( len= *), parameter :: label_relational_fmt = '( a6, 2i10, 2x, l1)'

!  hold the label of the case to be written

   character( len= 20) :: case_label

!  variables may have values from the input file

   integer :: left_operand, right_operand

!  check the read status

   integer :: check_status

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  execute the procedure to write the identifying banner

   call write_identifier()

!  read and write cases until end of file is encountered

   each_case: do

!  read strings from the standard input unit

      read( unit= input_unit, fmt= *, iostat= check_status) left_operand, right_operand

!  exit the loop when the end of the file is reached

      end_of_file: if( check_status < 0 )then

         exit each_case

      end if end_of_file

!  write the values just read and the < operator results

      write( unit= output_unit, fmt= label_relational_fmt) '<', left_operand, right_operand, left_operand < right_operand

!  write the values just read and the <= operator results

      write( unit= output_unit, fmt= label_relational_fmt) '<=', left_operand, right_operand, left_operand <= right_operand

!  write the values just read and the == operator results

      write( unit= output_unit, fmt= label_relational_fmt) '==', left_operand, right_operand, left_operand == right_operand

!  write the values just read and the /= operator results

      write( unit= output_unit, fmt= label_relational_fmt) '/=', left_operand, right_operand, left_operand /= right_operand

!  write the values just read and the > operator results

      write( unit= output_unit, fmt= label_relational_fmt) '>', left_operand, right_operand, left_operand > right_operand

!  write the values just read and the >= operator results

      write( unit= output_unit, fmt= label_relational_fmt) '>=', left_operand, right_operand, left_operand >= right_operand

!  end of read and write cases until end of file is encountered

   end do each_case

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

!  use an explicit format to write the banner

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

!  ends the program
!  the name must match the name on the program statement

end program logicals
