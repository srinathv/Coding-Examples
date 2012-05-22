
!  write basic operations of logical variables
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
   '$Id: Logicals.f90,v 1.1 2012/05/22 18:06:47 dnagle Exp $'

!  use an explicit format to write the diagnostic information

character( len= *), parameter :: label_logical_fmt = '( a, 2l3)'

!  use an explicit format to write the diagnostic information

character( len= *), parameter :: label_logical_operators_fmt = '( a, 2l3, 2x, a8, l3)'

!  hold the label of the case to be written

   character( len= 20) :: case_label

!  variables may have values true or false

   logical :: left_hand_side, right_hand_side

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

      read( unit= input_unit, fmt= label_logical_fmt, iostat= check_status) &
            case_label, left_hand_side, right_hand_side

!  exit the loop when the end of the file is reached

      end_of_file: if( check_status < 0 )then

         exit each_case

      end if end_of_file

!  write the values just read and the .not. operator results

      write( unit= output_unit, fmt= label_logical_operators_fmt) case_label, left_hand_side, right_hand_side, &
                                                                  'not rhs', .not. right_hand_side

!  write the values just read and the .and. operator results

      write( unit= output_unit, fmt= label_logical_operators_fmt) case_label, left_hand_side, right_hand_side, &
                                                                  'and', left_hand_side .and. right_hand_side

!  write the values just read and the .or. operator results

      write( unit= output_unit, fmt= label_logical_operators_fmt) case_label, left_hand_side, right_hand_side, &
                                                                  'or', left_hand_side .or. right_hand_side

!  write the values just read and the .eqv. operator results

      write( unit= output_unit, fmt= label_logical_operators_fmt) case_label, left_hand_side, right_hand_side, &
                                                                  'eqv', left_hand_side .eqv. right_hand_side

!  write the values just read and the .neqv. operator results

      write( unit= output_unit, fmt= label_logical_operators_fmt) case_label, left_hand_side, right_hand_side, &
                                                                  'neqv', left_hand_side .neqv. right_hand_side

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
