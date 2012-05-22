
!  write diagnostic information from iomsg
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Input_Output.f90 -o Input_Output
!  executes with
!  ./Input_Output <file.in

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program input_output

!  access the standard input unit, the standard output unit, and the standard error unit

use, intrinsic :: iso_fortran_env, only: input_unit, output_unit, error_unit

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Input_Output.f90,v 1.1 2012/05/22 18:24:06 dnagle Exp $'

!  use an explicit format to control the written line

character( len= *), parameter :: string_fmt = '( a)'

!  use an explicit format to control the written line

character( len= *), parameter :: count_fmt = '( i9, a)'

!  use a character string as the copy buffer

   character( len= 1024) :: buffer

!  loop index

   integer :: i

!  retrieve any i/o messages

   character( len= 1024) :: message

!  retrieve i/o status

   integer :: io_status

!  counters

   integer :: digit_count = 0
   integer :: letter_count = 0
   integer :: operator_count = 0
   integer :: assignment_count = 0
   integer :: other_count = 0
   integer :: record_count = 0

!  separate declarations from input_output
!  (there are now a few declarations)

continue

!  execute the procedure to write the identifying banner

   call write_identifier()

!  loop unitl quit is indicated

   copy_records: do

!  get the number of images and which image this one is

      read( unit= input_unit, fmt= string_fmt, iostat= io_status, iomsg= message) buffer

!  detect end of file

      end_of_file: if( io_status < 0 )then

         write( unit= error_unit, fmt= string_fmt) trim( message)

         exit copy_records

      end if end_of_file

      classify_each_char: do i = 1, len_trim( buffer)

!  classify

         class_a: select case( buffer( i: i))

         case( '0': '9') class_a

            digit_count = digit_count + 1

         case( 'A': 'Z', 'a': 'z') class_a

            letter_count = letter_count + 1

         case( '+', '-', '*', '/') class_a

            operator_count = operator_count + 1

         case( '=') class_a

            assignment_count = assignment_count + 1

         case default class_a

            other_count = other_count + 1

         end select class_a

      end do classify_each_char

!  count the records

      record_count = record_count + 1

!  write the new value of the target

   end do copy_records

!  write the report

   write( unit= output_unit, fmt= count_fmt) digit_count, ' digits'

   write( unit= output_unit, fmt= count_fmt) letter_count, ' letters'

   write( unit= output_unit, fmt= count_fmt) operator_count, ' operators'

   write( unit= output_unit, fmt= count_fmt) assignment_count, ' assignments'

   write( unit= output_unit, fmt= count_fmt) other_count, ' others'

   write( unit= output_unit, fmt= count_fmt) record_count, ' records'

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

use, intrinsic :: iso_fortran_env, only: compiler_version, compiler_options

!  separate declarations from input_output

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

end program input_output
