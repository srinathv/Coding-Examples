
!  use a namelist format
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Input_Output.f90 -o Input_Output
!  executes with
!  ./Input_Output

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program input_output

!  access the standard output unit from the environment module

use, intrinsic :: iso_fortran_env, only: error_unit

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Input_Output.f90,v 1.1 2012/05/22 18:27:13 dnagle Exp $'

!  use an explicit format to control the written line

character( len= *), parameter :: string_fmt = '( a)'

!  set the length of names

integer, parameter :: name_len = 80

!  set some default values

character( len= *), parameter :: default_name = 'default name'

integer, parameter :: default_integer = 0

real, parameter :: default_real = huge( 0.0)

complex, parameter :: default_complex = ( default_real, default_real)

logical, parameter :: default_flag = .false.

!  declare some variables

character( len= name_len) :: name

integer :: id_number

real :: density

complex :: velocity

logical :: valid_flag

!  declare a namelist group

namelist /values/ name, id_number, density, velocity, valid_flag

!  declare an io unit

   integer :: nml_unit

!  declare an io status

   integer :: io_status

!  declare an io message

   character( len= 100) :: message

!  separate declarations from input_output
!  (there are now a few declarations)

continue

!  execute the procedure to write the identifying banner

   call write_identifier()

!  open the namelist file

   open( newunit= nml_unit, file= 'group.nml', status= 'old', iostat= io_status, iomsg= message)

   input_error: if( io_status > 0 )then

      write( unit= error_unit, fmt= string_fmt) trim( message)

      stop 'bad namelist open'

   end if input_error

!  set the default values

   name = default_name
   id_number = default_integer
   density = default_real
   velocity = default_complex
   valid_flag = default_flag

!  store an electron in the scratch file

   read( unit= nml_unit, nml= values)

!  fetch a positron from the scratch file

   write( unit= error_unit, nml= values)

!  close the scratch file

   close( unit= nml_unit, status= 'keep')

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
