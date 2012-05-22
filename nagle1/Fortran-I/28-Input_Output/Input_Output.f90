
!  use a scratch file with direct access
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
   '$Id: Input_Output.f90,v 1.1 2012/05/22 18:26:33 dnagle Exp $'

!  use an explicit format to control the written line

character( len= *), parameter :: string_fmt = '( a)'

!  define a type

type :: gamma_t

   complex, dimension( 1: 2, 1: 2) :: gamma_mu

end type gamma_t

!  define another type

type :: spinor_t

   type( gamma_t), dimension( 0: 3) :: phi

end type spinor_t

!  define some variables

   type( spinor_t) :: electron, positron

!  declare a record length

   integer :: record_len

!  declare an io unit

   integer :: scratch_unit

!  declare an io status

   integer :: io_status

!  declare an io message

   character( len= 100) :: message

!  separate declarations from input_output
!  (there are now a few declarations)

continue

!  execute the procedure to write the identifying banner

   call write_identifier()

!  compute record length

   inquire( iolength= record_len) electron

!  open the scratch file

   open( newunit= scratch_unit, status= 'scratch', access= 'direct', recl= record_len, iostat= io_status, iomsg= message)

   input_error: if( io_status > 0 )then

      write( unit= error_unit, fmt= string_fmt) trim( message)

      stop 'bad scratch open'

   end if input_error

!  store an electron in the scratch file

   write( unit= scratch_unit, rec= 1) electron

!  fetch a positron from the scratch file

   read( unit= scratch_unit, rec = 1) positron

!  close the scratch file

   close( unit= scratch_unit, status= 'delete')

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
