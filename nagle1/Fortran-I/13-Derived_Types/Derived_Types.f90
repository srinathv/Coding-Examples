
!  write diagnostic information about a derived type
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Derived_Types.f90 -o Derived_Types
!  executes with
!  ./Derived_Types

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program derived_types

!  access the standard output unit from the environment module

use, intrinsic :: iso_fortran_env, only: output_unit

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Derived_Types.f90,v 1.1 2012/05/22 18:10:57 dnagle Exp $'

!  use an explicit format to control the written researcher

character( len= *), parameter :: researcher_fmt = '( 1x, ">", a20, "<", 1x, i9.9, 1x, l1)'

!  the null string

character( len= *), parameter :: null_string = ''

!  the length of the name

integer, parameter :: name_len = 20

!  declare a derived type

   type :: researcher_t

      character( len= name_len) :: name

      integer :: tax_id

      logical :: has_degree

   end type researcher_t

!  a derived type named constant

type( researcher_t), parameter :: default_researcher = researcher_t( name= null_string, tax_id= 0, has_degree= .false.)

!  declare a researcher variable and give it an initial value

   type( researcher_t), save :: pi = researcher_t( "smart guy", 999999999, .true.)

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  execute the procedure to write the identifying banner

   call write_identifier()

!  writes the derived types to output unit

   write( unit= output_unit, fmt= *, delim= 'quote') pi

   write( unit= output_unit, fmt= researcher_fmt) default_researcher

!  reinitialize a variable with a constant

   pi = default_researcher

   write( unit= output_unit, fmt= *, delim= 'quote') pi

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

!  use an explicit format to control the written line

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

end program derived_types
