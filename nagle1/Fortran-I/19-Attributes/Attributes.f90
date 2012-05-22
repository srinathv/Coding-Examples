
!  write diagnostic information about the 32-bit and 64-bit integer
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Attributes.f90 -o Attributes
!  executes with
!  ./Attributes

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program attributes

!  access the standard output unit from the environment module

use, intrinsic :: iso_fortran_env, only: output_unit

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Attributes.f90,v 1.1 2012/05/22 18:16:29 dnagle Exp $'

!  use an explicit format to control the written line

character( len= *), parameter :: label_fmt = '( a, 1x, i0)'

!  declare an integer pointer
!  this name provides an alias for an integer target

   integer, pointer :: integer_ptr => null()

!  declare a second integer pointer
!  this name provides another alias for an integer target

   integer, pointer :: second_ptr => null()

!  declare an integer target

   integer, pointer :: integer_alloc

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  execute the procedure to write the identifying banner

   call write_identifier()

!  an allocatable must be allocated before assigning it with a value

   allocate( integer_alloc)

!  now that it's allocated it can be a target
!  an allocatable has the target attribute

   integer_ptr => integer_alloc

!  a pointer has the target attribute
!  the second pointer points to the target as well

   second_ptr => integer_ptr

!  assign the target with a value

   integer_alloc = 1

!  write the value of the target

   write( unit= output_unit, fmt= label_fmt) 'the target value is', integer_ptr

!  assign the target with a new value

   integer_alloc = -1

!  write the new value of the target

   write( unit= output_unit, fmt= label_fmt) 'the target value is', second_ptr

!  it can be deallocated it as well
!  but it should be deallocated only if its currently allocated

   if( associated( integer_alloc) ) deallocate( integer_alloc)

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

end program attributes
