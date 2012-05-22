
!  use optional dummy arguments and pure procedures
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Procedures.f90 -o Procedures
!  executes with
!  ./Procedures

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program procedures

!  access the standard output unit from the environment module

use, intrinsic :: iso_fortran_env, only: output_unit

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Procedures.f90,v 1.1 2012/05/22 18:30:12 dnagle Exp $'

!  format string output

character( len= *), parameter :: string_fmt = '( a)'

!  make a type for an iteration

type :: field_t

   integer :: to, from

   real, dimension( 100, 100, 2) :: values

end type field_t

!  declare a variable

   type( field_t) :: heat

!  separate declarations from input_output
!  (there are now a few declarations)

continue

!  establish a direction

   heat% to = 1
   heat% from = 2

!  get some values

   call random_number( harvest= heat% values( :, :, heat% from))

!  and compute the maximum error left

   write( unit= output_unit, fmt= *) max_error( heat)

!  smooth the field

   call smooth( heat)

!  and compute the maximum error left

   write( unit= output_unit, fmt= *) max_error( heat)

!  stop the program
!  the end statement would have stopped execution anyway
!  but it's nice to leave a message that completion is expected

stop 'normal exit'

!  introduces internal procedures
!  these procedures access names in the host via host association

contains

!  write the identifying header

subroutine write_identifier( log_unit)

!  access the standard error unit from the environment module
!  and the intrinsic module procedures identifying the compiler and options used

use, intrinsic :: iso_fortran_env, only: error_unit, compiler_version, compiler_options

!  optional unit other than error_unit

integer, optional, intent( in) :: log_unit

!  the local unit

   integer :: local_unit

!  separate declarations from procedures

continue

!  get the unit to use

   got_unit: if( present( log_unit) )then

      local_unit = log_unit

   else got_unit

      local_unit = error_unit

   end if got_unit

!  write the version id

   write( unit= local_unit, fmt= string_fmt) rcs_id

!  write the compiler used to compile this executable

   write( unit= local_unit, fmt= string_fmt) compiler_version()

!  write the compiler options used to compile this executable

   write( unit= local_unit, fmt= string_fmt) compiler_options()

!  return to the main program

return

!  the end of the procedure

end subroutine write_identifier

!  write the identifying header

pure subroutine smooth( field)

type( field_t), intent( in out) :: field

!  sides of the array

   integer :: to, from

!  separate declarations from procedures

continue

!  set the direction

   to = field% to
   from = field% from

!  smooth the values

   associate( values => field% values)

      values( :, :, to) = 0.25 * ( eoshift( values( :, :, from), shift= 1, dim= 1) &
                                 + eoshift( values( :, :, from), shift= -1, dim= 1) &
                                 + eoshift( values( :, :, from), shift= 1, dim= 2) &
                                 + eoshift( values( :, :, from), shift= -1, dim= 2) )

   end associate

return

end subroutine smooth

!  write the identifying header

pure function max_error( a) result( me)
real :: me

type( field_t), intent( in) :: a

!  separate declarations from procedures

continue

   me = maxval( abs( a% values( :, :, 1) - a% values( :, :, 2) ))

return

end function max_error

!  ends the program
!  the name must match the name on the program statement

end program procedures
