
!  write diagnostic information about array pointers
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Pointers.f90 -o Pointers
!  executes with
!  ./Pointers

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program pointers

!  access the standard output unit and real kind value from the environment module

use, intrinsic :: iso_fortran_env, only: output_unit

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Pointers.f90,v 1.1 2012/05/23 15:29:55 dnagle Exp $'

!  set array size

real, dimension( -5: 5, 0: 10), target :: a
real, dimension( :, :), pointer :: pa

integer :: la1, la2, ua1, ua2, lp1, lp2, up1, up2

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  identify the compiler and options

   call write_identifier()

!  get the bounds of a

   la1 = lbound( a, dim= 1)
   la2 = lbound( a, dim= 2)
   ua1 = ubound( a, dim= 1)
   ua2 = ubound( a, dim= 2)

   write( unit= output_unit, fmt= '( a)') 'the bounds of the original array'
   write( unit= output_unit, fmt= '( 4( a, i3))') 'lower, dim= 1', la1, ' lower, dim= 2', la2, &
      ' upper, dim= 1', ua1, ' upper, dim= 2', ua2

!  pointer associate pa with the whole array

   pa => a

   lp1 = lbound( pa, dim= 1)
   lp2 = lbound( pa, dim= 2)
   up1 = ubound( pa, dim= 1)
   up2 = ubound( pa, dim= 2)

   write( unit= output_unit, fmt= '( a)') 'the bounds of the pointer array'
   write( unit= output_unit, fmt= '( 4( a, i3))') 'lower, dim= 1', lp1, ' lower, dim= 2', lp2, &
      ' upper, dim= 1', up1, ' upper, dim= 2', up2

!  pointer associate pa with an array slice (that happens to include the whole array)

   pa => a( :, :)

   lp1 = lbound( pa, dim= 1)
   lp2 = lbound( pa, dim= 2)
   up1 = ubound( pa, dim= 1)
   up2 = ubound( pa, dim= 2)

   write( unit= output_unit, fmt= '( a)') 'the bounds of the pointer array to the array section'
   write( unit= output_unit, fmt= '( 4( a, i3))') 'lower, dim= 1', lp1, ' lower, dim= 2', lp2, &
      ' upper, dim= 1', up1, ' upper, dim= 2', up2

   call mysub( a)

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

subroutine mysub( d)
use, intrinsic :: iso_fortran_env, only: output_unit
implicit none

real, dimension( :, :) :: d

integer :: ld1, ld2, ud1, ud2

continue

!  get the bounds of the dummy argument

   ld1 = lbound( d, dim= 1)
   ld2 = lbound( d, dim= 2)
   ud1 = ubound( d, dim= 1)
   ud2 = ubound( d, dim= 2)

   write( unit= output_unit, fmt= '( a)') 'the bounds of the dummy argument array'
   write( unit= output_unit, fmt= '( 4( a, i3))') 'lower, dim= 1', ld1, ' lower, dim= 2', ld2, &
      ' upper, dim= 1', ud1, ' upper, dim= 2', ud2

!  get the bounds of the host associated array

   la1 = lbound( a, dim= 1)
   la2 = lbound( a, dim= 2)
   ua1 = ubound( a, dim= 1)
   ua2 = ubound( a, dim= 2)

   write( unit= output_unit, fmt= '( a)') 'the bounds of the host associated array'
   write( unit= output_unit, fmt= '( 4( a, i3))') 'lower, dim= 1', la1, ' lower, dim= 2', la2, &
      ' upper, dim= 1', ua1, ' upper, dim= 2', ua2

return

end subroutine mysub

!  the end of the program

end program pointers
