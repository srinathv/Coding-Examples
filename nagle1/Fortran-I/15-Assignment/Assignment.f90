
!  write diagnostic information about assignment
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all Assignment.f90 -o Assignment
!  executes with
!  ./Assignment

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program assignment

!  access the standard output unit from the environment module

use, intrinsic :: iso_fortran_env, only: output_unit

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Assignment.f90,v 1.1 2012/05/22 18:12:43 dnagle Exp $'

!  use an explicit format to control the written line

character( len= *), parameter :: string_fmt = '( a)'

!  use an explicit format to write the diagnostic information

character( len= *), parameter :: array_fmt = '( 7en18.8)'

!  an integer

   real, dimension( 5, 7) :: stuff, more_stuff

!  loop limits and indexes

   integer :: lb1, lb2, ub1, ub2, i1, i2

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  execute the procedure to write the identifying banner

   call write_identifier()

!  assign random numbers

   call random_number( harvest= stuff)

!  assign to more_stuff depending

   where( stuff > 0.5 )

      more_stuff = log( stuff)

   else where( stuff > 0.01 )

      more_stuff = sqrt( stuff)

   else where

      more_stuff = 0.0

   end where

!  get the loop limits

   lb1 = lbound( stuff, dim= 1)
   ub1 = ubound( stuff, dim= 1)
   lb2 = lbound( stuff, dim= 2)
   ub2 = ubound( stuff, dim= 2)

!  write the results

   write( unit= output_unit, fmt= string_fmt) 'where stuff'

!  write stuff as rows and columns

   do i1 = lb1, ub1

      write( unit= output_unit, fmt= array_fmt) ( stuff( i1, i2), i2 = lb2, ub2)

   end do

!  write the results

   write( unit= output_unit, fmt= string_fmt) 'where more_stuff'

!  write more_stuff as rows and columns

   do i1 = lb1, ub1

      write( unit= output_unit, fmt= array_fmt) ( more_stuff( i1, i2), i2 = lb2, ub2)

   end do

!  assign to the diagonals

   forall( i1= lb1: ub1, i2= lb2: ub2, i1 == i2)

      stuff( i1, i2) = 0.0
      more_stuff( i1, i2) = stuff( i2, i1)

   end forall

!  write the results

   write( unit= output_unit, fmt= string_fmt) 'forall stuff'

!  write stuff as rows and columns

   do i1 = lb1, ub1

      write( unit= output_unit, fmt= array_fmt) ( stuff( i1, i2), i2 = lb2, ub2)

   end do

!  write the results

   write( unit= output_unit, fmt= string_fmt) 'forall more_stuff'

!  write more_stuff as rows and columns

   do i1 = lb1, ub1

      write( unit= output_unit, fmt= array_fmt) ( more_stuff( i1, i2), i2 = lb2, ub2)

   end do

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

end program assignment
