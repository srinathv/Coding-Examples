
!  write diagnostic information about do loops
!  gfortran -std=f2008 -Wall -fcheck=all Executables.f90 -o Executables
!  executes with
!  ./Executables

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program executables

!  access the standard output unit from the environment module

use, intrinsic :: iso_fortran_env, only: input_unit, output_unit

!  require declaration of all names
!  this should be your first statement after the use statement(s)
!  in _every_ program, module, and external subprogram

implicit none

!  record the version control id of this file

character( len= *), parameter :: rcs_id = &
   '$Id: Executables.f90,v 1.1 2012/05/22 18:23:09 dnagle Exp $'

!  write the arrays

character( len= *), parameter :: array_fmt = '( 1x, 7en18.8)'

!  declare some arrays for use in loops

   real, dimension( 5, 7) :: a, b, c

!  declare some loop indexes

   integer :: i, j

!  declare some loop limits

   integer :: l1, l2, u1, u2

!  separate declarations from executables
!  (there are now a few declarations)

continue

!  execute the procedure to write the identifying banner

   call write_identifier()

!  define b, c

   call random_number( harvest= b)
   call random_number( harvest= c)

!  define the loop limits

   l1 = lbound( a, dim= 1)
   u1 = ubound( a, dim= 1)
   l2 = lbound( a, dim= 2)
   u2 = ubound( a, dim= 2)

!  preset a

   a = -huge( a)

   write_preset: do i = l1, u1

      write( unit= output_unit, fmt= array_fmt) ( a( i, j), j = l2, u2)

   end do write_preset

!  loop over the array extents

   outer_counted: do j = l2 + 1, u2 - 1

      inner_counted: do i = l1 + 1, u1 - 1

!  an array computation

         a( i, j) = b( i - 1, j) + c( i, j + 1)

      end do inner_counted

   end do outer_counted

   write_counted: do i = l1, u1

      write( unit= output_unit, fmt= array_fmt) ( a( i, j), j = l2, u2)

   end do write_counted

!  loop forever

   i = 0
   j = 0

   outer_forever: do

      j = j + 1

!  must explicitly exit when gone too far

      if( j > u2 ) exit outer_forever

      inner_forever: do

         i = i + 1

!  must explicitly exit when gone too far

         if( i > u1 )then

            i = 0
            exit inner_forever

         end if

         write( unit= output_unit, fmt= *) i, j, a( i, j)

      end do inner_forever

   end do outer_forever

!  write the results

   write_forever: do i = l1, u1

      write( unit= output_unit, fmt= array_fmt) ( a( i, j), j = l2, u2)

   end do write_forever

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

end program executables
