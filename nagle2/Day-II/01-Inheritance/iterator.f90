! bof
! **********************************************************************
! Fortran 2003 module iterator

! **********************************************************************
! Source Control Strings

! $Id: iterator.f90 1.1 2003/05/10 13:24:25Z Dan Exp $

! **********************************************************************
!  Copyright 2012 Dan Nagle

!   This library is free software; you can redistribute it and/or
!   modify it under the terms of the GNU Library General Public
!   License as published by the Free Software Foundation; either
!   version 2 of the License, or (at your option) any later version.

!   This library is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!   Library General Public License for more details.

!   You should have received a copy of the GNU Library General Public
!   License along with this library; if not, write to the Free
!   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

! To report bugs, suggest enhancements, or other communication to the Authors,
! Contact:
!    Dan Nagle
!                               send email to dannagle@verizon.net

!  This file contains three program units, two modules and a program.

!  module iterator- type iterator_t

!  The first module defines an "iterator type", which names the basic
!  services needed by any floating point based calculation requiring
!  iteration for its solution.  Using the abstract type allows
!  a programmer to write code that uses any type extended from the abstract type.
!  The details of the calculation are hidden in the extended types.

!  module field_iterator- type field_t

!  The second module adds data which needs to be iterated and procedures
!  to provide the iteration.  (In this example, a simple 1-, 2-, or 3-d field
!  to be solved for boundary conditions via nearest neighbor smoothing.)

!  program heat_iteration- compute heat distribution

!  The program initializes the iteration and computes the iteration using
!  the procedures supplied.  Since "all the work" is specified within
!  the iterator_t and the field_t, the program itself is especially simple.

!  Not too much attention has been spent with nonessential details,
!  such as input/output.

! **********************************************************************
! iterator description

!  This module defines an abstract type which may be extended
!  to form other types which supports simple iteration.  That is,
!  iteration may be done by type bound procedures of the extended
!  types.  Note that no variables of the abstract type may be declared;
!  its only use is to be extended.

!  The iteration supported is a simple iteration as seen in many
!  floating point calculations, it is designed so that either achieving
!  a prescribed tolerance or exceeding a maximum number of iterations
!  without achieving a tolerance may be detected.

! **********************************************************************

!  iterator

! **********************************************************************

module iterator

! **********************************************************************

!  select the real kind to use

use, intrinsic :: iso_fortran_env, only: real_k => real64

! **********************************************************************

!  explicit names

implicit none

! **********************************************************************

!  explicit exports

private

!  export the real kind used

public :: real_k

! **********************************************************************

!  iterator RCS strings

! **********************************************************************

!  module source filename supplied by RCS

character( len= *), public, parameter :: iterator_rcs_id = &
   '$Id: iterator.f90 1.1 2003/05/10 13:24:25Z Dan Exp $'

! **********************************************************************

!  iterator types

! **********************************************************************

!  define the abstract type

!  The name of the type is iterator_t.
!  Public means (after the private above, which applies to the entire module),
!  that the name is visible outside this module.
!  Abstract means no variable of this type may be declared,
!  the only purpose of this type is to be extended by other types.

!  This abstract type codifies what any real-based iteration must have:
!  a count of iterations and a maximum count, and a tolerance for convergence.
!  An iteration must also have a way to initialize its data, a way
!  to take a single step, and a test for convergence.

type, public, abstract :: iterator_t

!  any iteration will have an iteration count
!  by default, at the beginning, there are zero iterations

   integer :: iteration_count = 0

!  any iteration will have a maximum number of iterations allowed
!  see the setup procedure

   integer :: maximum_count = 5000

!  any iteration will have a tolerance indicating convergence
!  again, see the setup procedure

   real( kind= real_k) :: tolerance = 1.0e-10_real_k

!  these are procedures which must be supplied by any iteration

contains

!  The name in parenthesis following the procedure keyword
!  is the name of an abstract interface,
!  any actual procedure purporting to fulfill the role
!  of the deferred procedure must match the abstract interface.

!  pass means that the procedure may be referenced similarly to a component,
!  that is, using the variable% name[( args)] syntax where variable is passed
!  as the pass argument

!  deferred means that this type does not actually have such a procedure,
!  but that any type which is going to extend this type,
!  that is, to be used to actually declare variables
!  must supply such a procedure with an interface matching the abstract one

!  any iteration must be initialized

   procedure( setup_signature), pass( object), deferred :: setup_iterator

!  any iteration must define one step of the iteration

   procedure( step_signature), pass( object), deferred :: step_iterator

!  any iteration must define its convergence criteria

   procedure( converged_signature), pass( object), deferred :: converged_iterator

!  any iteration must define its maximum count criteria

   procedure( runaway_signature), pass( object), deferred :: runaway_iterator

!  any iteration must define a write routine

   procedure( write_signature), pass( object), deferred :: write_iterator

end type iterator_t

!  the abstract interfaces do not represent actual procedures
!  (as would ordinary interfaces) but rather they are the signatures
!  which any actual procedure must match in order to be
!  a procedure fulfilling the role of the deferred procedure

!  define the interfaces the iterator_t deferred procedures must have
!
abstract interface

!  any procedure fulfilling the role of the setup procedure
!  must match this interface

!  this subroutine initializes the iteration

   subroutine setup_signature( object, maximum_steps, set_tolerance)

      import :: iterator_t, real_k

!  class means any variable of a type extended from iterator_t

      class( iterator_t), intent( out) :: object

!  might want to change the default maximum number of iterations

      integer, optional, intent( in) :: maximum_steps

!  might want to change the tolerance

      real( kind= real_k), optional, intent( in) :: set_tolerance

   end subroutine setup_signature

!  any procedure fulfilling the role of the iterate_once procedure
!  must match this interface

!  this subroutine computes one step of the iteration

   subroutine step_signature( object)

      import :: iterator_t

!  class means any variable of a type extended from iterator_t

      class( iterator_t), intent( in out) :: object

   end subroutine step_signature

!  any procedure fulfilling the role of the converged procedure
!  must match this interface

!  this function returns true when the iteration has converged

   function converged_signature( object) result( converged)

      import :: iterator_t

!  the function result type

      logical :: converged

!  class means any variable of a type extended from iterator_t

      class( iterator_t), intent( in) :: object

   end function converged_signature

!  any procedure fulfilling the role of the maximum count procedure
!  must match this interface

!  this function returns true when the iteration has runaway

   function runaway_signature( object) result( converged)

      import :: iterator_t

!  the function result type

      logical :: converged

!  class means any variable of a type extended from iterator_t

      class( iterator_t), intent( in) :: object

   end function runaway_signature

!  any procedure fulfilling the role of the write procedure
!  must match this interface

!  this writes the value of the object to *

   subroutine write_signature( object)

      import :: iterator_t

!  class means any variable of a type extended from iterator_t

      class( iterator_t), intent( in) :: object

   end subroutine write_signature

end interface

! **********************************************************************

!  iterator

! $Id: iterator.f90 1.1 2003/05/10 13:24:25Z Dan Exp $
! **********************************************************************

end module iterator

! **********************************************************************
! Fortran 2003 module field_iterator

! **********************************************************************
! field_iterator description

!  This module defines a type which is an extension of the iterator_t
!  defined in module iterator.  This type is a 2 dimensional field,
!  with boundary conditions along the four edges.  The iteration defined
!  for this type is a simple Poisson iteration, that is,
!  a_i,j = 0.25 * ( a_i-1,j + a_i+1,j + a_i,j-1 + a_i,j+1 )
!  This could be any 2-d diffusion calculation.

! **********************************************************************

!  field_iterator

! **********************************************************************

module field_iterator

! **********************************************************************

!  abstract iteration type and the real kind to use

use :: iterator, only: iterator_t, real_k

! **********************************************************************

!  explicit names

implicit none

! **********************************************************************

!  explicit exports

private

! **********************************************************************

!  field_iterator RCS strings

! **********************************************************************

!  module source filename supplied by RCS

character( len= *), public, parameter :: field_iterator_rcs_id = &
   '$Id: iterator.f90 1.1 2003/05/10 13:24:25Z Dan Exp $'

! **********************************************************************

!  field_iterator types

! **********************************************************************

!  this type adds to the iterator_t the components needed for the 1-d field

type, public, extends( iterator_t) :: field_1d_t

!  the components are known only to module procedures

   private

!  the values of interest

   real( kind= real_k), dimension( :, :), allocatable :: values

!  the target of the next update

   integer :: update = 2

contains

!  initialize the iteration

   procedure, pass( object) :: setup_iterator => setup_field_1d

   generic :: setup => setup_iterator

!  one step

   procedure, pass( object) :: step_iterator => step_field_1d

   generic :: step => step_iterator

!  compute convergence or not

   procedure, pass( object) :: converged_iterator => converged_field_1d

   generic :: converged => converged_iterator

!  compute runaway or not

   procedure, pass( object) :: runaway_iterator => runaway_field_1d

   generic :: runaway => runaway_iterator

!  write values to *

   procedure, pass( object) :: write_iterator => write_field_1d

   generic :: write_field => write_iterator

!  deallocate the data component

!   final :: final_field_1d

!sV adding another stepper
  
   procedure, pass( object) :: step_field_1d_2

   generic :: step2 => step_field_1d_2

end type field_1d_t

! **********************************************************************

!  this type adds to the iterator_t the components needed for the 2-d field

type, public, extends( iterator_t) :: field_2d_t

!  the components are known only to module procedures

   private

!  the values of interest

   real( kind= real_k), dimension( :, :, :), allocatable :: values

!  the target of the next update

   integer :: update = 2

contains

!  initialize the iteration

   procedure, pass( object) :: setup_iterator => setup_field_2d

   generic :: setup => setup_iterator

!  one step

   procedure, pass( object) :: step_iterator => step_field_2d

   generic :: step => step_iterator

!  compute convergence or not

   procedure, pass( object) :: converged_iterator => converged_field_2d

   generic :: converged => converged_iterator

!  compute runaway or not

   procedure, pass( object) :: runaway_iterator => runaway_field_2d

   generic :: runaway => runaway_iterator

!  write values to *

   procedure, pass( object) :: write_iterator => write_field_2d

   generic :: write_field => write_iterator

!  deallocate the data component

!   final :: final_field_2d

end type field_2d_t

! **********************************************************************

!  this type adds to the iterator_t the components needed for the 2-d field

type, public, extends( iterator_t) :: field_3d_t

!  the components are known only to module procedures

   private

!  the values of interest

   real( kind= real_k), dimension( :, :, :, :), allocatable :: values

!  the target of the next update

   integer :: update = 2

contains

!  initialize the iteration

   procedure, pass( object) :: setup_iterator => setup_field_3d

   generic :: setup => setup_iterator

!  one step

   procedure, pass( object) :: step_iterator => step_field_3d

   generic :: step => step_iterator

!  compute convergence or not

   procedure, pass( object) :: converged_iterator => converged_field_3d

   generic :: converged => converged_iterator

!  compute runaway or not

   procedure, pass( object) :: runaway_iterator => runaway_field_3d

   generic :: runaway => runaway_iterator

!  write values to *

   procedure, pass( object) :: write_iterator => write_field_3d

   generic :: write_field => write_iterator

!  deallocate the data component

!   final :: final_field_3d

end type field_3d_t

! **********************************************************************

!  module procedures

! **********************************************************************

!  since field_t extends iterator_t, it must define the procedures
!  deferred in the definition of iterator_t

contains

! **********************************************************************

!  initialize the field variable

subroutine setup_field_1d( object, maximum_steps, set_tolerance)

!  heat is the pass argument, it is the "this" variable

class( field_1d_t), intent( out) :: object

integer, optional, intent( in) :: maximum_steps

real( kind= real_k), optional, intent( in) :: set_tolerance

! **********************************************************************

!  setup_field_1d constants

! ----------------------------------------------------------------------

!  define zero so the kind parameter need not be repeated everywhere

real( kind= real_k), parameter :: zero = 0.0_real_k

! **********************************************************************

!  setup_field_1d local

! ----------------------------------------------------------------------

!  the sizes of the field

   integer :: nx

! ----------------------------------------------------------------------

!  read the field values along the four boundaries

   real( kind= real_k) :: a, b

! **********************************************************************

!  initialize the iteration

continue

!  a default value for maximum_count was specified
!  in the definition of iterator_t so it will be overridden
!  only if a maximum_steps argument is present

!  if a maximum iteration count was passed, test it

   set_max: if( present( maximum_steps) )then

!  if maximum iteration count is valid, use it

      valid_max: if( maximum_steps > 0 )then

         object% maximum_count = maximum_steps

      end if valid_max

   end if set_max

!  the current iteration count is zero by default
!  so this procedure needn't set it

! ----------------------------------------------------------------------

!  a default value for tolerance was not specified
!  in the definition of iterator_t so it must be set here
!  the valus of set_tolerance is used if present present
!  otherwise the default value is supplied here

!  if a tolerance was passed, test it

   set_tol: if( present( set_tolerance) )then

!  if tolerance is valid, use it

      valid_tol: if( set_tolerance > zero )then

         object% tolerance = max( set_tolerance, epsilon( set_tolerance))

      end if valid_tol

   end if set_tol

! ----------------------------------------------------------------------

!  read the boundary conditions

   read( unit= *, fmt= *) nx

   allocate( object% values( nx, 2))

!  read the boundary conditions

   read( unit= *, fmt= *) a, b

!  duplicate for the other half

   object% values( 1, :) = a
   object% values( nx, :) = b

!  initialize the internal field values

   object% values( 2: nx - 1, 1) = zero

! ----------------------------------------------------------------------

return

end subroutine setup_field_1d

! **********************************************************************

!  compute one step of the iteration

subroutine step_field_1d( object)

!  constant in update

real( kind= real_k), parameter :: half = 0.5_real_k

!  heat is the pass argument, it is the "this" variable

class( field_1d_t), intent( in out) :: object

!  source and sink of update

   integer :: to_side, from_side

!  size of values

   integer :: n1

! ----------------------------------------------------------------------

!  one step of a simple Poisson iteration

continue

   n1 = size( object% values, dim= 1)

!  alternate 1 -> 2 then 2 -> 1

   from_side = 3 - object% update

   to_side = object% update

   object% update = from_side

!  smooth the field one time

   object% values( 2: n1 - 1, to_side) = half * ( object% values( 1: n1 - 2, from_side) &
                                                + object% values( 3: n1, from_side) )

!  count the steps

   object% iteration_count = object% iteration_count + 1

return

end subroutine step_field_1d

! ----------------------------------------------------------------------
! ----------------------------------------------------------------------

subroutine step_field_1d_2( object)

!  constant in update

real( kind= real_k), parameter :: half = 0.5_real_k
real( kind= real_k), parameter :: forth = 0.25_real_k

!  heat is the pass argument, it is the "this" variable

class( field_1d_t), intent( in out) :: object

!  source and sink of update

   integer :: to_side, from_side

!  size of values

   integer :: n1

! ----------------------------------------------------------------------

!  one step of a simple Poisson iteration

continue

   n1 = size( object% values, dim= 1)

!  alternate 1 -> 2 then 2 -> 1

   from_side = 3 - object% update

   to_side = object% update

   object% update = from_side

!  smooth the field one time

   object% values( 3: n1 - 1, to_side) = forth *( ( object% values( 1: n1 - 2, from_side) &
                                                + object% values( 2: n1, from_side)  &
                                                + object% values( 4: n1, from_side)  &
                                                + object% values( 5: n1, from_side) ))

!  count the steps

   object% iteration_count = object% iteration_count + 1

return

end subroutine step_field_1d_2
! **********************************************************************

subroutine write_field_1d( object)

!  heat is the pass argument, it is the "this" variable

class( field_1d_t), intent( in) :: object

!  compute converged or not

continue

   write( unit= *, fmt= *) object% iteration_count

!  write a row on a line

   write( unit= *, fmt= *) object% values( :, object% update)

return

end subroutine write_field_1d

! **********************************************************************

function converged_field_1d( object) result( converged_example)
logical :: converged_example

!  heat is the pass argument, it is the "this" variable

class( field_1d_t), intent( in) :: object

!  size of values

   integer :: n1

!  compute converged or not

continue

   n1 = size( object% values, dim= 1)

!  convergence is when the maximum delta is less than the prescribed tolerance

   converged_example = maxval( abs( object% values( 2: n1 - 1, 1) &
                                  - object% values( 2: n1 - 1, 2) )) < object% tolerance

return

end function converged_field_1d

! **********************************************************************

function runaway_field_1d( object) result( runaway_example)
logical :: runaway_example

!  heat is the pass argument, it is the "this" variable

class( field_1d_t), intent( in) :: object

!  compute runaway or not

continue

!  convergence is when the maximum delta is less than the prescribed tolerance

   runaway_example = object% iteration_count > object% maximum_count

return

end function runaway_field_1d

! **********************************************************************

subroutine final_field_1d( object)

!  heat is the pass argument, it is the "this" variable

type( field_1d_t) :: object

!  compute runaway or not

continue

!  if allocated then deallocate the data

   if( allocated( object% values) ) deallocate( object% values)

return

end subroutine final_field_1d

! **********************************************************************

!  initialize the field variable

subroutine setup_field_2d( object, maximum_steps, set_tolerance)

!  heat is the pass argument, it is the "this" variable

class( field_2d_t), intent( out) :: object

integer, optional, intent( in) :: maximum_steps

real( kind= real_k), optional, intent( in) :: set_tolerance

! **********************************************************************

!  setup_field_2d constants

! ----------------------------------------------------------------------

!  define zero so the kind parameter need not be repeated everywhere

real( kind= real_k), parameter :: zero = 0.0_real_k

! **********************************************************************

!  setup_field_2d local

! ----------------------------------------------------------------------

!  the sizes of the field

   integer :: nx, ny

! ----------------------------------------------------------------------

!  read the field values along the four boundaries

   real( kind= real_k) :: a, b, c, d

! **********************************************************************

!  initialize the iteration

continue

!  a default value for maximum_count was specified
!  in the definition of iterator_t so it will be overridden
!  only if a maximum_steps argument is present

!  if a maximum iteration count was passed, test it

   set_max: if( present( maximum_steps) )then

!  if maximum iteration count is valid, use it

      valid_max: if( maximum_steps > 0 )then

         object% maximum_count = maximum_steps

      end if valid_max

   end if set_max

!  the current iteration count is zero by default
!  so this procedure needn't set it

! ----------------------------------------------------------------------

!  a default value for tolerance was not specified
!  in the definition of iterator_t so it must be set here
!  the valus of set_tolerance is used if present present
!  otherwise the default value is supplied here

!  if a tolerance was passed, test it

   set_tol: if( present( set_tolerance) )then

!  if tolerance is valid, use it

      valid_tol: if( set_tolerance > zero )then

         object% tolerance = max( set_tolerance, epsilon( set_tolerance))

      end if valid_tol

   end if set_tol

! ----------------------------------------------------------------------

!  read the boundary conditions

   read( unit= *, fmt= *) nx, ny

   allocate( object% values( nx, ny, 2))

!  read the boundary conditions

   read( unit= *, fmt= *) a, b, c, d

!  duplicate for the other half

   object% values( 1, :, :) = a
   object% values( nx, :, :) = b
   object% values( :, 1, :) = c
   object% values( :, ny, :) = d

!  initialize the internal field values

   object% values( 2: nx - 1, 2: ny - 1, 1) = zero

! ----------------------------------------------------------------------

return

end subroutine setup_field_2d

! **********************************************************************

!  compute one step of the iteration

subroutine step_field_2d( object)

!  constant in update

real( kind= real_k), parameter :: quarter = 0.25_real_k

!  heat is the pass argument, it is the "this" variable

class( field_2d_t), intent( in out) :: object

!  source and sink of update

   integer :: to_side, from_side

!  size of values

   integer :: n1, n2

! ----------------------------------------------------------------------

!  one step of a simple Poisson iteration

continue

   n1 = size( object% values, dim= 1)
   n2 = size( object% values, dim= 2)

!  alternate 1 -> 2 then 2 -> 1

   from_side = 3 - object% update

   to_side = object% update

   object% update = from_side

!  smooth the field one time

   object% values( 2: n1 - 1, 2: n2 - 1, to_side) = quarter * ( object% values( 1: n1 - 2, 2: n2 - 1, from_side) &
                                                              + object% values( 3: n1, 2: n2 - 1, from_side) &
                                                              + object% values( 2: n1 - 1, 1: n2 - 2, from_side) &
                                                              + object% values( 2: n1 - 1, 3: n2, from_side) )

!  count the steps

   object% iteration_count = object% iteration_count + 1

return

end subroutine step_field_2d

! **********************************************************************

subroutine write_field_2d( object)

!  heat is the pass argument, it is the "this" variable

class( field_2d_t), intent( in) :: object

!  size of values

   integer :: n1

!  loop index

   integer :: i

!  compute converged or not

continue

   n1 = size( object% values, dim= 1)

   write( unit= *, fmt= *) object% iteration_count

!  write a row on a line

   each_row: do i = 1, n1

      write( unit= *, fmt= *) i, object% values( i, :, object% update)

   end do each_row

return

end subroutine write_field_2d

! **********************************************************************

function converged_field_2d( object) result( converged_example)
logical :: converged_example

!  heat is the pass argument, it is the "this" variable

class( field_2d_t), intent( in) :: object

!  size of values

   integer :: n1, n2

!  compute converged or not

continue

   n1 = size( object% values, dim= 1)
   n2 = size( object% values, dim= 2)

!  convergence is when the maximum delta is less than the prescribed tolerance

   converged_example = maxval( abs( object% values( 2: n1 - 1, 2: n2 - 1, 1) &
                                  - object% values( 2: n1 - 1, 2: n2 - 1, 2) )) < object% tolerance

return

end function converged_field_2d

! **********************************************************************

function runaway_field_2d( object) result( runaway_example)
logical :: runaway_example

!  heat is the pass argument, it is the "this" variable

class( field_2d_t), intent( in) :: object

!  compute runaway or not

continue

!  convergence is when the maximum delta is less than the prescribed tolerance

   runaway_example = object% iteration_count > object% maximum_count

return

end function runaway_field_2d

! **********************************************************************

subroutine final_field_2d( object)

!  heat is the pass argument, it is the "this" variable

type( field_2d_t) :: object

!  compute runaway or not

continue

!  if allocated then deallocate the data

   if( allocated( object% values) ) deallocate( object% values)

return

end subroutine final_field_2d

! **********************************************************************

!  initialize the field variable

subroutine setup_field_3d( object, maximum_steps, set_tolerance)

!  heat is the pass argument, it is the "this" variable

class( field_3d_t), intent( out) :: object

integer, optional, intent( in) :: maximum_steps

real( kind= real_k), optional, intent( in) :: set_tolerance

! **********************************************************************

!  setup_field_3d constants

! ----------------------------------------------------------------------

!  define zero so the kind parameter need not be repeated everywhere

real( kind= real_k), parameter :: zero = 0.0_real_k

! **********************************************************************

!  setup_field_3d local

! ----------------------------------------------------------------------

!  the sizes of the field

   integer :: nx, ny, nz

! ----------------------------------------------------------------------

!  read the field values along the four boundaries

   real( kind= real_k) :: a, b, c, d, e, f

! **********************************************************************

!  initialize the iteration

continue

!  a default value for maximum_count was specified
!  in the definition of iterator_t so it will be overridden
!  only if a maximum_steps argument is present

!  if a maximum iteration count was passed, test it

   set_max: if( present( maximum_steps) )then

!  if maximum iteration count is valid, use it

      valid_max: if( maximum_steps > 0 )then

         object% maximum_count = maximum_steps

      end if valid_max

   end if set_max

!  the current iteration count is zero by default
!  so this procedure needn't set it

! ----------------------------------------------------------------------

!  a default value for tolerance was not specified
!  in the definition of iterator_t so it must be set here
!  the valus of set_tolerance is used if present present
!  otherwise the default value is supplied here

!  if a tolerance was passed, test it

   set_tol: if( present( set_tolerance) )then

!  if tolerance is valid, use it

      valid_tol: if( set_tolerance > zero )then

         object% tolerance = max( set_tolerance, epsilon( set_tolerance))

      end if valid_tol

   end if set_tol

! ----------------------------------------------------------------------

!  read the boundary conditions

   read( unit= *, fmt= *) nx, ny, nz

   allocate( object% values( nx, ny, nz, 2))

!  read the boundary conditions

   read( unit= *, fmt= *) a, b, c, d, e, f

!  duplicate for the other half

   object% values( 1, :, :, :) = a
   object% values( nx, :, :, :) = b
   object% values( :, 1, :, :) = c
   object% values( :, ny, :, :) = d
   object% values( :, :, 1, :) = e
   object% values( :, :, nz, :) = f

!  initialize the internal field values

   object% values( 2: nx - 1, 2: ny - 1, 2: nz - 1, 1) = zero

! ----------------------------------------------------------------------

return

end subroutine setup_field_3d

! **********************************************************************

!  compute one step of the iteration

subroutine step_field_3d( object)

!  constant in update

real( kind= real_k), parameter :: sixth = 1.0_real_k / 6.0_real_k

!  heat is the pass argument, it is the "this" variable

class( field_3d_t), intent( in out) :: object

!  source and sink of update

   integer :: to_side, from_side

!  size of values

   integer :: n1, n2, n3

! ----------------------------------------------------------------------

!  one step of a simple Poisson iteration

continue

   n1 = size( object% values, dim= 1)
   n2 = size( object% values, dim= 2)
   n3 = size( object% values, dim= 3)

!  alternate 1 -> 2 then 2 -> 1

   from_side = 3 - object% update

   to_side = object% update

   object% update = from_side

!  smooth the field one time

   object% values( 2: n1 - 1, 2: n2 - 1, 2: n3 - 1, to_side) = sixth * ( &
                                           object% values( 1: n1 - 2, 2: n2 - 1, 2: n3 - 1, from_side) &
                                         + object% values( 3: n1, 2: n2 - 1, 2: n3 - 1, from_side) &
                                         + object% values( 2: n1 - 1, 1: n2 - 2, 2: n3 - 1, from_side) &
                                         + object% values( 2: n1 - 1, 3: n2, 2: n3 - 1, from_side) &
                                         + object% values( 2: n1 - 1, 2: n2 - 1, 1: n3 - 2, from_side) &
                                         + object% values( 2: n1 - 1, 2: n2 - 1, 3: n3, from_side) )

!  count the steps

   object% iteration_count = object% iteration_count + 1

return

end subroutine step_field_3d

! **********************************************************************

subroutine write_field_3d( object)

!  heat is the pass argument, it is the "this" variable

class( field_3d_t), intent( in) :: object

!  size of values

   integer :: n1, n2

!  loop index

   integer :: i, j

!  compute converged or not

continue

   n1 = size( object% values, dim= 1)
   n2 = size( object% values, dim= 2)

   write( unit= *, fmt= *) object% iteration_count

!  write a row on a line

   each_row: do i = 1, n1

      each_column: do j = 1, n2

         write( unit= *, fmt= *) i, j, object% values( i, j, :, object% update)

      end do each_column

   end do each_row

return

end subroutine write_field_3d

! **********************************************************************

function converged_field_3d( object) result( converged_example)
logical :: converged_example

!  heat is the pass argument, it is the "this" variable

class( field_3d_t), intent( in) :: object

!  size of values

   integer :: n1, n2, n3

!  compute converged or not

continue

   n1 = size( object% values, dim= 1)
   n2 = size( object% values, dim= 2)
   n3 = size( object% values, dim= 3)

!  convergence is when the maximum delta is less than the prescribed tolerance

   converged_example = maxval( abs( object% values( 2: n1 - 1, 2: n2 - 1, 2: n3 - 1, 1) &
                                  - object% values( 2: n1 - 1, 2: n2 - 1, 2: n3 - 1, 2) )) < object% tolerance

return

end function converged_field_3d

! **********************************************************************

function runaway_field_3d( object) result( runaway_example)
logical :: runaway_example

!  heat is the pass argument, it is the "this" variable

class( field_3d_t), intent( in) :: object

!  compute runaway or not

continue

!  convergence is when the maximum delta is less than the prescribed tolerance

   runaway_example = object% iteration_count > object% maximum_count

return

end function runaway_field_3d

! **********************************************************************

subroutine final_field_3d( object)

!  heat is the pass argument, it is the "this" variable

type( field_3d_t) :: object

!  compute runaway or not

continue

!  if allocated then deallocate the data

   if( allocated( object% values) ) deallocate( object% values)

return

end subroutine final_field_3d

! $Id: iterator.f90 1.1 2003/05/10 13:24:25Z Dan Exp $
! **********************************************************************

end module field_iterator

! **********************************************************************
! Fortran 2003 program heat_iteration

! **********************************************************************
! heat_iteration describe the program

! **********************************************************************

!  heat_iteration

! **********************************************************************

program heat_iteration

! **********************************************************************

!  heat_iteration uses modules

! **********************************************************************

!  data and procedures for this problem

!  this program is designed using the abstract iterator type so it can use
!  any type extended from the iterator type-
!  all that must be done to switch is change the type field_t renames
!  field_t can rename field_1d_t or field_2d_t or field_3d_t

!use :: field_iterator, only: field_t => field_3d_t
use :: field_iterator, only: field_t => field_1d_t

! **********************************************************************

!  turn off implicit typing

implicit none

! **********************************************************************

!  heat_iteration RCS strings

! **********************************************************************

!  program source filename supplied by RCS

character( len= *), parameter :: heat_iteration_rcs_id = &
   '$Id: iterator.f90 1.1 2003/05/10 13:24:25Z Dan Exp $'

! **********************************************************************

!  heat_iteration data

! **********************************************************************

!  heat is a variable of type field
!  heat has all the components of the iterator type and the field type

type( field_t) :: heat

! **********************************************************************

!  heat_iteration text

! **********************************************************************

continue

!  initialize the iteration

   call heat% setup()

!  iterate until success or failure

   converge: do

!  one step of the iteration

      call heat% step2()

!  test success

      iteration_converged: if( heat% converged() )then

         write( unit= *, fmt= *) 'heat iteration converged'

         call heat% write_field()

         exit converge

      end if iteration_converged

!  test failure

      iteration_failed: if( heat% runaway() )then

         write( unit= *, fmt= *) 'heat iteration runaway'

         exit converge

      end if iteration_failed

!  end iterate until success or failure

   end do converge

!  day is done

stop 'heat_iteration'

! **********************************************************************

!  heat_iteration

! $Id: iterator.f90 1.1 2003/05/10 13:24:25Z Dan Exp $
! **********************************************************************

end program heat_iteration
