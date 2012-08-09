! bof
! **********************************************************************
! Fortran 2008 module color_points

! ----------------------------------------------------------------------
! Source Control Strings

! $Id$

! ----------------------------------------------------------------------
! color_points description

! ----------------------------------------------------------------------

!  color_points uses

!     iso_fortran_env- describes the processor

!  color_points includes

!     <none>

!  color_points constants

!  color_points types

!  color_points data

!  color_points library

! **********************************************************************

!  color_points

! ----------------------------------------------------------------------

module color_points

type color_point
   private

   real :: x,y
   integer :: color

end type color_point

! Interfaces for procedures with separate
! bodies in the submodule color_points_a

interface

! Destroy a color_point object

   module subroutine color_point_del ( p )
      import :: color_point
      type( color_point), allocatable :: p
   end subroutine color_point_del

! Distance between two color_point objects

   real module function color_point_dist ( a, b )
      import :: color_point
      type(color_point), intent(in) :: a, b
   end function color_point_dist

! Draw a color_point object

   module subroutine color_point_draw ( p )
      import :: color_point
      type(color_point), intent(in) :: p
   end subroutine color_point_draw

! Create a color_point object

   module subroutine color_point_new ( p )
      import :: color_point
      type(color_point), allocatable :: p
   end subroutine color_point_new

end interface

end module color_points
