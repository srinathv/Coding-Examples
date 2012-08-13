
! Submodule of color_points

submodule ( color_points ) color_points_a

   integer :: instance_count = 0

! Interface for a procedure with a separate
! body in submodule color_points_b

interface

   module subroutine inquire_palette ( pt, pal )
      use palette_stuff

! palette_stuff, especially submodules
! thereof, can reference color_points by use
! association without causing a circular
! dependence during translation because this
! use is not in the module.  Furthermore,
! changes in the module palette_stuff do not
! affect the translation of color_points.

      type( color_point), intent( in) :: pt
      type( palette), intent( out) :: pal

   end subroutine inquire_palette
end interface

! Invisible bodies for public separate module procedures
! declared in the module

contains

module subroutine color_point_del ( p )
   type(color_point), allocatable :: p
   instance_count = instance_count - 1
   deallocate ( p )
end subroutine color_point_del

real module function color_point_dist (a,b) result(dist)
   type(color_point), intent(in) :: a, b
   dist = sqrt( (b%x - a%x)**2 + (b%y - a%y)**2 )
end function color_point_dist

module subroutine color_point_new ( p )
   type(color_point), allocatable :: p
   instance_count = instance_count + 1
   allocate ( p )
end subroutine color_point_new

end submodule color_points_a
