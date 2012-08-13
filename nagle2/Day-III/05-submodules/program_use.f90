program main

use color_points

! "instance_count" and "inquire_palette" are not accessible here
! because they are not declared in the "color_points" module.
! "color_points_a" and "color_points_b" cannot be referenced by
! use association.

! just to demonstrate it’s possible

interface draw
   module procedure color_point_draw
end interface

type(color_point) :: C_1, C_2
real :: RC

!  ...
! body in color_points_a, interface in color_points

   call color_point_new (c_1)

!  ...
! body in color_points_b, specific interface
! in color_points, generic interface here.

   call draw (c_1)

!  ...
! body in color_points_a, interface in color_points

   rc = color_point_dist (c_1, c_2)

!  ...
! body in color_points_a, interface in color_points

   call color_point_del (c_1)

!  ...

end program main
