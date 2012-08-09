module palette_stuff

type :: palette
!...
end type palette

contains

subroutine test_palette ( p )

! Draw a color wheel using procedures from the color_points module

! This does not cause a circular dependency because
! the "use palette_stuff" that is logically within
! color_points is in the color_points_a submodule.

use color_points


type(palette), intent(in) :: p

!      ...

end subroutine test_palette

end module palette_stuff

! Subsidiary**2 submodule

submodule ( color_points:color_points_a ) color_points_b

contains

! Invisible body for interface declared in the ancestor module

module subroutine color_point_draw ( p )

use palette_stuff, only: palette

type(color_point), intent(in) :: p
type(palette) :: MyPalette

!      ...

call inquire_palette ( p, MyPalette ); ...

end subroutine color_point_draw

! Invisible body for interface declared in the parent submodule

module procedure inquire_palette

! ... implementation of inquire_palette

end procedure inquire_palette

! not accessible from color_points_a

subroutine private_stuff

! ...

end subroutine private_stuff

end submodule color_points_b
