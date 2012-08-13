

module foo
  implicit none

  integer :: fooBar

end module foo

subroutine bar
  use foo
  implicit none


  fooBar =3

end subroutine bar



