

module test2use

type :: foo
  real:: a
  integer :: b
end type foo

type :: bar
  integer :: a
  real :: b
end type

end module



program test
use test2use, only : foo
!use test2use, only : bar


type (foo) :: firstUse
type (bar) :: secondUse


end program
