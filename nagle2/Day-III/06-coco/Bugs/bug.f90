program bug
implicit none

type, abstract :: p_t
end type p_t

type, extends( p_t) :: c_t
end type c_t

class( p_t), pointer :: p_p

continue

select type( p_p)

type is( c_t)
   call foo( p_p)
end select

stop "What! Me worry?"

contains

subroutine foo( ptr)
type( c_t), pointer :: ptr
end subroutine foo

end program bug
