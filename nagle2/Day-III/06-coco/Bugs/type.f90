program bugger

type :: thingo_t
end type thingo_t

type, extends( thingo_t) :: sonofthingo_t
end type sonofthingo_t

class( thingo_t), pointer :: pt

continue

   select type( pt)
   type in( sonofthingo_t)
   end select

stop 'whew!'

end program bugger
