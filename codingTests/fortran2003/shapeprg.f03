program shape_prg
use shape_mod
type(shape) :: sh
logical filled
sh = constructor(5, .true., 100, 200)
call sh%print()
end
