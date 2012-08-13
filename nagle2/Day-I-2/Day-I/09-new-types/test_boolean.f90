
!  test type boolean

program test_boolean

use :: type_boolean

type( boolean_t) :: a

real :: r

integer :: i

continue

   read( unit= *, fmt= *) r

   a = r

   i = a

   write( unit= *, fmt= *) i

stop 'normal exit'

end program test_boolean
