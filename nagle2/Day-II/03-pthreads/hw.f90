program hello_world

use, intrinsic :: iso_c_binding
use pthreads

integer, parameter :: number_of_threads = 4

type( pthread_t), dimension( 1: number_of_threads), target :: threads

integer :: i, th_stat
integer( kind= c_int), dimension( 1: number_of_threads), target :: tasks

interface

   recursive function task( me) result( code) bind( c)
   use, intrinsic :: iso_c_binding
   integer( c_int), intent( in) :: me
   integer( c_int) :: code
   end function task

end interface

continue

   write( unit= *, fmt= *) 'Hello, world!'

   start_tasks: do i = 1, number_of_threads

      tasks( i) = i
      th_stat = pthread_create( &
         c_loc( threads( i)), c_null_ptr, c_funloc( task), c_loc( tasks( i)))
      write( unit= *, fmt= *) 'started task ', i, ' with code ', th_stat

   end do start_tasks

   wait_tasks: do i = 1, number_of_threads

      th_stat = pthread_join( threads( i), c_loc( tasks( i)))
      write( unit= *, fmt= *) 'joined task ', i, ' with code ', th_stat, &
                              ' task returned ', tasks( i)

   end do wait_tasks

stop 'normal exit in hello_world'

end program hello_world

recursive function task( me) result( code) bind( c)
use, intrinsic :: iso_c_binding

integer( kind= c_int), target, intent( in) :: me
integer( kind= c_int) :: code
integer :: i, sums

continue

   code = me
   do i=1,10
      sums = sums+me
   end do
   write( unit= *, fmt= *) 'I am ', me, ' and I sum to ', sums
  

return

end function task
