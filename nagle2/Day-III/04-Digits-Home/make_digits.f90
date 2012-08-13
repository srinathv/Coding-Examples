program make_digits

character( len= 1), dimension( 4), parameter :: digits_4 = [ '1', '2', '3', '4']

   integer :: i, j, k, l

continue

   do l = 1, 4

      do k = 1, 4

         do j = 1, 4

            do i = 1, 4

               write( unit= *, fmt= *) digits_4( i) // digits_4( j) // digits_4( k) // digits_4( l)

            end do

         end do

      end do

   end do

stop 'normal exit in digits_home'

end program make_digits
