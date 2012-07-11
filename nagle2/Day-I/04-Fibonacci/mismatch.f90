program rank_1

integer, dimension( 1000) :: a

continue

   call rank_3( a)

stop 'all done'

end program rank_1

subroutine rank_3( ta, imax, jmax, kmax) ! version 1

integer, dimension( :, :, :), pointer :: pa
integer, dimension( :), target :: ta

continue

   pa( 1: imax, 1: jmax, 1: kmax) => ta

return

end subroutine rank_3

subroutine rank_3( a, imax, jmax, kmax) ! version 2

integer, dimension( imax, jmax, kmax) :: a

continue

return

contains

function a_idx( i, j, k) result( idx)

integer, intent( in) :: i, j, k

continue

! j1:k1, j2:k2, j3:k3 and s1, s2, s3
! offset is 1+(s1 −j1) +(s2 − j2) × d1+(s3 −j3)×d2 ×d1
! di = ki-ji+1
!  see 6.5.3.2 Array element order

   idx = 1 + ( i - 1) + ( j - 1)*( imax - 1) + ( k - 1)*( imax- 1)*( jmax - 1)

return

end function a_idx
