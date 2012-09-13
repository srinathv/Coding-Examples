program test
implicit none
integer ni,nj,i,j
real(8) sum

ni=1
nj=100

sum=0.d0

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(j)
!$OMP DO
do i=1,ni
!$OMP PARALLEL DEFAULT(SHARED)
!$OMP DO
  do j=1,nj
    call work(sum)
  end do
!$OMP END DO
!$OMP END PARALLEL
end do
!$OMP END DO
!$OMP END PARALLEL

print *,sum

end program


subroutine work(sum)
implicit none
real(8) sum
integer i
do i=1,100000000
  sum=sum+sin(dble(i))
end do
return
end subroutine
