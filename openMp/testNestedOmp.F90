program test
implicit none
integer ni,nj,i,j,id,omp_get_thread_num
real(8) sum



ni=1
!nj=100
nj=8

sum=0.d0

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(j)
  write(*,*) "this is thread num =",id
!$OMP DO
do i=1,ni
#ifdef NESTED
write(*,*) "using nesting"
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(id)
!$OMP DO
  id=omp_get_thread_num()
#endif
  do j=1,nj
    call work(sum)
  end do
#ifdef NESTED
!$OMP END DO
!$OMP END PARALLEL
#endif
end do
!$OMP END DO
!$OMP END PARALLEL

print *,sum

end program


subroutine work(sum)
implicit none
real(8) sum
integer i
do i=1,100
  sum=sum+sin(dble(i))
end do
return
end subroutine
