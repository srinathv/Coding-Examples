module param
     implicit none

! You can adjust the size of this benchmark code to fit your target
! computer. In that case, please chose following sets of
! (mimax,mjmax,mkmax):
! small : 65,33,33
! small : 129,65,65
! medium: 257,129,129
! large : 513,257,257
! ext.large: 1025,513,513

     integer,parameter :: mx0 = 1025, ndx =  1,  mimax = mx0/ndx+3, &  ! configure for XL case
                          my0 = 513,  ndy =  25,  mjmax = my0/ndy+3, &  ! and 1*25*40 = 1000 processors
                          mz0 = 513,  ndz =  40,  mkmax = mz0/ndz+3, &
                          ndims = 3

     integer,dimension(ndims) :: iop
     integer,dimension(2)     :: npx, npy, npz

     integer :: id, npe, ijvec, ikvec, jkvec
     real    :: omega

     integer,dimension[ndx,ndy,*],save                   :: imax, jmax, kmax
     real,dimension(mimax,mjmax,mkmax)[ndx,ndy,*],save   :: p

     real,dimension(mimax,mjmax,mkmax)   :: bnd, wrk1, wrk2
     real,dimension(mimax,mjmax,mkmax,4) :: a
     real,dimension(mimax,mjmax,mkmax,3) :: b,c

contains

!---------------
     subroutine initmt(mz,it)

       integer,intent(in) :: mz,it
       integer            :: k

       a = 0.0
       b = 0.0
       c = 0.0
       p = 0.0
       wrk1 = 0.0
       wrk2 = 0.0
       bnd  = 0.0

       a(1:imax,1:jmax,1:kmax,1:3) = 1.0
       c(1:imax,1:jmax,1:kmax,1:3) = 1.0
       a(1:imax,1:jmax,1:kmax,4)   = 1.0/6.0
       do k=1,kmax
          p(1:imax,1:jmax,k)       = real((k-1+it)**2)/real((mz-1)**2)
       end do
       bnd(1:imax,1:jmax,1:kmax)   = 1.0

     end subroutine initmt

!---------------
     subroutine initcomm

       npe = num_images()
       id  = this_image()
       iop = this_image(p)

       if(ndx*ndy*ndz .ne. npe) then
          if(id .eq. 1) then
             write(*,*) 'Invalid number of PE',npe, ndx, ndy, ndz
             write(*,*) 'Please check partitioning pattern'
             write(*,*) '                 or number of  PE'
          end if
          call sync_all()
          stop
       end if

     end subroutine initcomm


!---------------
     subroutine initmax(mx,my,mz,ks)

       integer,intent(in)  :: mx,my,mz
       integer,intent(out) :: ks

       integer  i
       integer  mx1(0:ndx),my1(0:ndy),mz1(0:ndz)
       integer  mx2(0:ndx),my2(0:ndy),mz2(0:ndz)

!  define imax, communication direction

       mx1(0)= 0
       do  i=1,ndx
          mx1(i)= mx1(i-1) + mx/ndx + merge(1,0, i <= mod(mx,ndx))
       end do
       do i=0,ndx-1
          mx2(i)= mx1(i+1) - mx1(i) + merge(1,0, i /= 0) + merge(1,0, i /= ndx-1)
       end do

       my1(0)= 0
       do  i=1,ndy
          my1(i)= my1(i-1) + my/ndy + merge(1,0, i <= mod(my,ndy))
       end do
       do i=0,ndy-1
          my2(i)= my1(i+1) - my1(i) + merge(1,0, i /= 0) + merge(1,0, i /= ndy-1)
       end do

       mz1(0)= 0
       do  i=1,ndz
          mz1(i)= mz1(i-1) + mz/ndz + merge(1,0, i <= mod(mz,ndz))
       end do
       do i=0,ndz-1
          mz2(i)= mz1(i+1) - mz1(i) + merge(1,0, i /= 0) + merge(1,0, i /= ndz-1)
       end do

       imax = mx2(iop(1)-1)
       jmax = my2(iop(2)-1)
       kmax = mz2(iop(3)-1)

       ks = mz1(iop(3)-1) - merge(0,1, iop(3) == 1)

     end subroutine initmax

end module param



!*********************************************************************
!
! This benchmark test program is measuring a cpu performance
! of floating point operation by a Poisson equation solver.
!
! If you have any question, please ask me via email.
! written by Ryutaro HIMENO, November 26, 2001.
! Version 3.0
! ----------------------------------------------
! Ryutaro Himeno, Dr. of Eng.
! Head of Computer Information Division,
! RIKEN (The Institute of Pysical and Chemical Research)
! Email : himeno@postman.riken.go.jp
! -----------------------------------------------------------
!
! ** Modified for Fortran 2008, Bill Long, longb@cray.com
!
! This program is to measure a computer performance in MFLOPS
! by using a kernel which appears in a linear solver of pressure
! Poisson eq. which appears in an incompressible Navier-Stokes solver.
! A point-Jacobi method is employed in this solver as this method can 
! be easyly vectrized and be parallelized.
! ------------------
! Finite-difference method, curvilinear coodinate system
! Vectorizable and parallelizable on each grid point
! No. of grid points : imax x jmax x kmax including boundaries
! ------------------
! A,B,C:coefficient matrix, wrk1: source term of Poisson equation
! wrk2 : working area, OMEGA : relaxation parameter
! BND:control variable for boundaries and objects ( = 0 or 1)
! P: pressure
! -------------------
     PROGRAM HIMENOBMTXP
       use param
       use ca_intrinsics
       implicit none

       integer,parameter :: ttarget=60.0 ! ttarget specifys the measuring period in sec

       real(8) ::  cpu,cpu1[*],xmflops2,flop,rate,score
       integer(8) :: i1,i2
       integer    :: mx,my,mz,it,nn
       real       :: gosa

       omega=0.8
       mx= mx0-1
       my= my0-1
       mz= mz0-1

       call co_init
       call initcomm             ! Initializing communicator
       call initmax(mx,my,mz,it) ! Initializaing computational index
       call initmt(mz,it)        ! Initializing matrixes

       if(id .eq. 1) then
          write(*,*) 'Sequential version array size'
          write(*,*) ' mimax=',mx0,' mjmax=',my0,' mkmax=',mz0
          write(*,*) 'Parallel version  array size'
          write(*,*) ' mimax=',mimax,' mjmax=',mjmax,' mkmax=',mkmax
          write(*,*) ' imax=',imax,' jmax=',jmax,' kmax=',kmax
          write(*,*) ' I-decomp= ',ndx,' J-decomp= ',ndy,' K-decomp= ',ndz
          write(*,*)
       end if

! Start measuring

       nn=3
       if(id .eq. 1) then
          write(*,*) ' Start rehearsal measurement process.'
          write(*,*) ' Measure the performance in 3 times.'
       end if

       gosa= 0.0
       call sync_all()

       call system_clock(count = i1, count_rate = rate)

! Jacobi iteration

       call jacobi(nn,gosa)
       call system_clock(count = i2)
       cpu1= (i2-i1)/rate

       call co_maxval(cpu1,cpu)

       flop=real(mx-2)*real(my-2)*real(mz-2)*34.0
       if(cpu .ne. 0.0) xmflops2=flop/cpu*1.0d-6*real(nn)
       if(id .eq. 1) then
          write(*,*) '  MFLOPS:',xmflops2,'  time(s):',cpu,gosa
       end if

! End the test loop

       nn= int(ttarget/(cpu/3.0))
       if(id .eq. 1) then
          write(*,*) 'Now, start the actual measurement process.'
          write(*,*) 'The loop will be excuted in',nn,' times.'
          write(*,*) 'This will take about one minute.'
          write(*,*) 'Wait for a while.'
       end if

       gosa= 0.0
       call sync_all()

       call system_clock(count = i1)

! Jacobi iteration

       call jacobi(nn,gosa)
       call system_clock(count = i2)
       cpu1 = (i2-i1)/rate

       call co_maxval(cpu1,cpu)

       if(id .eq. 1) then
          if(cpu .ne. 0.0)  xmflops2=flop*1.0d-6/cpu*real(nn)
          write(*,*) ' Loop executed for ',nn,' times'
          write(*,*) ' Gosa :',gosa
          write(*,*) ' MFLOPS:',xmflops2, '  time(s):',cpu
          score=xmflops2/82.84
          write(*,*) ' Score based on Pentium III 600MHz :',score
       end if

       call sync_all()
     END PROGRAM HIMENOBMTXP

!*************************************************************
     subroutine jacobi(nn,gosa)
!*************************************************************
       use param
       use ca_intrinsics
       implicit none

       integer,intent(in) :: nn
       real,intent(out)   :: gosa
!dir$ no_cache_alloc a,b,c,wrk1

       integer    :: loop,k,j,i
       integer    :: myx, myy, myz
       real,save  :: wgosa[*]
       real       :: S0,SS

       DO loop=1,nn
          gosa=0.0
          wgosa=0.0
!dir$ concurrent
          DO K=2,kmax-1
!dir$ concurrent
             DO J=2,jmax-1
!dir$ concurrent
                DO I=2,imax-1
                   S0=a(I,J,K,1)*p(I+1,J,  K  ) &
                     +a(I,J,K,2)*p(I,  J+1,K  ) &
                     +a(I,J,K,3)*p(I,  J,  K+1) &

                     +b(I,J,K,1)*(p(I+1,J+1,K  )-p(I+1,J-1,K  )-p(I-1,J+1,K  )+p(I-1,J-1,K  )) &
                     +b(I,J,K,2)*(p(I,  J+1,K+1)-p(I,  J-1,K+1)-p(I,  J+1,K-1)+p(I,  J-1,K-1)) &
                     +b(I,J,K,3)*(p(I+1,J,  K+1)-p(I-1,J,  K+1)-p(I+1,J,  K-1)+p(I-1,J,  K-1)) &

                     +c(I,J,K,1)*p(I-1,J,  K  ) &
                     +c(I,J,K,2)*p(I,  J-1,K  ) &
                     +c(I,J,K,3)*p(I,  J,  K-1) &

                     +wrk1(I,J,K)

                   SS=(S0*a(I,J,K,4)-p(I,J,K))*bnd(I,J,K)
                   WGOSA=WGOSA+SS*SS
                   wrk2(I,J,K)=p(I,J,K)+OMEGA *SS
                enddo
             enddo
          enddo


          do k = kmax-1, 2, -1
             p(2:imax-1, 2:jmax-1, k) = wrk2(2:imax-1, 2:jmax-1, k)
          end do

!  Send shadow values to neighbors

          myx = iop(1)
          myy = iop(2)
          myz = iop(3)
          call sync_all()

          if (myz > 1)    p(2:imax-1, 2:jmax-1, kmax[myx,myy,myz-1])[myx,myy,myz-1] = p(2:imax-1, 2:jmax-1, 2     )
          if (myz < ndz)  p(2:imax-1, 2:jmax-1, 1                  )[myx,myy,myz+1] = p(2:imax-1, 2:jmax-1, kmax-1)

          if (myy > 1)    p(2:imax-1, jmax[myx,myy-1,myz], 2:kmax-1)[myx,myy-1,myz] = p(2:imax-1, 2,      2:kmax-1)
          if (myy < ndy)  p(2:imax-1, 1,                   2:kmax-1)[myx,myy+1,myz] = p(2:imax-1, jmax-1, 2:kmax-1)

          if (myx > 1)    p(imax[myx-1,myy,myz], 2:jmax-1, 2:kmax-1)[myx-1,myy,myz] = p(2,      2:jmax-1, 2:kmax-1)
          if (myx < ndx)  p(1,                   2:jmax-1, 2:kmax-1)[myx+1,myy,myz] = p(imax-1, 2:jmax-1, 2:kmax-1)

!  Sum results

          call co_sum(wgosa,gosa)

       enddo

     end subroutine jacobi
