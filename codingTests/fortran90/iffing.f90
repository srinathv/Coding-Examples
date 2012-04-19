

  PROGRAM iffing

  INTEGER:: iomethod

  write(*,*) "using the ifelselogic"
  write(*,*) "first test iommethod = 1 "
  iomethod =1 
  call iflogic(iomethod)
  write(*,*) "first test iommethod = 2 "
  iomethod =2 
  call iflogic(iomethod)
  write(*,*) "first test iommethod = 3 "
  iomethod =3 
  call iflogic(iomethod)

 write(*,*) "using the whatwewant"
  write(*,*) "first test iommethod = 1 "
  iomethod =1 
  call whatwewant(iomethod)
  write(*,*) "first test iommethod = 2 "
  iomethod =2 
  call whatwewant(iomethod)
  write(*,*) "first test iommethod = 3 "
  iomethod =3 
  call whatwewant(iomethod)

 


CONTAINS

  subroutine iflogic(iomethod)
  INTEGER, intent(in) :: iomethod

  if(iomethod <3) then
    write(*,*) "ascii write"
  elseif(iomethod > 1) then
    write(*,*) "hdf5 write"
  endif
  end subroutine

  subroutine whatwewant(iomethod)
  INTEGER, intent(in) :: iomethod

  if(iomethod <3) then
    write(*,*) "ascii write"
  endif
  if(iomethod > 1) then
    write(*,*) "hdf5 write"
  endif

  end subroutine





  END PROGRAM iffing
