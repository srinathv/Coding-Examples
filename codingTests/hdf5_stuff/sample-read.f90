! *******************************************************************
!     sample-read.f90
!
!     sample program for reading HDF-5 data
! *******************************************************************

      program sample_read
!
      USE ReadH5dataset  !  contains the generic HDF5 reading routines
!
      implicit none
!
! -----------------
!
!  Input/output control variables
!
      integer                                 :: na,iargc
      character(len=256)                      :: filename
!
!  Data storage
!
      integer                                 :: npixels
      real,dimension(:),pointer               :: valmin
      character(len=40)                       :: sensingstart
      integer,dimension(:,:),pointer          :: nfititer
      character(len=30),dimension(:),pointer  :: viewmode
      integer,dimension(:),pointer            :: pxlday
      integer,dimension(:),pointer            :: pxltime
      integer                                 :: pxltimeFill
      real                                    :: satposX
!
! -----------------
!
!  Command line
!  ------------
!
      na=iargc()
      if (na.ne.1) then
         write(6,10)
         goto 999
      endif
!
  10  format(/," *** usage: sample-read  hdf-file",/)
!
      CALL getarg(1,filename)
!
!
!  Read elements of the HDF5 file
!  ==============================
!
!  Scalar integer attribute (works with either call)
!
!      CALL H5ReadDataset (filename, &
      CALL H5ReadAttribute (filename, &
           "/META_DATA/NumberOfGroundPixels", npixels)
      IF (ErrorFlag.lt.0) goto 990
      write(6,*)'npixels =',npixels
!
!  1-D real attribute of a dataset (works with either call)
!
!      CALL H5ReadAttribute(filename, &
      CALL H5ReadDataset(filename, &
           "/GEOLOCATION/LongitudeCentre/ValueRangeMin", valmin)
      IF (ErrorFlag.lt.0) goto 990
      write(6,*)'ValueRangeMin=',valmin
!
!  Scaler string attribute (call not in interface yet)
!
!      CALL H5ReadDatawet(filename, &
      CALL H5ReadAttribute(filename, &
           "/META_DATA/SensingStartTime", sensingstart)
      IF (ErrorFlag.lt.0) goto 990
      write(6,*)'SensingStartTime=',sensingstart(1:LengString(sensingstart))
!
!  2-dim integer dataset
!
      CALL H5ReadDataset(filename, &
           "/DETAILED_RESULTS/FittingNumberOfIterations", nfititer)
      IF (ErrorFlag.lt.0) goto 990
      write(6,*)'FittingNumberOfIterations(1,7),(2,8)=', &
                 nfititer(1,7),nfititer(2,8)
!
!  1-dim string dataset
!
      CALL H5ReadDataset(filename, &
            "/GEOLOCATION/ViewMode", viewmode)
      IF (ErrorFlag.lt.0) goto 990
      write(6,*)'ViewMode(1)=',viewmode(1)(1:LengString(viewmode(1)))
!
!  Compound dataset.
!  The ground pixel measurement time is a compound dataset
!  with two integer 1-D datasets in it -- read separately.
!
      CALL H5ReadDataset(filename, &
            "/GEOLOCATION/Time", "Day", pxlday )
      IF (ErrorFlag.lt.0) goto 990
      write(6,*)'Time - Day (7):',pxlday(7)
!
      CALL H5ReadDataset(filename, &  
           "/GEOLOCATION/Time", "MillisecondOfDay", pxltime )
      IF (ErrorFlag.lt.0) goto 990
      write(6,*)'Time - MillisecondOfDay (7):',pxltime(7)
!
!  Compound attributes: read one element.
!
      CALL H5ReadAttribute(filename, &  
           "/GEOLOCATION/Time/FillValue", "MillisecondOfDay", pxltimeFill )
      IF (ErrorFlag.lt.0) goto 990
      write(6,*)'Time FillValue - MillisecondOfDay:',pxltimeFill
!
      CALL H5ReadAttribute(filename, &  
           "/META_DATA/SatellitePosition", "X", satposX )
      IF (ErrorFlag.lt.0) goto 990
      write(6,*)'SatellitePosition - X:',satposX
!
!
!  Normal end of program.
!
      goto 999
!
! -----------------
!
!  Error handling.
!
 990  write(6,'(a)')ErrorMessage
      goto 999
!
! -----------------
!
 999  continue
!
      end program sample_read
!
! *******************************************************************
