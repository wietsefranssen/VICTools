! Author : Wietse Franssen (Wageningen University)
! E-mail : wietse.franssen@wur.nl
! Created: march 2012
! Purpose: Convert output of the VIC-model (ASCII or Binary Format) to gridded NetCDF data.
! Usage  : ./VICCONVERT NETCDF2VIC <StartTimeStep> <EndTimeStep> <TimeStepPerPart> <BinaryInput(0=no/1=yes)>
!                                  <VICDataPath+prefix> <PrefixOfNetCDFFilename>
!                                  <VariableNameOfVar1> <MultiplicationFactOfVar1>
!                                  <VariableNameOfVar2> <MultiplicationFactOfVar2>
!                                  <VariableNameOfVarN> <MultiplicationFactOfVarN>
! Example: ./VICCONVERT NETCDF2VIC 1 3000 1000 0 ./VICMet/fluxes_ ./netcdf/ncData_ prec 100
!                                  tmin 100 tmax 100 wind 100
! Notes  : --> If in doubt, read the disclaimer.
!          --> In case you will get a memory issue: lower the <TimeStepPerPart> value.
!              Lowering this number will slow down the conversion but on the other hand it uses less memory.
!          --> If using ASCII output. <VariableNameOfVarN> and <MultiplicationFactOfVarN>
!              must still contain a number in although it will not be used during conversion.
! Disclaimer: Feel free to use or adapt any part of this program for your own
!             convenience.  However, this only applies with the understanding
!             that YOU ARE RESPONSIBLE TO ENSURE THAT THE PROGRAM DOES WHAT YOU
!             THINK IT SHOULD DO.  The author of this program does not in any
!             way, shape or form accept responsibility for problems caused by
!             the use of this code.  At any time, please feel free to discard
!             this code and WRITE YOUR OWN, it's what I would do.
!
SUBROUTINE vic2netcdf(Set_V2N, cConfigFile, nVar)

    use netcdf
    use definitionsAndTypes

    implicit none

    TYPE(type_VIC2NETCDF) :: Set_V2N

    integer                 :: iLon,iLat,iTime,iVar,iCell,iSkip
    integer                 :: nLon,nLat,nTime,nVar,nBand
    integer                 :: nLonTmp,nLatTmp,nTimeTmp
    integer                 :: xStart,xEnd,yStart,yEnd
    integer                 :: startPos
    real                    :: noNadaValue
    real                    :: rLat,rLon
    character(len = 8)      :: cLon, cLat
    character(len = 256)    :: gridVICInFile, gridVICOutFile
    character(len = 256)    :: cFormatASCII
    character(len = 512)    :: cConfigFile
    character(len = 10)      :: cYear
    character(len = 32)     :: cDummy1,cDummy2,cDummy3,cDummy4,cDummy5
    ! NetCDF:
    character(len = 256)    :: ncFileName,outFileName
    integer                 :: ncid
    integer                 :: dimidLon, dimidLat, dimidTime, dimidVar
    integer                 :: varidLon, varidLat, varidTime, varidVar, varidData
    integer                 :: iCel,ncells,ncEnd,iPercent
    real(kind = 4), allocatable     :: aLatCells(:),aLonCells(:)
    real(kind = 4), allocatable     :: aLat(:),aLon(:),aTime(:)
    real(kind = 4), allocatable     :: dataArray(:,:,:)
    real(kind = 4), allocatable     :: ncData(:,:,:)
    real(kind = 4), allocatable     :: ncDataMask(:,:)
    real(kind = 4), allocatable     :: dataMask(:,:)
    integer(kind = 4), allocatable  :: parts(:)
    integer(kind = 2)               :: iData
    real(kind = 4)                  :: fData
    integer(kind = 4)               :: nTimeTotal, nTimePart, nParts, iPart
    integer(kind = 4), allocatable  :: ncStart(:,:)
    integer(kind = 4), allocatable  :: ncCount(:,:)
    real(kind = 4), allocatable     :: dataDummy(:)
    LOGICAL :: file_exists
    LOGICAL  :: LEAP

    !! Configs:
    Set_V2N%lonMin              = -179.75
    Set_V2N%lonMax              = 179.75
    Set_V2N%latMin              = -89.75
    Set_V2N%latMax              = 89.75
    Set_V2N%resolution          = 0.50
    noNadaValue         = 1e+20 !-999000000.
    !! Configs END
    if(.not.allocated(dataDummy))             allocate(dataDummy         (nvar))

    nLon                = ((Set_V2N%lonMax - Set_V2N%lonMin) / Set_V2N%resolution) + 1
    nLat                = ((Set_V2N%latMax - Set_V2N%latMin) / Set_V2N%resolution) + 1
    ! write(cYear,(I)) Set_V2N%yearStart
    !  read (cYear,*) Set_V2N%yearStart
    !! Define Number of timesteps per part
    nTimeTotal          = Set_V2N%timeEnd - Set_V2N%timeStart + 1
    nParts = ceiling(real(nTimeTotal) / real(Set_V2N%numMaxTimeSteps))
    if(.not.allocated(parts))               allocate(parts           (nParts))
    if(.not.allocated(ncStart))             allocate(ncStart         (nParts,3))
    if(.not.allocated(ncCount))             allocate(ncCount         (nParts,3))
    ncStart(:,1:2)=1
    ncCount(:,1)=nLon
    ncCount(:,2)=nLat
    do iPart=1,nParts
        parts(iPart) = Set_V2N%numMaxTimeSteps
        ncStart(iPart,3) = 1 + (parts(iPart) * (iPart-1))
        ncCount(iPart,3) = parts(iPart)
    enddo
    parts(nParts) = nTimeTotal- (Set_V2N%numMaxTimeSteps * (nParts-1))
    ncCount(nParts,3) = parts(nParts)
    !! Define Number of timesteps per part END
    !print *, 'end:',Set_V2N%timeEnd , 'start',  Set_V2N%timeStart
    print *,'LonMin:                                 ', Set_V2N%lonMin
    print *,'LonMax:                                 ', Set_V2N%lonMax
    print *,'LatMin:                                 ', Set_V2N%latMin
    print *,'LatMax:                                 ', Set_V2N%latMax
    print *,'Resolution:                             ', Set_V2N%resolution
    print *,'Number of Cells in longitude direction: ', nLon
    print *,'Number of Cells in latitude direction:  ', nLat
    print *,'Number of timesteps:                    ', nTimeTotal
    print *,'Number(max) timesteps per part:         ', Set_V2N%numMaxTimeSteps
    print *,'Number of parts:                        ', nParts
        !print *,''
    !print *,'1960',LEAP(1960)
    !print *,'1961',LEAP(1961)
    !print *,'1962',LEAP(1962)


    !!!!!!!!!!!!!!!!!!!!! READING GRIDFILE AND RE-ORDER GRID ARRAY!!!!!!!!!!!!!!!!!!
    !** Count number of cells in mask
    xStart = 1
    xEnd   = nLon
    yStart = 1
    yEnd   = nLat

    !** Count number of cells (number of VIC files)
    print *,'Counting cells...'
    open (unit = 11, file = './latlon.tmp')
    iCel = 1
    do iLon=xStart,xEnd
      !   print *,iLon
         do iLat=yStart,yEnd
            rLon = Set_V2N%lonMin + ((iLon-1)*Set_V2N%resolution)
            rLat = Set_V2N%latMin + ((iLat-1)*Set_V2N%resolution)
            write(cLon, "(F8.2)") rLon
            write(cLat, "(F8.2)") rLat
            if (Set_V2N%binary == 3)  then ! if routing output
                outFileName = trim(Set_V2N%outPath)//trim(adjustl(cLat))//'_'//trim(adjustl(cLon))//'.day'
            else
                outFileName = trim(Set_V2N%outPath)//trim(adjustl(cLat))//'_'//trim(adjustl(cLon))
            endif
!            outFileName = trim(Set_V2N%outPath)//trim(adjustl(cLat))//'_'//trim(adjustl(cLon))
            INQUIRE(FILE=trim(adjustl(outFileName)), EXIST=file_exists)   ! file_exists will be TRUE if the file
            if(file_exists) then
                write (11,*) rLat,rLon
                iCel = iCel + 1
            endif
        enddo
    enddo
    close(11)
    nCells=iCel-1
    print *,'Number of cells:                        ', nCells
    print *,''


    !** Fill Lat and Lon arrays
    if(.not.allocated(aLon))               allocate(aLon           (nLon))
    if(.not.allocated(aLat))               allocate(aLat           (nLat))
    if(.not.allocated(aTime))              allocate(aTime          (nTimeTotal))
    do iLon=xStart,xEnd
        aLon(iLon) = Set_V2N%lonMin + ((iLon -1) * Set_V2N%resolution)
    enddo
    do iLat=yStart,yEnd
        !        aLat(iLat) = Set_V2N%latMin + ((iLat -1) * Set_V2N%resolution)
        aLat(iLat) = Set_V2N%latMax - ((iLat -1) * Set_V2N%resolution)
    enddo
    do iTime=1,nTimeTotal
        aTime(iTime) = iTime -1
    enddo

    !** create array with latlon values
    open (unit = 12, file = './latlon.tmp', STATUS='OLD')
    if(.not.allocated(aLatCells))             allocate(aLatCells         (nCells))
    if(.not.allocated(aLonCells))             allocate(aLonCells         (nCells))
    do iCel=1, nCells
        read(12,*) aLatCells(iCel),aLonCells(iCel)
    enddo
    close(12)

    do iPart=1,nParts
        nTimePart= parts(iPart)
        ncEnd = ncStart(iPart,3) + ncCount(iPart,3) - 1
        Print *, 'Doing part:         ', iPart, ' of   ', nParts
        print *, 'TimeStep:           ', ncStart(iPart,3) , ' till ', ncEnd, ' of ', nTimeTotal
        !** read the data into array
        print *,'reading VIC-files...'
        if(.not.allocated(dataArray))             allocate(dataArray         (nCells,nVar,nTimePart))
        if (Set_V2N%binary == 1)  then
            !!!!!!!!!!!!!!!!!!!!! READING BIN FILE !!!!!!!!!!!!!!!!!!
            do iCel=1,nCells
                write(cLon, "(F8.2)") aLonCells(iCel)
                write(cLat, "(F8.2)") aLatCells(iCel)
                outFileName = trim(Set_V2N%outPath)//trim(adjustl(cLat))//'_'//trim(adjustl(cLon))
                !  print *, 'cell: ', iCel, ' of ', ncells, 'lat: ' , trim(adjustl(cLat)), ' lon: ', trim(adjustl(cLon))
                if (mod(iCel,(nCells/20)).eq.0) then
                    iPercent = 100/real((real(nCells)/real(iCel)))
                    print *, iPercent ,'% Completed  (cell: ', iCel, ' of ', ncells, ')' ! , 'lat: ' , trim(adjustl(cLat)), ' lon: ', trim(adjustl(cLon))
                endif
                open (unit = 14, file = outFileName, form='unformatted',ACCESS='STREAM')
                !** Skip till correct location (to change if other formats than Int are used!)
                if (Set_V2N%timeStart .gt. 1 .or. iPart .gt. 1) then
                    startPos=-1
                    do iVar=1,nVar
                        if (trim(Set_V2N%varBinType(iVar)) .eq. 'f') then
                            startPos=((ncStart(iPart,3)-1+Set_V2N%timeStart-1)*4) + startPos
                        elseif (trim(Set_V2N%varBinType(iVar)) .eq. 'i') then
                            startPos=((ncStart(iPart,3)-1+Set_V2N%timeStart-1)*2) + startPos
                            !print *, 'Start', startPos, Set_V2N%timeStart, ncStart(iPart,3),Set_V2N%timeStart+(ncStart(iPart,3)-1) &
                            !,nTimePart
                        endif
                    enddo
                    read (14,pos=startPos) iData
                endif

                do iTime=1,nTimePart
                    do iVar=1,nVar
                        if (trim(Set_V2N%varBinType(iVar)) .eq. 'f') then
                            read (14) fData
                            dataArray(iCel,iVar,iTime) = fData/Set_V2N%varBinMultipl(iVar)
                        elseif (trim(Set_V2N%varBinType(iVar)) .eq. 'i') then
                            read (14) iData
                            dataArray(iCel,iVar,iTime) = real(iData)/Set_V2N%varBinMultipl(iVar)
                        endif
                    enddo
                enddo
                close(14)
            enddo
            !!!!!!!!!!!!!!!!!!!!! READING BIN FILE END !!!!!!!!!!!!!!!!!!
        else
            !!!!!!!!!!!!!!!!!!!!! READING ASCII FILE !!!!!!!!!!!!!!!!!!
            do iCel=1,nCells
                write(cLon, "(F8.2)") aLonCells(iCel)
                write(cLat, "(F8.2)") aLatCells(iCel)
                if (Set_V2N%binary == 3)  then ! if routing output
                    outFileName = trim(Set_V2N%outPath)//trim(adjustl(cLat))//'_'//trim(adjustl(cLon))//'.day'
                else
                    outFileName = trim(Set_V2N%outPath)//trim(adjustl(cLat))//'_'//trim(adjustl(cLon))
                endif
                if (mod(iCel,(nCells/20)).eq.0) then
                    iPercent = 100/real((real(nCells)/real(iCel)))
                    print *, iPercent ,'% Completed  (cell: ', iCel, ' of ', ncells, ')' ! , 'lat: ' , trim(adjustl(cLat)), ' lon: ', trim(adjustl(cLon))
                endif
                open (unit = 13, file = outFileName)

                if (Set_V2N%timeStart .gt. 1 .or. iPart .gt. 1) then
                    do iSkip=1,(ncStart(iPart,3)-1+Set_V2N%timeStart-1)
                        read (13,*) dataDummy(:)
                    enddo
                endif
                do iTime=1,nTimePart
                    read (13,*) dataArray(iCel,:,iTime)
                enddo
                close(13)
            enddo
        !!!!!!!!!!!!!!!!!!!!! READING ASCII FILE END !!!!!!!!!!!!!!!!!!
        endif

        do iVar=1,nVar
            !!!!!!!!!!!!!!!!!!!!! RE-ORDER ARRAY !!!!!!!!!!!!!!!!!!
            print *,'re-order array...'
            if(.not.allocated(ncData))              allocate(ncData          (nLon,nLat,nTimePart))
            ncData          (:,:,:) = noNadaValue

            do iCel=1,nCells
                iLat = nLat - ((aLatCells(iCel) - Set_V2N%latMin) / Set_V2N%resolution)
                iLon = ((aLonCells(iCel) - Set_V2N%lonMin) / Set_V2N%resolution)+1
                ncData          (iLon,iLat,:) = dataArray(iCel,iVar,:)
            enddo
            !!!!!!!!!!!!!!!!!!!!! RE-ORDER ARRAY END !!!!!!!!!!!!!!!!!!

            !!!!!!!!!!!!!!!!!!!!! WRITING DATAFILE !!!!!!!!!!!!!!!!!!
            if (Set_V2N%monthly == 1) then
              ncFileName = trim(Set_V2N%NetCDFPrefix)//trim(Set_V2N%varName(iVar)) &
              //'_monthly_'//Set_V2N%yearStart//'.nc'
            else
              ncFileName = trim(Set_V2N%NetCDFPrefix)//trim(Set_V2N%varName(iVar)) &
              //'_daily_'//Set_V2N%yearStart//'.nc'
            endif
            if (iPart .eq. 1) then
                ! CREATE NETCDF-FILE
                print *,'Creating NetCDF data file:      ', trim(ncFileName)
                call check( nf90_create(ncFileName, nf90_classic_model, ncid) )
!                call check( nf90_create(ncFileName, nf90_netcdf4, ncid) )
                ! Define the dimensions. NetCDF will hand back an ID for each.
                call check( nf90_def_dim(ncid, "lon", nLon, dimidLon) )
                call check( nf90_def_dim(ncid, "lat", nLat, dimidLat) )
                call check( nf90_def_dim(ncid, "time", nf90_unlimited, dimidTime) )
                ! Define the variables. NetCDF will hand back an ID for each.
                call check( nf90_def_var(ncid, "lon", nf90_float, dimidLon, varidLon) )
                call check( nf90_def_var(ncid, "lat", nf90_float, dimidLat, varidLat) )
                call check( nf90_def_var(ncid, "time", nf90_float, dimidTime, varidTime) )
                call check( nf90_def_var(ncid, trim(Set_V2N%varName(iVar)), nf90_float, &
                (/ dimidLon, dimidLat, dimidTime /), varidData) )
                ! Add attributes to the variables.
                call check( nf90_put_att(ncid, varidLon, "long_name", "longitude") )
                call check( nf90_put_att(ncid, varidLon, "units", "degrees_east") )
                call check( nf90_put_att(ncid, varidLon, "standard_name", "longitude") )
                call check( nf90_put_att(ncid, varidLat, "long_name", "latitude") )
                call check( nf90_put_att(ncid, varidLat, "units", "degrees_north") )
                call check( nf90_put_att(ncid, varidLat, "standard_name", "latitude") )
                if (Set_V2N%monthly == 1) then
                  call check( nf90_put_att(ncid, varidTime,"units", "months since "//Set_V2N%yearStart//"-01-01") )
                else
                  call check( nf90_put_att(ncid, varidTime,"units", "days since "//Set_V2N%yearStart//"-01-01") )
                endif
                call check( nf90_put_att(ncid, varidTime,"calendar", "standard") )
                call check( nf90_put_att(ncid, varidData, "short_field_name", trim(Set_V2N%varName(iVar))) )
                call check( nf90_put_att(ncid, varidData, "long_field_name", trim(Set_V2N%varNameLong(iVar))) )
                call check( nf90_put_att(ncid, varidData, "units", trim(Set_V2N%varUnit(iVar))) )
                call check( nf90_put_att(ncid, varidData, "_FillValue", noNadaValue) )
                call check( nf90_put_att(ncid, nf90_global, "title", "Impact model output for ISI-MIP") ) ! TODO
                !"impact model output for VIC4.1.2 bias corrected climate 0.5 degree grid") ) ! TODO
                call check( nf90_put_att(ncid, nf90_global, "comment1", &
                trim(Set_V2N%varComment1(iVar))) )
                call check( nf90_put_att(ncid, nf90_global, "comment2", &
                trim(Set_V2N%varComment2(iVar))) )
                call check( nf90_put_att(ncid, nf90_global, "institution", "WUR") ) ! TODO
                call check( nf90_put_att(ncid, nf90_global, "contact", "wietse.franssen@wur.nl, iha@nve.no") ) ! TODO
                !** End defititions
                call check( nf90_enddef(ncid) )
                !** Fill the variables lon and lat and time
                call check( nf90_put_var(ncid, varidLon ,aLon) )
                call check( nf90_put_var(ncid, varidLat ,aLat) )
                call check( nf90_put_var(ncid, varidTime ,aTime, &
                start = (/ 1 /), &
                count = (/ nTimeTotal /) ) )
                !** Close the netcdf file
                call check( nf90_close(ncid) )
            endif ! if iPart == 1
            ! OPEN NETCDF-FILE
            print *,'Open NetCDF data file:          ', trim(ncFileName)
            call check( nf90_open(ncFileName, nf90_Write, ncid) )
            !** Fill the data variable
            call check( nf90_put_var(ncid, varidData, ncData(:,:,:) ,ncStart(iPart,:),ncCount(iPart,:) ) )
            !** Close the netcdf file
            call check( nf90_close(ncid) )
            !!!!!!!!!!!!!!!!!!!!! WRITING DATAFILE END !!!!!!!!!!!!!!!!!!

            if(allocated(ncData))                deallocate(ncData)
        enddo !enddo: iVar=1,nVar
        if(allocated(dataArray))             deallocate(dataArray)
    enddo ! enddo of parts
    print *, 'END'
!!!! NETCDF CHECK ROUTINE  !!!!
contains
    subroutine check(status)
        integer, intent ( in) :: status

        if(status /= nf90_noerr) then
            print *, trim(nf90_strerror(status))
            stop "Stopped"
        end if
    end subroutine check
end
!!!! NETCDF CHECK ROUTINE END !!!!

!!!!!!!!!!!!!!! SOME FUNCTIONS !!!!!!!!!!!!!!!!!!
FUNCTION LEAP (YEAR) RESULT (LEAPFLAG)

    IMPLICIT NONE

    INTEGER :: YEAR
    LOGICAL :: LEAPFLAG

    LEAPFLAG = .FALSE.
    IF (MOD(YEAR,4) .EQ. 0)   LEAPFLAG = .TRUE.
    IF (MOD(YEAR,100) .EQ. 0) LEAPFLAG = .FALSE.
    IF (MOD(YEAR,400) .EQ. 0) LEAPFLAG = .TRUE.
    RETURN
END
!!!!!!!!!!!!!!! SOME FUNCTIONS END !!!!!!!!!!!!!!!!!!
