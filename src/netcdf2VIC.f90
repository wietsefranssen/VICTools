! Author : Wietse Franssen (Wageningen University)
! E-mail : wietse.franssen@wur.nl
! Created: march 2012
! Purpose: Convert meteorological gridded NetCDF data into ASCII or Binary Format that can be used for de VIC-Model.
! Usage  : ./VICCONVERT netcdf2VIC <StartTimeStep> <EndTimeStep> <TimeStepPerPart> <BinaryOutput(0=no/1=yes)>
!                                  <Outpath+prefix> <NetCDFMaskFilename> <VarnameOfMask>
!                                  <NetCDFFilenamOfVar1> <VariableNameOfVar1> <MultiplicationFactOfVar1>
!                                  <NetCDFFilenamOfVar2> <VariableNameOfVar2> <MultiplicationFactOfVar2>
!                                  <NetCDFFilenamOfVarN> <VariableNameOfVarN> <MultiplicationFactOfVarN>
! Example: ./VICCONVERT netcdf2VIC 1 3000 1000 1 ./VICMet/fluxes_ ./cru.nc cell ./Precip.nc prec 100
!                                  ./Tmin.nc tmin 100 ./Tmax.nc tmax 100 ./Wind.nc wind 100
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
SUBROUTINE netcdf2vic(Set_N2V, cConfigFile, nVar)

    use netcdf
    use definitionsAndTypes

    implicit none

    TYPE(type_NETCDF2VIC) :: Set_N2V

    integer                 :: iLon,iLat,iTime,iVar,iCell
    integer                 :: nLon,nLat,nTime,nVar,nBand
    integer                 :: nLonTmp,nLatTmp,nTimeTmp
    integer                 :: xStart,xEnd,yStart,yEnd
    real                    :: noNadaValue
    character(len = 8)      :: cLon, cLat
    character(len = 256)    :: gridVICInFile, gridVICOutFile
    character(len = 256)    :: cFormatASCII
    character(len = 512)    :: cConfigFile
    character(len = 32)     :: cDummy1,cDummy2,cDummy3,cDummy4,cDummy5
    ! NetCDF:
    character(len = 256)    :: ncFileName,outFileName
    integer                 :: ncid
    integer                 :: dimidLon, dimidLat, dimidTime, dimidVar
    integer                 :: varidLon, varidLat, varidTime, varidVar, varidData
    integer                 :: iCel,ncells,ncEnd,iPercent
    real(kind = 4), allocatable     :: aLat(:),aLon(:)
    real(kind = 4), allocatable     :: dataArray(:,:,:)
    real(kind = 4), allocatable     :: ncData(:,:,:)
    real(kind = 4), allocatable     :: ncDataMask(:,:)
    real(kind = 4), allocatable     :: dataMask(:,:)
    integer(kind = 2), allocatable  :: ll2xy(:,:)
    integer(kind = 4), allocatable  :: parts(:)
    integer(kind = 2)               :: iData
    integer(kind = 4)               :: nTimeTotal, nTimePart, nParts, iPart
    integer(kind = 4), allocatable  ::ncStart(:,:)
    integer(kind = 4), allocatable  ::ncCount(:,:)
    integer                 :: startPos,startPosPart1

    !! Configs:
    Set_N2V%lonMin              = -179.75
    Set_N2V%lonMax              = 179.75
    Set_N2V%latMin              = -89.75
    Set_N2V%latMax              = 89.75
    Set_N2V%resolution          = 0.50
    noNadaValue                 = 1e+20 !-999000000.
    !! Configs END

    gridVICInFile       = trim(Set_N2V%gridVICInFile)
    nLon                = ((Set_N2V%lonMax - Set_N2V%lonMin) / Set_N2V%resolution) + 1
    nLat                = ((Set_N2V%latMax - Set_N2V%latMin) / Set_N2V%resolution) + 1

    !! Define Number of timesteps per part
    nTimeTotal          = Set_N2V%timeEnd - Set_N2V%timeStart + 1
    nParts = ceiling(real(nTimeTotal) / real(Set_N2V%numMaxTimeSteps))
    if(.not.allocated(parts))               allocate(parts           (nParts))
    if(.not.allocated(ncStart))             allocate(ncStart         (nParts,3))
    if(.not.allocated(ncCount))             allocate(ncCount         (nParts,3))
    ncStart(:,1:2)=1
    ncCount(:,1)=nLon
    ncCount(:,2)=nLat
    do iPart=1,nParts
        parts(iPart) = Set_N2V%numMaxTimeSteps
        ncStart(iPart,3) = 1 + (parts(iPart) * (iPart-1))
        ncCount(iPart,3) = parts(iPart)
    enddo
    parts(nParts) = nTimeTotal- (Set_N2V%numMaxTimeSteps * (nParts-1))
    ncCount(nParts,3) = parts(nParts)
    !! Define Number of timesteps per part END

    print *,'LonMin:                                 ', Set_N2V%lonMin
    print *,'LonMax:                                 ', Set_N2V%lonMax
    print *,'LatMin:                                 ', Set_N2V%latMin
    print *,'LatMax:                                 ', Set_N2V%latMax
    print *,'Resolution:                             ', Set_N2V%resolution
    print *,'Number of Cells in longitude direction: ', nLon
    print *,'Number of Cells in latitude direction:  ', nLat
    print *,'Number of timesteps:                    ', nTimeTotal
    print *,'Number(max) timesteps per part:         ', Set_N2V%numMaxTimeSteps
    print *,'Number of parts:                        ', nParts
    print *,''

    !!!!!!!!!!!!!!!!!!!!! READING GRIDFILE AND RE-ORDER GRID ARRAY!!!!!!!!!!!!!!!!!!
    !** READING GRIDFILE
    ncFileName = trim(Set_N2V%gridVICInFile)
    ! READ NETCDF
    print *,'Reading NetCDF grid file:      ', trim(ncFileName)
    call check( nf90_open(ncFileName, nf90_nowrite, ncid) )
    ! Read the dimensions. NetCDF will hand back an ID for each.
    call check( nf90_inq_dimid(ncid, "lon", dimidLon) )
    call check( nf90_inq_dimid(ncid, "lat", dimidLat) )
    ! Read the length of the dimensions.
    call check( nf90_inquire_dimension(ncid, dimidLon, len = nLonTmp) )
    call check( nf90_inquire_dimension(ncid, dimidLat, len = nLatTmp) )
    if(.not.allocated(aLon))               allocate(aLon           (nLonTmp))
    if(.not.allocated(aLat))               allocate(aLat           (nLatTmp))
    ! Inq the variable.
    call check( nf90_inq_varid(ncid, "lon", varidLon) )
    call check( nf90_inq_varid(ncid, "lat", varidLat) )
    call check( nf90_inq_varid(ncid, trim(Set_N2V%varNameMask), varidData) )
    ! Read data from the file.
    call check( nf90_get_var(ncid, varidLon, aLon ) )
    call check( nf90_get_var(ncid, varidLat, aLat ) )
    if(.not.allocated(ncDataMask))             allocate(ncDataMask         (nLonTmp,nLatTmp))
    call check( nf90_get_var(ncid, varidData, ncDataMask(:,:) ) )
    ! Close the file.
    call check( nf90_close(ncid) )
    !** Count number of cells in mask
    xStart =((Set_N2V%lonMin - minval(aLon(:))) / Set_N2V%resolution) + 1
    xEnd    = xStart + nLon -1
    yStart =((Set_N2V%latMin - minval(aLat(:))) / Set_N2V%resolution) + 1
    yEnd    = yStart + nLat -1
    iCel = 1
    do iLon=xStart,xEnd
        do iLat=yStart,yEnd
            if (ncDataMask(iLon,iLat) .ne. noNadaValue) then
                iCel = iCel + 1
            endif
        enddo
    enddo
    nCells=iCel-1
    !** re-order grid array
    if(.not.allocated(dataMask))             allocate(dataMask         (nCells,2))
    iCel = 1
    do iLon=xStart,xEnd
        do iLat=yStart,yEnd
            if (ncDataMask(iLon,iLat) .ne. noNadaValue) then
                dataMask(iCel,1) = aLon(iLon)
                dataMask(iCel,2) = aLat(iLat)
                iCel = iCel + 1
            endif
        enddo
    enddo
    !** free data
    if(allocated(aLon))                deallocate(aLon)
    if(allocated(aLat))                deallocate(aLat)
        !     if(allocated(ncDataMask))                deallocate(ncDataMask)
        !!!!!!!!!!!!!!!!!!!!! READING GRIDFILE AND RE-ORDER GRID ARRAY  END !!!!!!!!!!!!!!!!!!

    do iPart=1,nParts
        nTimePart= parts(iPart)
        ncEnd = ncStart(iPart,3) + ncCount(iPart,3) - 1
        Print *, 'Doing part:         ', iPart, ' of   ', nParts
        print *, 'TimeStep:           ', ncStart(iPart,3) , ' till ', ncEnd, ' of ', nTimeTotal
        !!!!!!!!!!!!!!!!!!!!! READING DATAFILE !!!!!!!!!!!!!!!!!!
        do iVar=1,nVar
            ncFileName = trim(Set_N2V%dataVICInFile(iVar))
            ! READ NETCDF
            print *,'Reading NetCDF data file:      ', trim(ncFileName)
            call check( nf90_open(ncFileName, nf90_nowrite, ncid) )
            ! Read the dimensions. NetCDF will hand back an ID for each.
            call check( nf90_inq_dimid(ncid, "lon", dimidLon) )
            call check( nf90_inq_dimid(ncid, "lat", dimidLat) )
            call check( nf90_inq_dimid(ncid, "time", dimidTime) )
            ! Read the length of the dimensions.
            call check( nf90_inquire_dimension(ncid, dimidLon, len = nLonTmp) )
            call check( nf90_inquire_dimension(ncid, dimidLat, len = nLatTmp) )
            call check( nf90_inquire_dimension(ncid, dimidTime, len = nTimeTmp) )
            if(.not.allocated(aLon))               allocate(aLon           (nLonTmp))
            if(.not.allocated(aLat))               allocate(aLat           (nLatTmp))
            ! Inq the variable.
            call check( nf90_inq_varid(ncid, "lon", varidLon) )
            call check( nf90_inq_varid(ncid, "lat", varidLat) )
            call check( nf90_inq_varid(ncid, trim(Set_N2V%varName(iVar)) , varidData) )
            ! Read data from the file.
            call check( nf90_get_var(ncid, varidLon, aLon ) )
            call check( nf90_get_var(ncid, varidLat, aLat ) )
            if(.not.allocated(ncData))             allocate(ncData         (nLonTmp,nLatTmp,nTimePart))
            call check( nf90_get_var(ncid, varidData, ncData(:,:,:) ,ncStart(iPart,:),ncCount(iPart,:) ) )
            ! Close the file.
            call check( nf90_close(ncid) )

            !** re-order data array
            if(.not.allocated(ll2xy))              allocate(ll2xy          (ncells,2))
            call calc_ll2xy(ll2xy,aLon,aLat,dataMask,ncells,nLonTmp,nLatTmp)

            if(.not.allocated(dataArray))             allocate(dataArray         (nCells,nVar,nTimePart))
            do iCel=1,nCells
                dataArray(iCel,iVar,:) = ncData(ll2xy(iCel,1),ll2xy(iCel,2),:)
            enddo
        enddo
        !!!!!!!!!!!!!!!!!!!!! READING DATAFILE END !!!!!!!!!!!!!!!!!!

        !!!!!!!!!!!!!!!!!!!!! CONVERSIONS !!!!!!!!!!!!!!!!!!
        !** Do this because ISIMIP VIC-INPUT is not ALMA:
        do iVar=1,nVar
            if (trim(Set_N2V%varName(iVar)) .eq. 'precip' .or. trim(Set_N2V%varName(iVar)) .eq. 'pr') then
                dataArray(:,iVar,:) = dataArray(:,iVar,:)*86400
            elseif (trim(Set_N2V%varName(iVar)) .eq. 'tmin' .or. &
                  trim(Set_N2V%varName(iVar)) .eq. 'tmax' .or. &
                  trim(Set_N2V%varName(iVar)) .eq. 'tasmin' .or. &
                  trim(Set_N2V%varName(iVar)) .eq. 'tasmax') then
                dataArray(:,iVar,:) = dataArray(:,iVar,:)-273.16
            elseif (trim(Set_N2V%varName(iVar)) .eq. 'ps') then
                dataArray(:,iVar,:) = dataArray(:,iVar,:)/1000
            endif
        enddo
        !!!!!!!!!!!!!!!!!!!!! CONVERSIONS END !!!!!!!!!!!!!!!!!!

        print *,'writing files...'
        if (Set_N2V%binary == 1)  then
            !!!!!!!!!!!!!!!!!!!!! CREATING BIN FILE !!!!!!!!!!!!!!!!!!
            !        do iCel=1,4
            do iCel=1,nCells
                write(cLon, "(F8.2)") dataMask(iCel,1)
                write(cLat, "(F8.2)") dataMask(iCel,2)
                outFileName = trim(Set_N2V%outPath)//trim(adjustl(cLat))//'_'//trim(adjustl(cLon))
                !  print *, 'cell: ', iCel, ' of ', ncells, 'lat: ' , trim(adjustl(cLat)), ' lon: ', trim(adjustl(cLon))
                if (mod(iCel,(nCells/20)).eq.0) then
                    iPercent = 100/real((real(nCells)/real(iCel)))
                    print *, iPercent ,'% Completed  (cell: ', iCel, ' of ', ncells, ')' ! , 'lat: ' , trim(adjustl(cLat)), ' lon: ', trim(adjustl(cLon))
                endif
!                open (unit = 30, file = outFileName, form='unformatted',ACCESS='STREAM')
                !open (unit = 30, file = outFileName, form='unformatted',ACCESS='STREAM',POSITION='APPEND')
                !** Skip till correct location (to change if other formats than Int are used!)
!startPos= ftell(30)
                if (iPart .gt. 1) then
                    open (unit = 30, file = outFileName, form='unformatted',ACCESS='STREAM',POSITION='APPEND')
                    !call ftell(30,startPos)
                    !startPos= startPos -1

                    !read (30,pos=startPos) iData
!                    startPos=-1
                    startPos=startPosPart1
                    do iVar=1,nVar
                            startPos=((ncStart(iPart,3)-1+Set_N2V%timeStart-1)*2) + startPos
                    enddo
                    read (30,pos=startPos) iData
                else
                    open (unit = 30, file = outFileName, form='unformatted',ACCESS='STREAM',POSITION='APPEND')
                    if (iCel .eq. 1) then
                        call ftell(30,startPosPart1)
                        startPosPart1 = startPosPart1 -1
                    endif
!print *,'hoi:  ', startPos
                endif

                do iTime=1,nTimePart
                    do iVar=1,nVar
                        iData = int(dataArray(iCel,iVar,iTime)*Set_N2V%varBinMultipl(iVar))
                        if (iData .lt. 0) then
                            if (trim(Set_N2V%varName(iVar)) .eq. 'tmin' .or. &
                              trim(Set_N2V%varName(iVar)) .eq. 'tmax' .or. &
                              trim(Set_N2V%varName(iVar)) .eq. 'tasmin' .or. &
                              trim(Set_N2V%varName(iVar)) .eq. 'tasmax') then
                               ! do nothing
                            else
                                iData = 0
                            endif
                        elseif (iData .gt. 65535) then
                            idata = 0
                        endif
                        write (30) iData
                    enddo
                enddo
                close(30)
            enddo
            !!!!!!!!!!!!!!!!!!!!! CREATING BIN FILE END !!!!!!!!!!!!!!!!!!
        else
            !!!!!!!!!!!!!!!!!!!!! CREATING ASCII FILE !!!!!!!!!!!!!!!!!!
            cFormatASCII = "f16.8"
            do iVar = 1,nVar-1
                cFormatASCII = trim(cFormatASCII)//",f16.10"
            enddo
            cFormatASCII = '('//trim(cFormatASCII)//')'
            do iCel=1,nCells
                write(cLon, "(F8.2)") dataMask(iCel,1)
                write(cLat, "(F8.2)") dataMask(iCel,2)
                outFileName = trim(Set_N2V%outPath)//trim(adjustl(cLat))//'_'//trim(adjustl(cLon))
                if (mod(iCel,(nCells/20)).eq.0) then
                    iPercent = 100/real((real(nCells)/real(iCel)))
                    print *, iPercent ,'% Completed  (cell: ', iCel, ' of ', ncells, ')' ! , 'lat: ' , trim(adjustl(cLat)), ' lon: ', trim(adjustl(cLon))
                endif
                open (unit = 31, file = outFileName,POSITION='APPEND')
                do iTime=1,nTimePart
                    write (31,cFormatASCII) ( dataArray(iCel,:,iTime))
                enddo
                close(31)
            enddo
            !!!!!!!!!!!!!!!!!!!!! CREATING ASCII FILE END !!!!!!!!!!!!!!!!!!
        endif
        if(allocated(ll2xy))                deallocate(ll2xy)
        if(allocated(dataArray))            deallocate(dataArray)
        if(allocated(aLon))                 deallocate(aLon)
        if(allocated(aLat))                 deallocate(aLat)
        if(allocated(ncData))               deallocate(ncData)
    enddo

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
SUBROUTINE calc_ll2xy(ll2xy, aLon,aLat,dataMask,ncells,nLonTmp,nLatTmp)
    integer     :: ncells, nLonTmp, nLatTm
    logical     :: xFlip, yFlip
    integer(kind = 2)   :: ll2xy(ncells,2)
    real(kind = 4)      :: aLat(nLonTmp), aLon(nLatTmp)
    real(kind = 4)      :: dataMask(ncells,2)
    ! First define if the Arrays in the data are flipped or not
    xFlip = .false.
    yFlip = .false.
    if (aLon(1) .gt. aLon(2)) xFlip = .true.
    if (aLat(1) .gt. aLat(2)) yFlip = .true.
    !print *,'x: ', xFlip
    !print *,'y: ', yFlip
    if (xFlip) then
        do iCel=1,ncells
            ll2xy(iCel,1) = (( aLon(1) - dataMask(iCel,1) ) * 2 ) + 1
            ll2xy(iCel,2) = (( dataMask(iCel,2) - aLat(1) ) * 2 ) + 1
          !  print *,ll2xy(iCel,:),dataMask(iCel,:)
        enddo
    elseif (yFlip) then
        do iCel=1,ncells
            ll2xy(iCel,1) = (( dataMask(iCel,1) - aLon(1) ) * 2 ) + 1
            ll2xy(iCel,2) = (( aLat(1) - dataMask(iCel,2) ) * 2 ) + 1
        enddo
    elseif (xFlip .and. yFlip) then
        do iCel=1,ncells
            ll2xy(iCel,1) = (( aLon(1) - dataMask(iCel,1) ) * 2 ) + 1
            ll2xy(iCel,2) = (( aLat(1) - dataMask(iCel,2) ) * 2 ) + 1
        enddo
    else
        do iCel=1,ncells
            ll2xy(iCel,1) = (( dataMask(iCel,1) - aLon(1)  ) * 2 ) + 1
            ll2xy(iCel,2) = (( dataMask(iCel,2) - aLat(1) ) * 2 ) + 1
        enddo
    endif
end
!!!!!!!!!!!!!!! SOME FUNCTIONS END !!!!!!!!!!!!!!!!!!


