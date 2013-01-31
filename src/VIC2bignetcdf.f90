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
SUBROUTINE vic2bignetcdf(Set_V2BIGN, cConfigFile)

  use netcdf
  use definitionsAndTypes

  implicit none

  CHARACTER(LEN = 512)         :: cDummy 
  REAL                         :: rDummy1,rDummy2 
  INTEGER                      :: iCell, iVar, iTime, iLat, iLon, iPart
  INTEGER                      :: nCell, nVar, nTime, nLat, nLon, nPart
  INTEGER                      :: iMonthly,iPercent
  INTEGER                      :: xStart,xEnd,yStart,yEnd
  INTEGER                      :: ncid,i,iLatTmp
  INTEGER                      :: dimidLon, dimidLat, dimidTime, dimidVar
  INTEGER                      :: varidLon, varidLat, varidTime, varidVar, varidData
  REAL                         :: noNadaValue
  TYPE(type_VIC2BIGNETCDF)     :: Set_V2BIGN
  CHARACTER(LEN = 512)         :: cConfigFile
  CHARACTER(LEN = 512)         :: ncFileName,fileName
  CHARACTER(LEN = 512)         :: cInPath,cOutPath,cPrefixOut,cYearStart
  INTEGER                      :: maxCells
  REAL ::MaxArraySize
  CHARACTER(LEN = 256), ALLOCATABLE     :: aInFile(:) 
  INTEGER(KIND = 4), ALLOCATABLE  :: parts(:)
  INTEGER(KIND = 4), ALLOCATABLE  :: ncStart(:,:)
  INTEGER(KIND = 4), ALLOCATABLE  :: ncCount(:,:)
  !REAL                         :: rLat, rLon
  REAL(KIND = 4), ALLOCATABLE  :: aLat(:),aLon(:),aTime(:)
  REAL(KIND = 4), ALLOCATABLE  :: ncData(:,:,:,:)
  CHARACTER(LEN = 5)         :: cVar

  REAL(KIND = 4), ALLOCATABLE  :: aLat2(:,:),aLon2(:,:)
  CHARACTER(LEN = 80), ALLOCATABLE     :: aInFile2(:,:) 

  MaxArraySize = 2000 ! Size in megaBytes
  Set_V2BIGN%lonMin              = -179.75
  Set_V2BIGN%lonMax              = 179.75
  Set_V2BIGN%latMin              = -89.75
  Set_V2BIGN%latMax              = 89.75
  Set_V2BIGN%resolution          = 0.50
  noNadaValue         = 1e+20
  iPart=1
  iVar=1
  nLon                = ((Set_V2BIGN%lonMax - Set_V2BIGN%lonMin) / Set_V2BIGN%resolution) + 1
  nLat                = ((Set_V2BIGN%latMax - Set_V2BIGN%latMin) / Set_V2BIGN%resolution) + 1

  xStart = 1
  xEnd   = nLon
  yStart = 1
  yEnd   = nLat

  !** readin config file
  OPEN (UNIT = 11, FILE = TRIM(cConfigFile))

  READ (11,'(a)') cInPath
  READ (11,'(a)') cOutPath
  READ (11,*) cDummy, cPrefixOut
  READ (11,*) cDummy, nCell
  READ (11,*) cDummy, nVar
  READ (11,*) cDummy, cYearStart
  READ (11,*) cDummy, nTime
  READ (11,*) cDummy, iMonthly

  IF(.NOT.ALLOCATED(aInFile2))            ALLOCATE(aInFile2        (nLat,nLon))
  IF(.NOT.ALLOCATED(aLon2))               ALLOCATE(aLon2           (nLat,nLon))
  IF(.NOT.ALLOCATED(aLat2))               ALLOCATE(aLat2           (nLat,nLon))

  IF(.NOT.ALLOCATED(aInFile))            ALLOCATE(aInFile        (nCell))
  IF(.NOT.ALLOCATED(aLon))               ALLOCATE(aLon           (nLon))
  IF(.NOT.ALLOCATED(aLat))               ALLOCATE(aLat           (nLat))
  IF(.NOT.ALLOCATED(aTime))              ALLOCATE(aTime          (nTime))

  do iLon=xStart,xEnd
     aLon(iLon) = Set_V2BIGN%lonMin + ((iLon -1) * Set_V2BIGN%resolution)
  enddo
  do iLat=yStart,yEnd
     !        aLat(iLat) = Set_V2N%latMin + ((iLat -1) * Set_V2N%resolution)
     aLat(iLat) = Set_V2BIGN%latMax - ((iLat -1) * Set_V2BIGN%resolution)
  enddo
  do iTime=1,nTime
     aTime(iTime) = iTime-1
  enddo

  aInFile2(:,:) = "nofile"
  DO iCell = 1, nCell
     !READ (11,*) aInfile(iCell), aLat(iCell), aLon(iCell)    
     READ (11,*) cDummy, rDummy1, rDummy2
     !PRINT *,rDummy1, ((rDummy1 - Set_V2BIGN%latMin) / Set_V2BIGN%resolution) +1
     iLat = ((rDummy1 - Set_V2BIGN%latMin) / Set_V2BIGN%resolution) +1
     iLon = ((rDummy2 - Set_V2BIGN%lonMin) / Set_V2BIGN%resolution) +1
     aInFile2(iLat,iLon) = cDummy
     ! Print *, trim(aInFile2(iLat,iLon)), iLat, iLon
  ENDDO

  !!   do iLat=yStart,parts(iPart)
  !!      do iLon=xStart,xEnd
  !        ncData        (iLon,iLat,iTime) = i
  !         PRINT *,aLon(iLon), aLat(iLat)
  !        READ (11,*) aInfile2(iLat,iLon), aLat2(iLat,iLon), aLon2(iLat,iLon)   
  !!      enddo  
  !!    enddo

  maxCells = (MaxArraySize * 1024 * 1024) / (4 * nLon * nTime * nVar)
  nPart = ceiling(real(nLat) / real(maxCells))
  if(.not.allocated(parts))               allocate(parts           (nPart))
  if(.not.allocated(ncStart))             allocate(ncStart         (nPart,3))
  if(.not.allocated(ncCount))             allocate(ncCount         (nPart,3))
  ncStart(:,1)=1
  ncStart(:,3)=1
  ncCount(:,1)=nLon
  ncCount(:,3)=nTime
  do iPart=1,nPart
     parts(iPart) = maxCells
     ncStart(iPart,2) = 1 + (parts(iPart) * (iPart-1)) 
     ncCount(iPart,2) = parts(iPart)
  enddo
  parts(nPart) = nLat - (maxCells * (nPart-1))
  ncCount(nPart,2) = parts(nPart)

  PRINT *, "Max array size(MB): ", MaxArraySize
  PRINT *, "Max latitude lines: ", maxCells
  PRINT *, "Number of Parts:    ", nPart
  !PRINT *, "Part:    ", parts
  PRINT *, "cInPath: ", trim(cInPath)
  PRINT *, "cOutPath:", trim(cOutPath)
  PRINT *, "nLat:    ", nLat
  PRINT *, "nLon:    ", nLon
  PRINT *, "nVar:    ", nvar
  PRINT *, "nCell:   ", nCell
  PRINT *, "nTime:   ", nTime
  !PRINT *, TRIM(aInFile(5)), aLat(5), aLon(5)

  iTime=1
  i=1    
  iCell = 1
  do iPart=1,nPart

     print*,'Part: ', iPart

     IF(.NOT.ALLOCATED(ncData))             ALLOCATE(ncData        (nLon,parts(iPart),nTime,nVar)) 
     ncData(:,:,:,:) = noNadaValue
     !    do iLat=yStart,yEnd
    ! print *, 'begin, eind', ncStart(iPart,2),((ncStart(iPart,2)+parts(iPart)))-1
     print *, 'begin, eind', (nLat-ncStart(iPart,2))+1,nLat-((ncStart(iPart,2)+parts(iPart)))+2
     iLatTmp = 1

     ! do iLat=ncStart(iPart,2),(((ncStart(iPart,2)+parts(iPart)))-1)
     do iLat=(nLat-ncStart(iPart,2))+1,nLat-((ncStart(iPart,2)+parts(iPart)))+2 , -1
        do iLon=xStart,xEnd
             if (mod(iCell,(nCell/20)).eq.0) then
               iPercent = 100/real((real(nCell)/real(iCell)))
               print *, iPercent ,'% Completed  (cell: ', iCell, ' of ', nCell, ')' ! , 'lat: ' , trim(adjustl(cLat)), ' lon: ', trim(adjustl(cLon))
             endif

             if (trim(aInFile2(iLat,iLon)) .NE. 'nofile') then
              !PRINT *,trim(aInFile2(iLat,iLon)), i, iLat, iLon

!!!!!!!!!!!!!!!!!!!!! READING ASCII FILE !!!!!!!!!!!!!!!!!!
              !                if (Set_V2N%binary == 3)  then ! if routing output
              !                    outFileName = trim(Set_V2N%outPath)//trim(adjustl(cLat))//'_'//trim(adjustl(cLon))//'.day'
              !                else
              fileName = trim(cInPath)//trim(aInFile2(iLat,iLon))
              !                endif

              open (unit = 13, file = fileName)
              !  print*,trim(fileName), ' --> 3'

              do iTime=1,nTime
                 READ(13,*) ncData(iLon,iLatTmp,iTime,:)
                 ! PRINT*,'--> ', ncData(iLon,iLatTmp,iTime,:)
              enddo
              close(13)
!!!!!!!!!!!!!!!!!!!!! READING ASCII FILE END !!!!!!!!!!!!!!!!!!
             iCell = iCell+ 1
             i=i+1
           endif
           !                   print*,'3a'
        enddo
        iLatTmp = iLatTmp + 1
     enddo




     do iVar=1,nVar

        write(cVar, "(I2)") iVar

!!!!!!!!!!!!!!!!!!!!! WRITING DATAFILE !!!!!!!!!!!!!!!!!!
        if (iMonthly == 1) then
           ncFileName = trim(cOutPath)//'/'//trim(cPrefixOut)//trim(adjustl(cVar)) &
                //'_monthly_'//trim(cYearStart)//'.nc'
        else
           ncFileName = trim(cOutPath)//'/'//trim(cPrefixOut)//trim(adjustl(cVar)) &
                //'_daily_'//trim(cYearStart)//'.nc'
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
           call check( nf90_def_var(ncid, 'var'//trim(adjustl(cVar)), nf90_float, &
                (/ dimidLon, dimidLat, dimidTime /), varidData) )
           ! Add attributes to the variables.
           call check( nf90_put_att(ncid, varidLon, "long_name", "longitude") )
           call check( nf90_put_att(ncid, varidLon, "units", "degrees_east") )
           call check( nf90_put_att(ncid, varidLon, "standard_name", "longitude") )
           call check( nf90_put_att(ncid, varidLat, "long_name", "latitude") )
           call check( nf90_put_att(ncid, varidLat, "units", "degrees_north") )
           call check( nf90_put_att(ncid, varidLat, "standard_name", "latitude") )
           if (iMonthly == 1) then
              call check( nf90_put_att(ncid, varidTime,"units", "months since "//trim(cYearStart)//"-01-01") )
           else
              call check( nf90_put_att(ncid, varidTime,"units", "days since "//trim(cYearStart)//"-01-01") )
           endif
           call check( nf90_put_att(ncid, varidTime,"calendar", "standard") )
           call check( nf90_put_att(ncid, varidData, "short_field_name", 'var'//trim(adjustl(cVar))) )
           !call check( nf90_put_att(ncid, varidData, "long_field_name", trim(Set_V2BIGN%varNameLong(iVar))) )
           call check( nf90_put_att(ncid, varidData, "_FillValue", noNadaValue) )

           !** End defititions
           call check( nf90_enddef(ncid) )
           !** Fill the variables lon and lat and time
           call check( nf90_put_var(ncid, varidLon ,aLon) )
           call check( nf90_put_var(ncid, varidLat ,aLat) )
           call check( nf90_put_var(ncid, varidTime ,aTime, &
                start = (/ 1 /), &
                count = (/ nTime /) ) )
           !** Close the netcdf file
           call check( nf90_close(ncid) )
        endif ! if iPart == 1

        ! OPEN NETCDF-FILE
        print *,'Add to NetCDF data file:        ', trim(ncFileName)
        call check( nf90_open(ncFileName, nf90_Write, ncid) )
        !** Fill the data variable
        call check( nf90_put_var(ncid, varidData, ncData(:,:,:,iVar) , ncStart(iPart,:),ncCount(iPart,:) ) )
        !** Close the netcdf file
        call check( nf90_close(ncid) )
!!!!!!!!!!!!!!!!!!!!! WRITING DATAFILE END !!!!!!!!!!!!!!!!!!
     enddo ! iVar
     if(allocated(ncData))                deallocate(ncData) 
  enddo !parts
contains
  subroutine check(status)
    integer, intent ( in) :: status

    if(status /= nf90_noerr) then
       print *, trim(nf90_strerror(status))
       stop "Stopped"
    end if
  end subroutine check
end SUBROUTINE vic2bignetcdf
!!!! NETCDF CHECK ROUTINE END !!!!

