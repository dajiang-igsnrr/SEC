module mesc_inout_module
  use mic_constant
  use mic_variable
  use netcdf
  implicit none

contains

!> this part reads restart file that includes all the pool sizes from previous model run
!! read the C and N pool sizes and assign them to the "miccpool" and "micnpool"
!! input:  netcdf file frestart_in
!! output: miccpool and micnppol
!!
  subroutine vmic_restart_read(miccpool,micnpool,frestart_in)
  ! read soil carbon pool sizes "miccpool%cpool(mp,ms,mcpool)"
    use netcdf
    use mic_constant
    use mic_variable
    implicit none
    TYPE(mic_cpool),              INTENT(INOUT)   :: miccpool
    TYPE(mic_npool),              INTENT(INOUT)   :: micnpool
    character*140 frestart_in                        ! restart filename
    ! local variables
    integer mpx,msx,mcpoolx                          ! array dimensions
    integer status,ncid,varid                        ! local variables
    real(r_2), dimension(mp,ms,mcpool)  :: fcpool    ! carbon pools
    real(r_2), dimension(mp,ms)         :: fnpool    ! nitrogen pools

   ! open restart file
    status = nf90_open(frestart_in,nf90_nowrite,ncid)
    if(status /= nf90_noerr) CALL nc_abort(STATUS, 'Error opening '//frestart_in)

    ! get dimensions
    status = nf90_inq_dimid(ncid,'mp',varid)
    if(status /= nf90_noerr) CALL nc_abort(STATUS, 'Error inquiring dimensions mp_id')
    status = nf90_inquire_dimension(ncid,varid,len=mpx)
    if(status /= nf90_noerr) CALL nc_abort(STATUS, 'Error reading mp')

    status = nf90_inq_dimid(ncid,'ms',varid)
    if(status /= nf90_noerr) CALL nc_abort(STATUS,'Error inquiring dimensions ms_id')
    status = nf90_inquire_dimension(ncid,varid,len=msx)
    if(status /= nf90_noerr) CALL nc_abort(STATUS, 'Error reading ms')
                        
    status = nf90_inq_dimid(ncid,'mcpool',varid)
    if(status /= nf90_noerr) CALL nc_abort(STATUS, 'Error inquiring dimensions mccpool_id')
    status = nf90_inquire_dimension(ncid,varid,len=mcpoolx)
    if(status /= nf90_noerr) CALL nc_abort(STATUS,'Error reading mcpool')   

    ! get variables
    status = nf90_inq_varid(ncid,'mic_cpool',varid)
    if(status /= nf90_noerr) CALL nc_abort(STATUS, 'Error inquiring miccpoolc')
    status = nf90_get_var(ncid,varid,fcpool)
    if(status /= nf90_noerr) CALL nc_abort(STATUS,'Error reading fcpool')

    status = nf90_inq_varid(ncid,'mic_npool',varid)
    if(status /= nf90_noerr) CALL nc_abort(STATUS, 'Error inquiring micnpoolc')
    status = nf90_get_var(ncid,varid,fnpool)
    if(status /= nf90_noerr) CALL nc_abort(STATUS,'Error reading fnpool')

    ! close the file
    status = NF90_close(ncid)
    if(status /= nf90_noerr) call nc_abort(status, 'Error in clsoing netCDF input file')

    ! assign the values from the restart file 
    if(mpx/=mp .or. msx/=ms .or. mcpoolx/=mcpool) then
       print *, 'dimensions do not match! ', mp,mpx,ms,msx,mcpool,mcpoolx
       STOP
    endif
    miccpool%cpool    = fcpool
    micnpool%mineralN = fnpool

  end subroutine vmic_restart_read
  
!> write out model pool sizes into restart file
!! input: frestart_out
!! output: miccpool%cpool, micnpool%npool
!!  
  subroutine vmic_restart_write(frestart_out,miccpool,micnpool)
  ! write out soil carbon pool sizes "miccpool%cpool(mp,ms,mcpool)"
    use netcdf
    use mic_constant
    use mic_variable  
    implicit None
    TYPE(mic_cpool),              INTENT(INOUT)   :: miccpool
    TYPE(mic_npool),              INTENT(INOUT)   :: micnpool
	! local variables for writing netcdf file
    INTEGER*4                :: STATUS
    INTEGER*4                :: FILE_ID, mp_ID, miccarb_ID, soil_ID   
    CHARACTER                :: CDATE*10,frestart_out*99
    INTEGER*4                :: cmic_ID, nmic_ID
    integer :: values(10)
    real(r_2)  missreal

    missreal=-1.0e10
    call date_and_time(values=values)
    WRITE(CDATE, '(I4.4,"-",I2.2,"-",I2.2)') values(1),values(2),values(3)
    
    ! Create NetCDF file:
    STATUS = NF90_create(frestart_out, NF90_CLOBBER, FILE_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, 'Error creating restart file ')

    WRITE(*,*) 'writing mic restart', frestart_out
    ! Put the file in define mode:
    STATUS = NF90_redef(FILE_ID)

    STATUS = NF90_PUT_ATT( FILE_ID, NF90_GLOBAL, "Valid restart date", CDATE )

    ! Define dimensions:
    ! mp (number of patches)
    STATUS = NF90_def_dim(FILE_ID, 'mp'   , mp     , mp_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, 'Error defining mp dimension ')

    ! ms: number of soil layers
    STATUS = NF90_DEF_DIM(FILE_ID, 'ms', ms, soil_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, 'Error defining soil dimension ' )

    ! mcpool: number of soil carbon pools
    STATUS = NF90_def_dim(FILE_ID, 'mcpool', mcpool, miccarb_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, 'Error defining mic_carbon_pools dimension ' )

    STATUS = NF90_def_var(FILE_ID,'mic_cpool',NF90_FLOAT,(/mp_ID,soil_ID,miccarb_ID/),cmic_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, 'Error defining mic_cpool variable ' )

    STATUS = NF90_def_var(FILE_ID,'mic_npool',NF90_FLOAT,(/mp_ID,soil_ID/),nmic_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, 'Error defining mic_npool variable ' )

    ! End define mode:
    STATUS = NF90_enddef(FILE_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, 'Error ending define mode ' )

    ! PUT VARS
    STATUS = NF90_PUT_VAR(FILE_ID, cmic_ID, REAL(miccpool%cpool, 4) )
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, 'Error writing mic_cpool variable ' )

    STATUS = NF90_PUT_VAR(FILE_ID, nmic_ID, REAL(micnpool%mineralN, 4) )
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, 'Error writing mic_npool variable ')

    ! Close NetCDF file:
    STATUS = NF90_close(FILE_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, 'Error closing restart file '  )

    write(*, *) 'restart file written to ', frestart_out

  end subroutine vmic_restart_write

!> abort model run in case when error occurs during reading/writing netcdf file
!! input integer variable ok
!! output: character string "message"
!!
  subroutine nc_abort( ok, message )
    USE netcdf
    ! Input arguments
    CHARACTER(LEN=*), INTENT(IN) :: message
    INTEGER, INTENT(IN) :: ok

    WRITE(*,*) message ! error from subroutine
    WRITE(*,*) NF90_STRERROR(ok) ! netcdf error details

    STOP

  end subroutine nc_abort

!> write out fluxes into a netcdf file
!> input:  micoutput%cinput,micoutput%rsoil,micoutput%cleach in gc/m2/year for each "mp"
!! output: netcdf foutput
!! "micinput" not used yet
!!
  subroutine vmic_output_write(foutput,micinput,micoutput)
    ! fNPP is not quite right yet. It shoudl be the sump of "cinputm+cinputs"
    use netcdf
    use mic_constant
    use mic_variable  
    implicit None
    TYPE(mic_input),         INTENT(INout)   :: micinput
    TYPE(mic_output),        INTENT(INout)   :: micoutput
    real(r_2)     missreal
    INTEGER*4                :: STATUS
    INTEGER*4                :: FILE_ID, mp_ID
    CHARACTER                :: CDATE*10,foutput*99
    INTEGER*4                :: cinput_ID, rsoil_ID, cleach_ID
    integer :: values(10)

    missreal=-1.0e10
    call date_and_time(values=values)
    WRITE(CDATE, '(I4.4,"-",I2.2,"-",I2.2)') values(1),values(2),values(3)
    ! Create NetCDF file:
    STATUS = NF90_create(foutput, NF90_CLOBBER, FILE_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, 'Error creating output file ')

    WRITE(*,*) 'writing output file', foutput
    print *, CDATE
    
    ! Put the file in define mode:
    STATUS = NF90_redef(FILE_ID)

    STATUS = NF90_PUT_ATT( FILE_ID, NF90_GLOBAL, "Valid output date", CDATE  )

    ! Define dimensions:
    ! mp (number of patches)
    STATUS = NF90_def_dim(FILE_ID, 'mp'   , mp     , mp_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, 'Error defining mp dimension ')

    STATUS = NF90_def_var(FILE_ID,'Cinput',NF90_FLOAT,(/mp_ID/),cinput_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, 'Error defining NPP ' )


    STATUS = NF90_def_var(FILE_ID,'rsoil',NF90_FLOAT,(/mp_ID/),rsoil_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, 'Error defining rsoil ' )


    STATUS = NF90_def_var(FILE_ID,'Cleach',NF90_FLOAT,(/mp_ID/),cleach_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, 'Error defining cleach ' )
    
    ! End define mode:
    STATUS = NF90_enddef(FILE_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, 'Error ending define mode ' )

    ! put attributes
    STATUS = NF90_PUT_ATT(FILE_ID,cinput_ID,'unit','g C m-2 year-1')
    STATUS = NF90_PUT_ATT(FILE_ID,cinput_ID,'missing_value', real(missreal,4))
    
    STATUS = NF90_PUT_ATT(FILE_ID,rsoil_ID,'unit','g C m-2 year-1')
    STATUS = NF90_PUT_ATT(FILE_ID,rsoil_ID,'missing_value', real(missreal,4))
        
    STATUS = NF90_PUT_ATT(FILE_ID,cleach_ID,'unit','g C m-2 year-1')
    STATUS = NF90_PUT_ATT(FILE_ID,cleach_ID,'missing_value', real(missreal,4))
    
    ! PUT VARS
    STATUS = NF90_PUT_VAR(FILE_ID, cinput_ID, REAL(micoutput%fluxcinput,4) )
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, 'Error writing NPP ' )

    STATUS = NF90_PUT_VAR(FILE_ID, rsoil_ID, REAL(micoutput%fluxrsoil,4) )
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, 'Error writing Rsoil ')

    STATUS = NF90_PUT_VAR(FILE_ID, cleach_ID, REAL(micoutput%fluxcleach,4) )
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, 'Error writing Cleach ')
    
    ! Close NetCDF file:
    STATUS = NF90_close(FILE_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, 'Error closing restart file '  )

    write(*, *) 'output written to ', foutput
    
  end subroutine vmic_output_write

!> get PFT-dependent model paramater values (up to 20 parameters)
!! input: hartd-wired parameter filename "parameters_global.csv"  
!! output: write the parameter values to "micpxdef"
!!
  
  subroutine getparam_global(fglobalparam,jmodel,micpxdef) 
    use mic_constant
    use mic_variable
    implicit none
    TYPE(mic_param_xscale)    :: micpxdef
    character*140  fglobalparam
    integer jmodel
    integer ibgc,ipft,n
    real(r_2), dimension(14)    :: x
    
    open(100,file=fglobalparam)
    read(100,*)
    do ibgc=1,mbgc
       read(100,*) ipft, (x(n),n=1,14)
       ! ensire x(1:16) are consistent with "vmic_param_xscale"
       micpxdef%xav(ibgc)        = x(1)
       micpxdef%xak(ibgc)        = x(2)
       micpxdef%xfm(ibgc)        = x(3)
       micpxdef%xfs(ibgc)        = x(4)
       micpxdef%xtvmic(ibgc)     = x(5)
       micpxdef%xtvp(ibgc)       = x(6)
       micpxdef%xtvc(ibgc)       = x(7)
       micpxdef%xtvac(ibgc)      = x(8)
       micpxdef%xkba(ibgc)       = x(9)
       micpxdef%xqmaxcoeff(ibgc) = x(10)
       micpxdef%xdiffsoc(ibgc)   = x(11)
       micpxdef%xnpp(ibgc)       = x(12)
       micpxdef%xvmaxbeta(ibgc)  = x(14) 
       ! the following parameters are fixed to 1.0	   
       micpxdef%xfp2ax(ibgc)     = 1.0
       micpxdef%xbeta(ibgc)      = 1.0
       micpxdef%xdesorp(ibgc)    = 1.0   
    enddo
    close(100)
      
    do ipft=1,mpft
       if(jmodel==1) then
          micpxdef%xrootbeta(ipft) = xrootcable(ipft) 
       endif
       if(jmodel==2 .or. jmodel==3) then
          micpxdef%xrootbeta(ipft) = xrootorchidee(ipft)
       endif
    enddo       

  end subroutine getparam_global

!> get number of patches 
!! input: hartd-wired parameter filename "fpatch"  
!! output: write the parameter values to "mpx"
!!  
  subroutine getpatch_global(fpatch,jmodel,mpx)
  ! read in global patch area fraction and calculate the number of land cell using sum(PFTfrac(lon,lat,pft))
  use netcdf
  use mic_constant
  character*140 fpatch
  integer jmodel,mpx
  real*8, dimension(:,:,:),   allocatable :: xfield3
  real*4, dimension(:,:,:,:), allocatable :: xfield4 
  integer i,j,np,ncid1,ok,varid,maxpft
  
    print *, 'patch filename', fpatch
    select case (jmodel)

      case(1) 
      allocate(xfield3(nlon,nlat,17))
      
      ok = NF90_OPEN(fpatch,0,ncid1)
      IF (ok /= NF90_NOERR) CALL nc_abort(ok,'Error opening file'//fpatch)
      ok = NF90_INQ_VARID(ncid1,'PFTfrac',varid)
      ok = NF90_GET_VAR(ncid1,varid,xfield3)
      ok = NF90_close(ncid1) 

      xfield3 = max(0.0,xfield3)   
      np=0 
      do i=1,nlon
      do j=1,nlat  
         if(sum(xfield3(i,j,:))>0.9) then
            maxpft= maxloc(xfield3(i,j,:),dim=1)
            if(maxpft >0 .and. maxpft <14) np=np+1
         endif  
      enddo
      enddo    

      deallocate(xfield3)

     case(2)
       allocate(xfield4(nlon,nlat,19,1))   
       ok = NF90_OPEN(fpatch,0,ncid1)
       IF (ok /= NF90_NOERR) CALL nc_abort(ok,'Error opening file'//fpatch)
       ok = NF90_INQ_VARID(ncid1,'maxvegetfrac',varid)
       ok = NF90_GET_VAR(ncid1,varid,xfield4)
       ok = NF90_close(ncid1) 

       xfield4 = max(0.0,xfield4)   
       np=0 
       do i=1,nlon
       do j=1,nlat  
          if(sum(xfield4(i,j,:,1))>0.9) then
             maxpft= maxloc(xfield4(i,j,:,1),dim=1)          
             if(maxpft >0 .and. maxpft <=mpft) np=np+1
          endif  
       enddo
       enddo    
     deallocate(xfield4)

    end select  

    mpx = np
  end subroutine getpatch_global   

!> get global CABLE forcing for running mes-c
!! input: hartd-wired parameter filename "fglobal_cable"  
!! output: write the parameter values to "micglobal% and micparam%"
!!  
  subroutine getdata_global_cable(fglobal,jglobal,jmodel,micglobal,micparam,zse)
  ! read in global forcing from CABLE/ORCHIDEE from time-invarying and time-varying data files
  ! averaging the input files for each land cell using PFTfrac 
  ! read in the following data
  ! real*4, dim(lon,lat) :: Ald,Alo,Fed,Feo
  ! real*8, dimension(lon,lat): cell_area  
  use netcdf
  use mic_constant
  use mic_variable
  implicit none
  TYPE(mic_global_input), INTENT(INOUT)  :: micglobal
  TYPE(mic_parameter),    INTENT(INOUT)  :: micparam
  real(r_2)  zse(ms)
  character*140 fglobal(10)
  integer       jglobal,jmodel
  ! local variables
  real(r_2), dimension(nlon)            :: lon
  real(r_2), dimension(nlat)            :: lat
  real(r_2), dimension(ntime)           :: time
  real(r_2), dimension(nlon,nlat,mpft)  :: patchfrac
  integer ncid1,ncid3,ok,lonid,latid,timeid,varid,n,np,ns
  !
  integer i,j,k,npx,isoilx,sorderx
  integer, dimension(:),        allocatable  :: ilon,jlat, fcluster
  real*4, dimension(:,:),       allocatable  :: varx2_flt
  real*8, dimension(:),         allocatable  :: varmp1_db
  real*8, dimension(:,:),       allocatable  :: varx2_db,varmp2_db
  real*8, dimension(:,:,:),     allocatable  :: varx3_db,varmp3_db,varsoc3_db,varbulk_db,varaoc_db
  real*8, dimension(:,:,:,:),   allocatable  :: varx4_db,watpot
  real*8, dimension(:,:,:,:,:), allocatable  :: varx5_db  
  real(r_2), dimension(:),      allocatable  :: falo,fald,ffeo,ffed
  integer   maxpft,pft
  real*8    bulkd2
  
  
    allocate(ilon(mp),jlat(mp),fcluster(mp))
    allocate(varx2_flt(nlon,nlat))
    allocate(varx2_db(nlon,nlat))
    allocate(varx3_db(nlon,nlat,mpft),varsoc3_db(nlon,nlat,ms),varbulk_db(nlon,nlat,ms),varaoc_db(nlon,nlat,ms))
    allocate(varx4_db(nlon,nlat,mpft,ntime),watpot(nlon,nlat,ms,ntime))
    allocate(varx5_db(nlon,nlat,mpft,ms,ntime))
    allocate(varmp1_db(mp))
    allocate(varmp2_db(mp,ntime))
    allocate(varmp3_db(mp,ms,ntime))
    allocate(falo(mp),fald(mp),ffeo(mp),ffed(mp))


  ! file 7: with aoc fraction
    ok = NF90_OPEN(fglobal(7),0,ncid1)
    IF (ok /= NF90_NOERR) CALL nc_abort(ok,'Error opening file'//fglobal(7))
    print *, 'global input1 = ', fglobal(7)
    ok = NF90_INQ_VARID(ncid1,'aoc_fraction',varid)
    ok = NF90_GET_VAR(ncid1,varid,varaoc_db)
    ok = NF90_close(ncid1) 

  
  ! file 1: time-invarying data
    ok = NF90_OPEN(fglobal(1),0,ncid3)
    IF (ok /= NF90_NOERR) CALL nc_abort(ok,'Error opening file'//fglobal(1))
    print *, 'global input1 = ', fglobal(1)

    ok = NF90_INQ_VARID(ncid3,'lon',lonid)
    ok = NF90_GET_VAR(ncid3,lonid,lon)    

    ok = NF90_INQ_VARID(ncid3,'lat',latid)    
    ok = NF90_GET_VAR(ncid3,latid,lat)    
     
    ok = NF90_INQ_VARID(ncid3,'PFTfrac',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3_db)
    patchfrac(:,:,:) = real(varx3_db(:,:,:),kind=r_2)  

    ok = NF90_INQ_VARID(ncid3,'HWSD_SOC',varid)
    ok = NF90_GET_VAR(ncid3,varid,varsoc3_db)

    ok = NF90_INQ_VARID(ncid3,'HWSD_bulk_density',varid)
    ok = NF90_GET_VAR(ncid3,varid,varbulk_db)
    
    patchfrac= max(0.0,patchfrac);micglobal%pft(:)=-1;micglobal%patchfrac(:,:)=0.0
    
    np=0 
    do i=1,nlon
    do j=1,nlat  
       if(sum(patchfrac(i,j,:))>0.9) then
          maxpft= maxloc(patchfrac(i,j,:),dim=1)
          if(maxpft >0 .and. maxpft <14) then
             np=np+1
             ilon(np) = i
             jlat(np) = j
             micglobal%lon(np)         = real(lon(i),kind=r_2)
             micglobal%lat(np)         = real(lat(j),kind=r_2)
             micparam%csoilobs(np,:)   = real(varsoc3_db(i,j,:),kind=r_2)              
             bulkd2                    = ( varbulk_db(i,j,1)*zse(1)+varbulk_db(i,j,2)*zse(2)+varbulk_db(i,j,3)*zse(3) &
                                         + varbulk_db(i,j,4)*zse(4)+varbulk_db(i,j,5)*zse(5)                          &
                                         + varbulk_db(i,j,6)*zse(6)+varbulk_db(i,j,7)*zse(7) )/sum(zse(1:7))   
             micglobal%bulkd(np)       = max(500.0_r_2,min(1800.0_r_2,real(bulkd2,kind=r_2)))
             micglobal%patchfrac(np,:) = patchfrac(i,j,:)             
             micglobal%pft(np)         = maxpft
             micparam%fracaoc(np,:)    = max(0.0_r_2,min(0.7_r_2,real(varaoc_db(i,j,:),kind=r_2)))
          endif
       endif  
    enddo
    enddo        

    if(np/=mp) then
      print *, 'np is not equal to mp', np,mp
      STOP
    endif      


    ok = NF90_INQ_VARID(ncid3,'area',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_flt)
    varx2_db = real(varx2_flt,kind=8)   
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    micglobal%area(:)=max(0.0, real(varmp1_db(:),kind=r_2)) *(1.0e-12)
   
    ok = NF90_INQ_VARID(ncid3,'SoilOrder',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_db)
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    micglobal%sorder(:) = int(varmp1_db(:))
     
    ok = NF90_INQ_VARID(ncid3,'isoil',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_db)
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    micglobal%isoil(:) = int(varmp1_db(:)) 

    ok = NF90_INQ_VARID(ncid3,'Ald',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_flt)
    varx2_db = real(varx2_flt,kind=8)
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    fald(:) = real(varmp1_db(:),r_2)
    print *, 'ald', maxval(fald), minval(fald),sum(fald)/real(mp)

    ok = NF90_INQ_VARID(ncid3,'Alo',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_flt)
    varx2_db = real(varx2_flt,kind=8)
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    falo(:) = real(varmp1_db(:),kind=r_2)
    print *, 'alo', maxval(falo), minval(falo),sum(falo)/real(mp)
    
    ok = NF90_INQ_VARID(ncid3,'Fed',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_flt)
    varx2_db = real(varx2_flt,kind=8)
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    ffed(:) = real(varmp1_db(:),kind=r_2)
    print *, 'ffed', maxval(ffed), minval(ffed),sum(ffed)/real(mp)
    
    ok = NF90_INQ_VARID(ncid3,'Feo',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_flt)
    varx2_db = real(varx2_flt,kind=8)
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    ffeo(:) = real(varmp1_db(:),kind=r_2)
    print *, 'ffeo', maxval(ffeo), minval(ffeo),sum(ffeo)/real(mp)
    
    ok = NF90_INQ_VARID(ncid3,'clay',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_flt)
    varx2_db = real(varx2_flt,kind=8)
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    micglobal%clay(:) = real(varmp1_db(:),kind=r_2)
    print *, 'clay', maxval(micglobal%clay), minval(micglobal%clay),sum(micglobal%clay)/real(mp)
    
    ok = NF90_INQ_VARID(ncid3,'silt',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_flt)
    varx2_db = real(varx2_flt,kind=8)
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    micglobal%silt = real(varmp1_db,kind=r_2)
    print *, 'silt', maxval(micglobal%silt), minval(micglobal%silt),sum(micglobal%silt)/real(mp)
    
    ok = NF90_INQ_VARID(ncid3,'pH',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_flt)
    varx2_db = real(varx2_flt,kind=8)
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    micglobal%ph = real(varmp1_db,kind=r_2)
    micglobal%ph =min(9.0,max(4.0,micglobal%ph))
    print *, 'ph', maxval(micglobal%ph), minval(micglobal%ph),sum(micglobal%ph)/real(mp)
    
    ok = NF90_INQ_VARID(ncid3,'npp',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3_db)
    varx3_db = max(0.0,varx3_db)
    call lonlat2mpx3(ilon,jlat,patchfrac,varx3_db,varmp1_db)
    micglobal%npp = real(varmp1_db,kind=r_2)
    micglobal%npp = max(100.0,micglobal%npp)
    print *, 'npp', maxval(micglobal%npp), minval(micglobal%npp),sum(micglobal%npp)/real(mp)
    
    ! using hwsd_bulk density
    !ok = NF90_INQ_VARID(ncid3,'rhosoil',varid)
    !ok = NF90_GET_VAR(ncid3,varid,varx2_flt)
    !varx2_db = real(varx2_flt,kind=8)    
    !call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    !micglobal%bulkd = real(varmp1_db,kind=r_2)
    !print *, 'bulkd', maxval(micglobal%bulkd), minval(micglobal%bulkd),sum(micglobal%bulkd)/real(mp)
    
    ok = NF90_INQ_VARID(ncid3,'lignin_CWD',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3_db)
    call lonlat2mpx3(ilon,jlat,patchfrac,varx3_db,varmp1_db)
    micglobal%ligwood = real(varmp1_db,kind=r_2)
    print *, 'ligwood', maxval(micglobal%ligwood), minval(micglobal%ligwood),sum(micglobal%ligwood)/real(mp)
    
    ok = NF90_INQ_VARID(ncid3,'lignin_leaf',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3_db)
    call lonlat2mpx3(ilon,jlat,patchfrac,varx3_db,varmp1_db)
    micglobal%ligleaf = real(varmp1_db,kind=r_2)
    print *, 'ligleaf', maxval(micglobal%ligleaf), minval(micglobal%ligleaf),sum(micglobal%ligleaf)/real(mp)
    
    ok = NF90_INQ_VARID(ncid3,'lignin_root',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3_db)
    call lonlat2mpx3(ilon,jlat,patchfrac,varx3_db,varmp1_db)
    micglobal%ligroot = real(varmp1_db,kind=r_2)
    print *, 'ligroot', maxval(micglobal%ligroot), minval(micglobal%ligroot),sum(micglobal%ligroot)/real(mp)
    
    ok = NF90_INQ_VARID(ncid3,'CN_ratio_leaf',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3_db)
    call lonlat2mpx3(ilon,jlat,patchfrac,varx3_db,varmp1_db)
    micglobal%cnleaf = real(spread(varmp1_db,dim=2,ncopies=ntime),kind=r_2)
    print *, 'cnleaf', maxval(micglobal%cnleaf), minval(micglobal%cnleaf),sum(micglobal%cnleaf)/real(size(micglobal%cnleaf))
    
    ok = NF90_INQ_VARID(ncid3,'CN_ratio_noleaf',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3_db)
    call lonlat2mpx3(ilon,jlat,patchfrac,varx3_db,varmp1_db)
    micglobal%cnwood = real(spread(varmp1_db,dim=2,ncopies=ntime),kind=r_2)
    print *, 'cnwood', maxval(micglobal%cnwood), minval(micglobal%cnwood),sum(micglobal%cnwood)/real(size(micglobal%cnwood))
    
    ok = NF90_INQ_VARID(ncid3,'CN_ratio_belowground',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3_db)
    call lonlat2mpx3(ilon,jlat,patchfrac,varx3_db,varmp1_db)
    micglobal%cnroot = real(spread(varmp1_db,dim=2,ncopies=ntime),kind=r_2)
    print *, 'cnroot', maxval(micglobal%cnroot), minval(micglobal%cnroot),sum(micglobal%cnroot)/real(size(micglobal%cnroot))
    
    ok = NF90_close(ncid3) 

    ! check the time-invariant data and replace bad values withy default values
    do np=1,mp
       pft = micglobal%pft(np)
       if(min(micglobal%ligwood(np),micglobal%ligleaf(np),micglobal%ligroot(np))<0.0 .or. &
          max(micglobal%ligwood(np),micglobal%ligleaf(np),micglobal%ligroot(np))>1.0) then
          micglobal%ligleaf(np) = ligleaf1(pft)
          micglobal%ligwood(np) = ligwood1(pft)
          micglobal%ligroot(np) = ligroot1(pft)          
       endif

       if(min(micglobal%cnleaf(np,1),micglobal%cnwood(np,1),micglobal%cnroot(np,1))<10.0 .or. &
          max(micglobal%cnleaf(np,1),micglobal%cnwood(np,1),micglobal%cnroot(np,1))>1000.0) then
          micglobal%cnleaf(np,:) = cnleaf1(pft)
          micglobal%cnwood(np,:) = cnwood1(pft)
          micglobal%cnroot(np,:) = cnroot1(pft)          
       endif
       ! replacing negative values of metal oxide with their global means in kg/m2
       if(fald(np)<0.0) fald(np) =0.46_r_2
       if(falo(np)<0.0) falo(np) =0.39_r_2
       if(ffed(np)<0.0) ffed(np) =2.74_r_2
       if(ffeo(np)<0.0) ffeo(np) =3.53_r_2
       micparam%siteid(np)       = np 
       micglobal%poros(:)  = 1.0 - micglobal%bulkd(:)/2650.0

    enddo

   ! use the lat and lon to estimate bgctype
    call cluster_centre(fglobal(3),micglobal%clay,micglobal%silt,micglobal%ph,fald,falo,ffed,ffeo,fcluster)
    micparam%bgctype =fcluster  
    micglobal%bgctype=fcluster
    
    
    ! reading time-varying data
    ! temporary solution
    do n=1,ntime
       micglobal%time(n) = n
    enddo   

    print *, 'reading time-varying data', fglobal(2)
    
  ! file 2: daily aboveground leaf fall (g C/m2/day)     ! Open netcdf file
    ok = NF90_OPEN(fglobal(2),0,ncid3)
    IF (ok /= NF90_NOERR) CALL nc_abort(ok,'Error opening file'//fglobal(2))
    
    ok = NF90_INQ_VARID(ncid3,'Leaf_fall',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx4_db)
    varx4_db = max(0.0, varx4_db)
    call lonlat2mpx4(ilon,jlat,patchfrac,varx4_db,varmp2_db)
    micglobal%dleaf = real(varmp2_db,kind=r_2)
    print *, 'dleaf', minval(micglobal%dleaf),maxval(micglobal%dleaf), &
                      sum(micglobal%dleaf)/real(size(micglobal%dleaf))
    
    ok = NF90_INQ_VARID(ncid3,'non_leaf_aboveground_litterfall',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx4_db)
    varx4_db = max(0.0, varx4_db)
    call lonlat2mpx4(ilon,jlat,patchfrac,varx4_db,varmp2_db)
    micglobal%dwood = real(varmp2_db,kind=r_2)
    print *, 'dwood', minval(micglobal%dwood),maxval(micglobal%dwood), &
                      sum(micglobal%dwood)/real(size(micglobal%dwood))
    
    ok = NF90_INQ_VARID(ncid3,'Belowground_litter_fall',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx4_db)
    varx4_db = max(0.0, varx4_db)
    call lonlat2mpx4(ilon,jlat,patchfrac,varx4_db,varmp2_db)
    micglobal%droot = real(varmp2_db,kind=r_2)
    print *, 'droot', minval(micglobal%droot),maxval(micglobal%droot), &
                      sum(micglobal%droot)/real(size(micglobal%droot))
    
    ok = NF90_INQ_VARID(ncid3,'SoilTemp',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx5_db)
    call lonlat2mpx5(ilon,jlat,patchfrac,-100.0d0,50.0d0,0.0d0,'tsoil',varx5_db,varmp3_db)
    micglobal%tsoil = real(varmp3_db,kind=r_2)
    print *, 'tsoil', minval(micglobal%tsoil),maxval(micglobal%tsoil), &
                      sum(micglobal%tsoil)/real(size(micglobal%tsoil))
    
    ok = NF90_INQ_VARID(ncid3,'SoilMoist',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx5_db)
    call lonlat2mpx5(ilon,jlat,patchfrac,0.0d0,1.0d0,0.15d0,'moist',varx5_db,varmp3_db)
    micglobal%moist = real(varmp3_db,kind=r_2)
    print *, 'moist', minval(micglobal%moist),maxval(micglobal%moist), &
                      sum(micglobal%moist)/real(size(micglobal%moist))
    
    ok = NF90_INQ_VARID(ncid3,'water_potential',varid)
    ok = NF90_GET_VAR(ncid3,varid,watpot)
    call lonlat2mpx4b(ilon,jlat,patchfrac,-1000.0d0,0.0d0,-100.0d0,'watpt',watpot,varmp3_db)
    micglobal%matpot = real(varmp3_db,kind=r_2)    
    print *, 'matpot', minval(micglobal%matpot),maxval(micglobal%matpot), &
                       sum(micglobal%matpot)/real(size(micglobal%matpot))
    
    ok = NF90_close(ncid3) 

    ! assign time-invariance properties from "micglobal" to "micparam"
    micparam%pft        = micglobal%pft
    micparam%bgctype    = micglobal%bgctype
    micparam%isoil      = micglobal%isoil
    micparam%sorder     = micglobal%sorder 
    micparam%fligleaf   = micglobal%ligleaf
    micparam%fligroot   = micglobal%ligroot
    micparam%fligwood   = micglobal%ligwood
    micparam%xcnleaf(:) = micglobal%cnleaf(:,1)
    micparam%xcnroot(:) = micglobal%cnroot(:,1)
    micparam%xcnwood(:) = micglobal%cnwood(:,1)

    ! filter out land cells with "bgctype<0"
    print *, 'calculations are not done for the following cells' 
    do np=1,mp
       if(micparam%bgctype(np) <1 .or. micparam%bgctype(np) >mbgc &
         .or. minval(micparam%csoilobs(np,:)) < 0.0               &
         .or. maxval(micparam%csoilobs(np,:)) > 120.0) then 
          print *, np, micparam%bgctype(np),micglobal%area(np),micglobal%isoil(np), &
                   micglobal%sorder(np),micglobal%bgctype(np), micglobal%npp(np)
          micparam%bgctype(np)= mbgc
          micglobal%area(np)  = -1.0
       endif
       ! replacing NPP in the time-invariant input file using the mean of time-varying input
       micglobal%npp(np) = 365.0 * sum(micglobal%dleaf(np,:) + micglobal%dwood(np,:) + micglobal%droot(np,:)) &
                         /real(size(micglobal%dleaf(np,:)))
    enddo

! write out time-invariant input data
    if(jglobal==1) then
       open(31,file=fglobal(5))
       do np=1,mp
          write(31,101) micparam%siteid(np),micglobal%area(np),micparam%pft(np), &
          micparam%isoil(np),micparam%sorder(np),micparam%bgctype(np),   &
          micglobal%npp(np), &
          minval(micglobal%dleaf(np,:) + micglobal%dwood(np,:) + micglobal%droot(np,:))*365.0, &
          maxval(micglobal%dleaf(np,:) + micglobal%dwood(np,:) + micglobal%droot(np,:))*365.0, &
          micglobal%ph(np),micglobal%clay(np)+micglobal%silt(np),micglobal%bulkd(np), &
          sum(micglobal%tsoil(np,:,:))/real(size(micglobal%tsoil(np,:,:))),           &
          sum(micglobal%moist(np,:,:))/real(size(micglobal%moist(np,:,:))),           &
          max(-1.0,micparam%csoilobs(np,:)),                                                    &
          micparam%fracaoc(np,1),micparam%fracaoc(np,3), micparam%fracaoc(np,ms)   

       enddo
       close(31) 
    endif    
101 format(i5,1x,f8.4,1x,4(i3,1x),100(f10.4,1x))

    
    deallocate(ilon,jlat,fcluster)
    deallocate(varx2_flt)
    deallocate(varx2_db)
    deallocate(varx3_db,varsoc3_db,varbulk_db,varaoc_db)
    deallocate(varx4_db,watpot)
    deallocate(varx5_db)
    deallocate(varmp1_db)    
    deallocate(varmp2_db)
    deallocate(varmp3_db)
    deallocate(falo,fald,ffeo,ffed)

   
  end subroutine getdata_global_cable

!> get global ORCHIDEE forcing for running mes-c
!! input: hartd-wired parameter filename "fglobal_cable"  
!! output: write the parameter values to "micglobal% and micparam%"
!! 
subroutine getdata_global_orchidee(fglobal,jglobal,jmodel,micglobal,micparam,zse)
  ! read in global forcing from ORCHIDEE from time-invarying and time-varying data files
  ! averaging the input files for each land cell using PFTfrac 
  ! read in the following data
  ! real*4, dim(lon,lat) :: Ald,Alo,Fed,Feo
  ! real*8, dimension(lon,lat): cell_area 
  ! use soil-grid or deafult values of soil silt, sand and clay fraction, soil pH and bulk density
  !  
  use netcdf
  use mic_constant
  use mic_variable
  implicit none
  TYPE(mic_global_input), INTENT(INOUT)  :: micglobal
  TYPE(mic_parameter),    INTENT(INOUT)  :: micparam
  real(r_2) zse(ms)
  character*140 fglobal(10)
  integer       jglobal,jmodel
  ! local variables
  real(r_2), dimension(nlon)            :: lon
  real(r_2), dimension(nlat)            :: lat
  real*4,    dimension(nlon)            :: lon_flt
  real*4,    dimension(nlat)            :: lat_flt  
  real(r_2), dimension(ntime)           :: time
  real(r_2), dimension(nlon,nlat,mpft)  :: patchfrac
  integer ncid1,ncid3,ok,lonid,latid,timeid,varid,n,np,ns
  !
  integer i,j,k,npx,isoilx,sorderx,ilonx,jlatx
  integer, dimension(:),        allocatable  :: ilon,jlat, fcluster
  real*4, dimension(:,:),       allocatable  :: varx2_flt
  real*4, dimension(:,:,:,:),   allocatable  :: varx4_flt
  real*8, dimension(:),         allocatable  :: varmp1_db
  real*8, dimension(:,:),       allocatable  :: varx2_db,varmp2_db
  real*8, dimension(:,:,:),     allocatable  :: varx3_db,varmp3_db,varsoc3_db,varbulk_db,varaoc_db
  real*8, dimension(:,:,:,:),   allocatable  :: varx4_db,tsoil4_db,watpot4_db
  real*8, dimension(:,:,:,:,:), allocatable  :: moist5_db  
  real(r_2), dimension(:),      allocatable  :: falo,fald,ffeo,ffed
  double precision, dimension(:,:),       allocatable  :: modisnpp
  double precision, dimension(:),         allocatable  :: modisnpp_mp
  integer   maxpft,pft
  real*8    bulkd2
  ! data
  real*4, dimension(12)    :: sandx,clayx,siltx,porex,bulkdx,fcpx,wiltx
  data sandx/0.93,0.81,0.63,0.17,0.06,0.40,0.54,0.08,0.30,0.48,0.06,0.15/
  data clayx/0.03,0.06,0.11,0.19,0.10,0.20,0.27,0.33,0.33,0.41,0.46,0.55/
  data siltx/0.04,0.13,0.26,0.64,0.84,0.40,0.19,0.59,0.37,0.11,0.48,0.30/
  data porex/0.43,0.41,0.41,0.45,0.46,0.43,0.39,0.43,0.41,0.38,0.36,0.38/
  data bulkdx/1510.5,1563.5,1563.5,1457.5,1431.0,1510.5,1616.5,1510.5,1563.5,1643.0,1696.0,1643.0/
  data fcpx/0.0493,0.0710,0.1218,0.2402,0.2582,0.1654,0.1695,0.3383,0.2697,0.2672,0.337,0.3469/
  data wiltx/0.0450,0.0570,0.0657,0.1039,0.0901,0.0884,0.1112,0.1967,0.1496,0.1704,0.2665,0.2707/

    allocate(ilon(mp),jlat(mp),fcluster(mp))
    allocate(varx2_flt(nlon,nlat))
    allocate(varx4_flt(nlon,nlat,mpft,1))
    allocate(varx2_db(nlon,nlat))
    allocate(varx3_db(nlon,nlat,mpft),varsoc3_db(nlon,nlat,ms),varbulk_db(nlon,nlat,ms),varaoc_db(nlon,nlat,ms))
    allocate(varx4_db(nlon,nlat,mpft,ntime))
    allocate(tsoil4_db(nlon,nlat,ms,ntime),watpot4_db(nlon,nlat,ms,ntime))
    allocate(moist5_db(nlon,nlat,ms,mpft,ntime))
    allocate(varmp1_db(mp))
    allocate(varmp2_db(mp,ntime))
    allocate(varmp3_db(mp,ms,ntime))
    allocate(falo(mp),fald(mp),ffeo(mp),ffed(mp))
    allocate(modisnpp(nlon,nlat),modisnpp_mp(mp))

  ! file 7: with aoc fraction
    ok = NF90_OPEN(fglobal(7),0,ncid1)
    IF (ok /= NF90_NOERR) CALL nc_abort(ok,'Error opening file'//fglobal(7))
    print *, 'global input1 = ', fglobal(7)
    ok = NF90_INQ_VARID(ncid1,'aoc_fraction',varid)
    ok = NF90_GET_VAR(ncid1,varid,varaoc_db)
    ok = NF90_close(ncid1) 
    
  ! file 1: time-invarying data
    ok = NF90_OPEN(fglobal(1),0,ncid3)
    IF (ok /= NF90_NOERR) CALL nc_abort(ok,'Error opening file'//fglobal(1))
    print *, 'global input1 = ', fglobal(1)

    ok = NF90_INQ_VARID(ncid3,'lon',lonid)
    ok = NF90_GET_VAR(ncid3,lonid,lon_flt)    
    lon(:) = real(lon_flt(:),kind=r_2)

    ok = NF90_INQ_VARID(ncid3,'lat',latid)    
    ok = NF90_GET_VAR(ncid3,latid,lat_flt)    
    lat(:) = real(lat_flt(:),kind=r_2)
    
    ok = NF90_INQ_VARID(ncid3,'maxvegetfrac',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx4_flt)
    patchfrac(:,:,:) = real(varx4_flt(:,:,:,1),kind=r_2)  

    ok = NF90_INQ_VARID(ncid3,'HWSD_SOC',varid)
    ok = NF90_GET_VAR(ncid3,varid,varsoc3_db)

    ok = NF90_INQ_VARID(ncid3,'HWSD_bulk_density',varid)
    ok = NF90_GET_VAR(ncid3,varid,varbulk_db)

    !   
    patchfrac= max(0.0,patchfrac);micglobal%pft(:)=-1;micglobal%patchfrac(:,:)=0.0
    np=0 
    do i=1,nlon
    do j=1,nlat  
       if(sum(patchfrac(i,j,:))>0.9) then
          maxpft= maxloc(patchfrac(i,j,:),dim=1)
          if(maxpft >0 .and. maxpft <=mpft) then
             np=np+1
             ilon(np) = i
             jlat(np) = j
             micglobal%lon(np)         = real(lon(i),kind=r_2)
             micglobal%lat(np)         = real(lat(j),kind=r_2)
             micparam%csoilobs(np,:)   = real(varsoc3_db(i,j,:),kind=r_2) 
             bulkd2                    = ( varbulk_db(i,j,1)*zse(1)+varbulk_db(i,j,2)*zse(2)+varbulk_db(i,j,3)*zse(3) &
                                         + varbulk_db(i,j,4)*zse(4)+varbulk_db(i,j,5)*zse(5)                          &
                                         + varbulk_db(i,j,6)*zse(6)+varbulk_db(i,j,7)*zse(7) )/sum(zse(1:7))   
             micglobal%bulkd(np)       = max(500.0_r_2,min(1800.0_r_2,real(bulkd2,kind=r_2)))
             micglobal%patchfrac(np,:) = patchfrac(i,j,:)             
             micglobal%pft(np)         = maxpft
             micparam%fracaoc(np,:)    = max(0.0_r_2,min(0.7_r_2,real(varaoc_db(i,j,:),kind=r_2))) 
          endif
       endif  
    enddo
    enddo        

    if(np/=mp) then
      print *, 'np is not equal to mp', np,mp
      STOP
    endif      


    ok = NF90_INQ_VARID(ncid3,'cell_area',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_db)
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    micglobal%area(:)=max(0.0, real(varmp1_db(:),kind=r_2)) *(1.0e-12)
   
    ok = NF90_INQ_VARID(ncid3,'USDA_Soil_texture_class',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_db)
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    micglobal%isoil(:) = int(varmp1_db(:))
     
    ok = NF90_INQ_VARID(ncid3,'USDA_SoilSuborder',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_flt)
    varx2_db = real(varx2_flt,kind=8)     
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    micglobal%sorder(:) = int(varmp1_db(:)) 

    ok = NF90_INQ_VARID(ncid3,'Ald',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_flt)
    varx2_db = real(varx2_flt,kind=8)
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    fald(:) = real(varmp1_db(:),r_2)
    print *, 'ald', maxval(fald), minval(fald),sum(fald)/real(mp)

    ok = NF90_INQ_VARID(ncid3,'Alo',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_flt)
    varx2_db = real(varx2_flt,kind=8)
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    falo(:) = real(varmp1_db(:),kind=r_2)
    print *, 'alo', maxval(falo), minval(falo),sum(falo)/real(mp)
    
    ok = NF90_INQ_VARID(ncid3,'Fed',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_flt)
    varx2_db = real(varx2_flt,kind=8)
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    ffed(:) = real(varmp1_db(:),kind=r_2)
    print *, 'ffed', maxval(ffed), minval(ffed),sum(ffed)/real(mp)
    
    ok = NF90_INQ_VARID(ncid3,'Feo',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_flt)
    varx2_db = real(varx2_flt,kind=8)
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    ffeo(:) = real(varmp1_db(:),kind=r_2)
    print *, 'ffeo', maxval(ffeo), minval(ffeo),sum(ffeo)/real(mp)
    
    ok = NF90_INQ_VARID(ncid3,'pH',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_flt)
    varx2_db = real(varx2_flt,kind=8)
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    micglobal%ph = real(varmp1_db,kind=r_2)
    micglobal%ph =min(9.0,max(4.0,micglobal%ph))
    print *, 'ph', maxval(micglobal%ph), minval(micglobal%ph),sum(micglobal%ph)/real(mp)
    
    ok = NF90_INQ_VARID(ncid3,'clay_soilgrid',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_flt)
    varx2_db = real(varx2_flt,kind=8)
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    micglobal%clay = real(varmp1_db,kind=r_2)
    micglobal%clay =min(1.0,max(0.0,micglobal%clay))
    print *, 'clay', maxval(micglobal%clay), minval(micglobal%clay),sum(micglobal%clay)/real(mp)    

    ok = NF90_INQ_VARID(ncid3,'silt_soilgrid',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_flt)
    varx2_db = real(varx2_flt,kind=8)
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    micglobal%silt = real(varmp1_db,kind=r_2)
    micglobal%silt =min(1.0,max(0.0,micglobal%silt))
    print *, 'silt', maxval(micglobal%silt), minval(micglobal%silt),sum(micglobal%silt)/real(mp) 
    
    ok = NF90_INQ_VARID(ncid3,'npp',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_db)
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)  
    micglobal%npp = real(varmp1_db,kind=r_2)
    micglobal%npp = max(100.0,micglobal%npp)
    print *, 'npp', maxval(micglobal%npp), minval(micglobal%npp),sum(micglobal%npp)/real(mp)

   
    ok = NF90_close(ncid3) 
    
    ! check the time-invariant data and replace bad values withy default values
    do np=1,mp
       pft = micglobal%pft(np)
       ns  = micglobal%isoil(np) 
       i=ilon(np);j=jlat(np)
       IF(pft <0 .or. pft >mpft .or. ns <1 .or. ns >12) then
          micglobal%area(np) = -1.0
          pft= 1; ns =1
       endif
   
      ! here used default look table values asin global ORCHIDEE 
      ! micglobal%clay(np) = real(clayx(ns),kind=r_2)
      ! micglobal%silt(np) = real(siltx(ns),kind=r_2)
      ! micglobal%bulkd(np)= real(bulkdx(ns),kind=r_2)
      ! micglobal%poros(np)= real(porex(ns),kind=r_2)  

       micglobal%ligleaf(np)  = ligleaf2(pft)
       micglobal%ligwood(np)  = ligwood2(pft)
       micglobal%ligroot(np)  = ligroot2(pft)      
       micglobal%cnleaf(np,:) = cnleaf2(pft)
       micglobal%cnwood(np,:) = cnwood2(pft)
       micglobal%cnroot(np,:) = cnroot2(pft)   
       micparam%siteid(np)    = np
       micglobal%poros(:)  = 1.0 - micglobal%bulkd(:)/2650.0
       ! replacing negative values of metal oxide with their global means in kg/m2
       if(fald(np)<0.0) fald(np) =0.46_r_2
       if(falo(np)<0.0) falo(np) =0.39_r_2
       if(ffed(np)<0.0) ffed(np) =2.74_r_2
       if(ffeo(np)<0.0) ffeo(np) =3.53_r_2
       
    enddo

   ! use the cluster centres to estimate bgctype
    call cluster_centre(fglobal(3),micglobal%clay,micglobal%silt,micglobal%ph,fald,falo,ffed,ffeo,fcluster)
    micparam%bgctype =fcluster  
    micglobal%bgctype=fcluster

    ! reading time-varying data
    ! temporary solution
    do n=1,ntime
       micglobal%time(n) = n
    enddo   

    print *, 'reading time-varying data', fglobal(2)
    
  ! file 2: daily aboveground leaf fall (g C/m2/day)     ! Open netcdf file
    ok = NF90_OPEN(fglobal(2),0,ncid3)
    IF (ok /= NF90_NOERR) CALL nc_abort(ok,'Error opening file'//fglobal(2))
    
    ok = NF90_INQ_VARID(ncid3,'Leaf_fall',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx4_db)
    varx4_db = max(0.0, varx4_db)
    call lonlat2mpx4(ilon,jlat,patchfrac,varx4_db,varmp2_db)
    micglobal%dleaf = real(varmp2_db,kind=r_2)
    print *, 'dleaf', minval(micglobal%dleaf),maxval(micglobal%dleaf), &
                      sum(micglobal%dleaf)/real(size(micglobal%dleaf))
    
    ok = NF90_INQ_VARID(ncid3,'non_leaf_aboveground_litterfall',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx4_db)
    varx4_db = max(0.0, varx4_db)
    call lonlat2mpx4(ilon,jlat,patchfrac,varx4_db,varmp2_db)
    micglobal%dwood = real(varmp2_db,kind=r_2)
    print *, 'dwood', minval(micglobal%dwood),maxval(micglobal%dwood), &
                      sum(micglobal%dwood)/real(size(micglobal%dwood))
    
    ok = NF90_INQ_VARID(ncid3,'Belowground_litter_fall',varid)
    ok = NF90_GET_VAR(ncid3,varid,varx4_db)
    varx4_db = max(0.0, varx4_db)
    call lonlat2mpx4(ilon,jlat,patchfrac,varx4_db,varmp2_db)
    micglobal%droot = real(varmp2_db,kind=r_2)
    print *, 'droot', minval(micglobal%droot),maxval(micglobal%droot), &
                      sum(micglobal%droot)/real(size(micglobal%droot))
    
    ok = NF90_INQ_VARID(ncid3,'SoilTemp',varid)
    ok = NF90_GET_VAR(ncid3,varid,tsoil4_db)
    call lonlat2mpx4b(ilon,jlat,patchfrac,-100.0d0,50.0d0,0.0d0,'tsoil',tsoil4_db,varmp3_db)
    micglobal%tsoil = real(varmp3_db,kind=r_2)
    print *, 'tsoil', minval(micglobal%tsoil),maxval(micglobal%tsoil), &
                      sum(micglobal%tsoil)/real(size(micglobal%tsoil))
    
    ok = NF90_INQ_VARID(ncid3,'SoilMoist',varid)
    ok = NF90_GET_VAR(ncid3,varid,moist5_db)
    call lonlat2mpx5a(ilon,jlat,patchfrac,0.0d0,1.0d0,0.15d0,'moist',moist5_db,varmp3_db)
    micglobal%moist = real(varmp3_db,kind=r_2)
    print *, 'moist', minval(micglobal%moist),maxval(micglobal%moist), &
                      sum(micglobal%moist)/real(size(micglobal%moist))
    
    ok = NF90_INQ_VARID(ncid3,'water_potential',varid)
    ok = NF90_GET_VAR(ncid3,varid,watpot4_db)
    call lonlat2mpx4b(ilon,jlat,patchfrac,-1000.0d0,0.0d0,-100.0d0,'watpt',watpot4_db,varmp3_db)
    micglobal%matpot = real(varmp3_db,kind=r_2)    
    print *, 'matpot', minval(micglobal%matpot),maxval(micglobal%matpot), &
                       sum(micglobal%matpot)/real(size(micglobal%matpot))
    
    ok = NF90_close(ncid3) 


    ! use modis-npp to rescale the orchidee NPP and carbon inputs to soil
    if(jmodel==3) then
       ok = nf90_open(fglobal(6),nf90_nowrite,ncid3)
       if(ok /= nf90_noerr) print*, 'Error opening modisnpp'
    
       ! get variables
       ok = nf90_inq_varid(ncid3,'npp',varid)
       if(ok /= nf90_noerr) print*, 'Error inquiring data modis_npp'
       ok = nf90_get_var(ncid3,varid,modisnpp)
       if(ok /= nf90_noerr) print*,'Error reading data npp'
       ! Close netcdf file
       ok = NF90_CLOSE(ncid3)   
       
       modisnpp_mp(:) = 0.0
       do np=1,mp
          ilonx=(micglobal%lon(np) + 179.75)/0.5 + 1
          jlatx=(89.75-micglobal%lat(np))/0.5    + 1
          modisnpp_mp(np) = max(100.0,modisnpp(ilonx,jlatx))
          micglobal%npp(np) = sum(micglobal%dleaf(np,:)+micglobal%dwood(np,:)+micglobal%droot(np,:)) *365.0/(real(ntime))
          micglobal%dleaf(np,:) = micglobal%dleaf(np,:) * modisnpp_mp(np)/micglobal%npp(np)
          micglobal%dwood(np,:) = micglobal%dwood(np,:) * modisnpp_mp(np)/micglobal%npp(np)
          micglobal%droot(np,:) = micglobal%droot(np,:) * modisnpp_mp(np)/micglobal%npp(np)
          micglobal%npp(np) = sum(micglobal%dleaf(np,:) + micglobal%dwood(np,:) + micglobal%droot(np,:))           
       enddo
    endif
    
   
    ! assign time-invariance properties from "micglobal" to "micparam"
    micparam%pft        = micglobal%pft
    micparam%bgctype    = micglobal%bgctype
    micparam%isoil      = micglobal%isoil
    micparam%sorder     = micglobal%sorder 
    micparam%fligleaf   = micglobal%ligleaf
    micparam%fligroot   = micglobal%ligroot
    micparam%fligwood   = micglobal%ligwood
    micparam%xcnleaf(:) = micglobal%cnleaf(:,1)
    micparam%xcnroot(:) = micglobal%cnroot(:,1)
    micparam%xcnwood(:) = micglobal%cnwood(:,1)

    ! filter out land cells with "bgctype<0"
    print *, 'calculations are not done for the following cells' 
    do np=1,mp
       if(micparam%bgctype(np) <1 .or. micparam%bgctype(np) >mbgc &
         .or. minval(micparam%csoilobs(np,:)) < 0.0               &
         .or. maxval(micparam%csoilobs(np,:)) > 120.0) then    
          print *, np, micparam%bgctype(np),micglobal%area(np),micglobal%isoil(np), &
                   micglobal%sorder(np),micglobal%bgctype(np), micglobal%npp(np)
          micparam%bgctype(np)= mbgc
          micglobal%area(np)  = -1.0
       endif
       ! replacing NPP in the time-invariant input file using the mean of time-varying input
       micglobal%npp(np) = 365.0 * sum(micglobal%dleaf(np,:) + micglobal%dwood(np,:) + micglobal%droot(np,:)) &
                         /real(size(micglobal%dleaf(np,:)))
    enddo

! write out time-invariant input data
    if(jglobal==1) then
       open(31,file=fglobal(5))
       do np=1,mp
          write(31,101) micparam%siteid(np),micglobal%area(np),micparam%pft(np), &
          micparam%isoil(np),micparam%sorder(np),micparam%bgctype(np),   &
          micglobal%npp(np), &
          minval(micglobal%dleaf(np,:) + micglobal%dwood(np,:) + micglobal%droot(np,:))*365.0, &
          maxval(micglobal%dleaf(np,:) + micglobal%dwood(np,:) + micglobal%droot(np,:))*365.0, &
          micglobal%ph(np),micglobal%clay(np)+micglobal%silt(np),micglobal%bulkd(np), &
          sum(micglobal%tsoil(np,:,:))/real(size(micglobal%tsoil(np,:,:))),           &
          sum(micglobal%moist(np,:,:))/real(size(micglobal%moist(np,:,:))),           &
          max(-1.0,micparam%csoilobs(np,:)),                                                    &
          micparam%fracaoc(np,1),micparam%fracaoc(np,3), micparam%fracaoc(np,ms)   
       enddo
       close(31) 
    endif    
101 format(i5,1x,f8.4,1x,4(i3,1x),100(f10.4,1x))


    
    deallocate(ilon,jlat,fcluster)
    deallocate(varx2_flt)
    deallocate(varx4_flt)
    deallocate(varx2_db)
    deallocate(varx3_db,varsoc3_db,varbulk_db,varaoc_db)
    deallocate(varx4_db)
    deallocate(tsoil4_db,watpot4_db)
    deallocate(moist5_db)
    deallocate(varmp1_db)
    deallocate(varmp2_db)
    deallocate(varmp3_db)
    deallocate(falo,fald,ffeo,ffed)
    deallocate(modisnpp,modisnpp_mp)
    
end subroutine getdata_global_orchidee

!> compute cluster based on the z-transformed value of soil properties
!! input: clay, silt, ph, ald, alo, fed and feo  
!! output: cluster (integer)
!!   
subroutine cluster_centre(filecluster,fclay,fsilt,fph,fald,falo,ffed,ffeo,fcluster)
  use mic_constant
  implicit none
  character*140 filecluster
  integer, dimension(mp)   :: fcluster
  real(r_2), dimension(mp) :: fclay,fsilt,fph,fald,falo,ffed,ffeo
  real, dimension(10)      :: claymid,siltmid,phmid,aldmid,alomid,fedmid,feomid  
  real*8  clayavg,siltavg,phavg,aldavg,aloavg,fedavg,feoavg
  real*8  claysd,siltsd,phsd,aldsd,alosd,fedsd,feosd
  real(r_2), dimension(10)  :: xdist
  integer n,m
  
    open(81,file=filecluster)
    read(81,*)
    read(81,*) claymid(1:10), clayavg,claysd
    read(81,*) siltmid(1:10), siltavg,siltsd
    read(81,*) phmid(1:10),   phavg,  phsd
    read(81,*) aldmid(1:10),  aldavg,aldsd
    read(81,*) alomid(1:10),  aloavg,alosd
    read(81,*) fedmid(1:10),  fedavg,fedsd
    read(81,*) feomid(1:10),  feoavg,feosd
    close(81)
    
    fcluster(:)=-1
    do n=1,mp
       xdist(:) = 1.0e6
       do m=1,10
          xdist(m) = ((fclay(n) - clayavg)/claysd - claymid(m))** 2 &
                   + ((fsilt(n) - siltavg)/siltsd - siltmid(m))** 2 & 
                   + ((fph(n)   - phavg)/phsd     - phmid(m))** 2   & 
                   + ((fald(n)  - aldavg)/aldsd   - aldmid(m))** 2  & 
                   + ((falo(n)  - aloavg)/alosd   - alomid(m))** 2  & 
                   + ((ffed(n)  - fedavg)/fedsd   - fedmid(m))** 2  & 
                   + ((ffeo(n)  - feoavg)/feosd   - feomid(m))** 2  
       enddo
       fcluster(n) = MINLOC(xdist,dim=1)

    enddo       
end subroutine cluster_centre

!> map 2d variable into 1d
!! input: varx2_db(nlon,nlat) 
!! output: varmp1_db(mp)
!!      
subroutine lonlat2mpx2(ilon, jlat, varx2_db, varmp1_db)
    ! map varx2_db(nlon,nlat) to varmp1_db(mp) 
    use mic_constant
    implicit none

    integer,   dimension(mp)              :: ilon,jlat
    real*8,    dimension(nlon,nlat)       :: varx2_db
    real*8,    dimension(mp)              :: varmp1_db
    integer :: np

    ! Initialize output (optional)
    varmp1_db = 0.0d0

    do np = 1, mp
        ! Assign value
        varmp1_db(np) = varx2_db(ilon(np), jlat(np))
    end do

end subroutine lonlat2mpx2

!> mapping 3d double real variables to 1d (mp)
!! input: varx3_db 
!! output: varmp1_db
!!   
subroutine lonlat2mpx3(ilon, jlat, patchfrac, varx3_db, varmp1_db)
! map varx3_db(nlon,nlat,mpft) to varmp1_db(mp) 
    use mic_constant
    implicit none

    integer,   dimension(mp)              :: ilon,jlat
    real(r_2), dimension(nlon,nlat,mpft)  :: patchfrac  
    real*8,    dimension(nlon,nlat,mpft)  :: varx3_db
    real*8,    dimension(mp)              :: varmp1_db
    integer :: np
    real*8, dimension(mpft)               :: varx_slice, weights
    real*8  :: areatot

    ! Initialize output
    varmp1_db = 0.0d0

    do np = 1, mp
        ! Extract all PFT values and weights
        varx_slice = varx3_db(ilon(np), jlat(np), 1:mpft)
        weights    = patchfrac(ilon(np), jlat(np), 1:mpft)
        areatot    = sum(weights)
        varmp1_db(np) = sum(varx_slice * weights) / areatot
    end do

end subroutine lonlat2mpx3

!> mapping 4d double real variables to 2d (mp)
!! input: varx4_db 
!! output: varmp2_db
!!   
subroutine lonlat2mpx4(ilon, jlat, patchfrac, varx4_db, varmp2_db)
! map varx3_db(nlon,nlat,mpft,ntime) to varmp2_db(mp,ntime) 
    use mic_constant
    implicit none
    integer, dimension(mp)  :: ilon,jlat
    integer np, day, mpft_size
    real(r_2), dimension(nlon,nlat,mpft)        :: patchfrac
    real*8,    dimension(nlon,nlat,mpft,ntime)  :: varx4_db
    real*8,    dimension(mp,ntime)              :: varmp2_db
    real*8,    dimension(mpft)                  :: varx_slice, weights
    real*8     areatot

    ! Initialize output
    varmp2_db = 0.0d0

    do np = 1, mp
       weights(:)    = patchfrac(ilon(np), jlat(np),:)
       areatot       = sum(weights)
       do day = 1, ntime
          ! Extract all PFT values for this np, day
          varx_slice(:) = varx4_db(ilon(np), jlat(np), :, day)
          varmp2_db(np, day) = sum(varx_slice(:) * weights(:)) / areatot
        end do
    end do

end subroutine lonlat2mpx4

!> mapping 3d double real variables to 2d (mp)
!! input: varx3_db(nlon,nlat,ms,ntime)
!! output: varmp2_db(mp,ms,ntime) 
!!   
subroutine lonlat2mpx4b(ilon,jlat,patchfrac,xmin,xmax,xdef,varname,watpot,varmp3_db)
! map varx3_db(nlon,nlat,ms,ntime) to varmp2_db(mp,ms,ntime) 
    use mic_constant
    implicit none

    integer, dimension(mp)                      :: ilon, jlat
    real(r_2), dimension(nlon,nlat,mpft)        :: patchfrac
    real*8, dimension(nlon,nlat,ms,ntime)       :: watpot
    real*8, dimension(mp,ms,ntime)              :: varmp3_db
    real*8  :: xmin, xmax,xdef
    integer np
    character*5 varname

    ! Initialize output
    varmp3_db = 0.0d0

    do np = 1, mp

       varmp3_db(np,:,:) = watpot(ilon(np),jlat(np),:,:)
       
       ! check the range and print out if outside the range
       if(minval(varmp3_db(np,:,:)) <xmin .or. maxval(varmp3_db(np,:,:)) >xmax) then
        !  print *, 'values outside the range :', np,ilon(np),jlat(np)
        !  print *, 'range =', varname, xmin, xmax
        !  print *, 'values= ',varmp3_db(np,:,1)
          varmp3_db(np,:,:)=xdef
       endif
    end do
    
    
end subroutine lonlat2mpx4b    


!> mapping 5d double real variables to 3d (mp)
!! input: varx5_db(nlon,nlat,mpft,ms,ntime)
!! output: varmp3_db(mp,ms,ntime)
!!    
subroutine lonlat2mpx5(ilon, jlat, patchfrac,xmin,xmax,xdef,varname,varx5_db, varmp3_db)
! map varx5_db(nlon,nlat,mpft,ms,ntime) to varmp3_db(mp,ms,ntime) 
    use mic_constant
    implicit none

    integer, dimension(mp)                      :: ilon, jlat
    real(r_2), dimension(nlon,nlat,mpft)        :: patchfrac
    real*8, dimension(nlon,nlat,mpft,ms,ntime)  :: varx5_db
    real*8, dimension(mp,ms,ntime)              :: varmp3_db
    real*8, dimension(mpft)                     :: wts
    integer :: np, pft
    real*8  :: xmin, xmax,xdef,total_weight
    character*5 varname

    ! Initialize output
    varmp3_db = 0.0d0

    do np = 1, mp
       ! Extract weights once for this np
       wts = patchfrac(ilon(np), jlat(np), :)
       total_weight = sum(wts)
       do pft=1,mpft
          varmp3_db(np,:,:) =  varmp3_db(np,:,:) + varx5_db(ilon(np),jlat(np),pft,:,:) * wts(pft)
       enddo
       varmp3_db(np,:,:) = varmp3_db(np,:,:)/total_weight
       
       ! check the range and print out if outside the range
       if(minval(varmp3_db(np,:,:)) <xmin .or. maxval(varmp3_db(np,:,:)) >xmax) then
        !  print *, 'values outside the range :', np, wts(:)
        !  print *, 'range =', varname, xmin, xmax
        !  print *, 'values= ',varmp3_db(np,:,1)
          varmp3_db(np,:,:) = xdef
       endif
    end do

end subroutine lonlat2mpx5


!> mapping 5d double real variables to 3d (mp)
!! input: varx5_db(nlon,nlat,ms,mpft,ntime)
!! output: varmp2_db(mp,ms,ntime)
!!   
subroutine lonlat2mpx5a(ilon, jlat, patchfrac,xmin,xmax,xdef,varname,varx5_db, varmp3_db)
! map varx5_db(nlon,nlat,ms,mpft,ntime) to varmp2_db(mp,ms,ntime) 
    use mic_constant
    implicit none

    integer, dimension(mp)                      :: ilon, jlat
    real(r_2), dimension(nlon,nlat,mpft)        :: patchfrac
    real*8, dimension(nlon,nlat,ms,mpft,ntime)  :: varx5_db
    real*8, dimension(mp,ms,ntime)              :: varmp3_db
    real*8, dimension(mpft)                     :: wts
    integer :: np, pft,ns
    real*8  :: xmin, xmax,xdef,total_weight
    character*5 varname

    ! Initialize output
    varmp3_db = 0.0d0

    do np = 1, mp
       ! Extract weights once for this np
       wts = patchfrac(ilon(np), jlat(np), :)
       total_weight = sum(wts)
       do ns=1,ms
       do pft=1,mpft
          varmp3_db(np,ns,:) =  varmp3_db(np,ns,:) + varx5_db(ilon(np),jlat(np),ns,pft,:) * wts(pft)
       enddo
       varmp3_db(np,ns,:) = varmp3_db(np,ns,:)/total_weight
       enddo
       
       ! check the range and print out if outside the range
       if(minval(varmp3_db(np,:,:)) <xmin .or. maxval(varmp3_db(np,:,:)) >xmax) then
        !  print *, 'values outside the range :', np, wts(:)
        !  print *, 'range =', varname, xmin, xmax
        !  print *, 'values= ',varmp3_db(np,:,1)
          varmp3_db(np,:,:) = xdef
       endif
    end do

end subroutine lonlat2mpx5a


 
!> read in global atmospheric C14 data and input data for model run with 14C
!! input 1: file 1 "frac14c" with all observed C14, carbon input, soil properties and other site-sepefici
!!        parameter for model run
!! input 2: file 2" f14c" with atmospheric 14C in five different zones
!! output: all data read in here are written into "micparam", "micinpout" and "micnpool"
!! "fcluster" is not read in yet
!!
  subroutine getdata_c14(frac14c,f14c,filecluster,micinput,micparam,micnpool,zse)
    use netcdf
    use mic_constant
    use mic_variable
    implicit none
    TYPE(mic_parameter), INTENT(INout)   :: micparam
    TYPE(mic_input),     INTENT(INout)   :: micinput
    TYPE(mic_npool),     INTENT(INOUT)   :: micnpool
    real(r_2) zse(ms)       ! soil layer thickness in m-2
	! local variables
    integer:: ncid,varid,status
    integer:: np,ns,i,j
    integer:: nz
    character*140 frac14c,f14c(5)
	character*140 filecluster   ! cluster filename (not used)

    character(len = nf90_max_name):: name
    real(r_2),dimension(:,:),allocatable:: fclay,fsilt,fph,ftemp,fmoist,fporosity,fmatpot
    real(r_2),dimension(:),  allocatable:: fsoc,fpoc,fmaoc,ffmpoc,ffmmaoc,fbulkd
    real(r_2),dimension(:),  allocatable:: fnpp,fanpp,fbnpp,flignin,fcna,fcnb
    integer,  dimension(:),  allocatable:: fid,fpft,ftop,fbot,fyear,fregion,fcluster
    real*8,   dimension(:),  allocatable:: lat,lon
 
    ! allocate variable for reading 
    allocate(fsoc(mp))

    allocate(fclay(mp,ms))
    allocate(fsilt(mp,ms))
    allocate(fph(mp,ms))
    allocate(ftemp(mp,ms))
    allocate(fmoist(mp,ms))
    allocate(fporosity(mp,ms))
    allocate(fmatpot(mp,ms))

    allocate(fnpp(mp))
    allocate(fanpp(mp))
    allocate(fbnpp(mp))
    allocate(flignin(mp))
    allocate(fcna(mp))
    allocate(fcnb(mp))
    allocate(fid(mp))
    allocate(fpft(mp))

    ! inputdata for 14C
    allocate(fpoc(mp))
    allocate(fmaoc(mp))
    allocate(ffmpoc(mp))
    allocate(ffmmaoc(mp))
    allocate(fbulkd(mp))
    allocate(ftop(mp)) !! upper depth of observed soil layer
    allocate(fbot(mp)) !! lower depth of observed soil layer
    allocate(fyear(mp)) !! year at which c14 was observed
    allocate(fregion(mp)) !! north/south hemisphere zone of c14
    allocate(fcluster(mp))
    allocate(lat(mp),lon(mp))
    
   ! open .nc file
    status = nf90_open(frac14c,nf90_nowrite,ncid)
    if(status /= nf90_noerr) print*, 'Error opening frc_c14.nc'

    ! get dimensions/profile_id
    status = nf90_inq_varid(ncid,'nsite',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring dimensions/profile_id'
    status = nf90_get_var(ncid,varid,fid)
    if(status /= nf90_noerr) print*,'Error reading profile_id'

    ! get variables
    status = nf90_inq_varid(ncid,'SOC',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring soc'
    status = nf90_get_var(ncid,varid,fsoc)
    if(status /= nf90_noerr) print*,'Error reading soc'

    status = nf90_inq_varid(ncid,'bulkd',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring bulk density'
    status = nf90_get_var(ncid,varid,fbulkd)
    if(status /= nf90_noerr) print*,'Error reading bulk density'

    status = nf90_inq_varid(ncid,'clay',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring clay'
    status = nf90_get_var(ncid,varid,fclay)
    if(status /= nf90_noerr) print*,'Error reading clay'

    status = nf90_inq_varid(ncid,'silt',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring silt'
    status = nf90_get_var(ncid,varid,fsilt)
    if(status /= nf90_noerr) print*,'Error reading silt'

    status = nf90_inq_varid(ncid,'ph',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring ph'
    status = nf90_get_var(ncid,varid,fph)
    if(status /= nf90_noerr) print*,'Error reading ph'

    status = nf90_inq_varid(ncid,'temp',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring soil temperature'
    status = nf90_get_var(ncid,varid,ftemp)
    if(status /= nf90_noerr) print*,'Error reading soil temperature'

    status = nf90_inq_varid(ncid,'moist',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring soil moisture'
    status = nf90_get_var(ncid,varid,fmoist)
    if(status /= nf90_noerr) print*,'Error reading soil moisture'

    status = nf90_inq_varid(ncid,'porosity',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring soil porosity'
    status = nf90_get_var(ncid,varid,fporosity)
    if(status /= nf90_noerr) print*,'Error reading soil porosity'

    status = nf90_inq_varid(ncid,'matpot',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring soil matric potential'
    status = nf90_get_var(ncid,varid,fmatpot)
    if(status /= nf90_noerr) print*,'Error reading soil matric potential'

    status = nf90_inq_varid(ncid,'npp',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring npp'
    status = nf90_get_var(ncid,varid,fnpp)
    if(status /= nf90_noerr) print*,'Error reading npp'

    status = nf90_inq_varid(ncid,'anpp',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring anpp'
    status = nf90_get_var(ncid,varid,fanpp)
    if(status /= nf90_noerr) print*,'Error reading anpp'

    status = nf90_inq_varid(ncid,'bnpp',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring bnpp'
    status = nf90_get_var(ncid,varid,fbnpp)
    if(status /= nf90_noerr) print*,'Error reading bnpp'

    status = nf90_inq_varid(ncid,'lignin_C',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring lignin/C'
    status = nf90_get_var(ncid,varid,flignin)
    if(status /= nf90_noerr) print*,'Error reading lignin/C'

    status = nf90_inq_varid(ncid,'cna',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring C/N aboveground'
    status = nf90_get_var(ncid,varid,fcna)
    if(status /= nf90_noerr) print*,'Error reading C/N aboveground'

    status = nf90_inq_varid(ncid,'cnb',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring C/N belowground'
    status = nf90_get_var(ncid,varid,fcnb)
    if(status /= nf90_noerr) print*,'Error reading C/N belowground'

    status = nf90_inq_varid(ncid,'pft',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring plant functional type'
    status = nf90_get_var(ncid,varid,fpft)
    if(status /= nf90_noerr) print*,'Error reading plant functional type'

      status = nf90_inq_varid(ncid,'POC',varid)
      if(status /= nf90_noerr) print*, 'Error inquiring POC'
      status = nf90_get_var(ncid,varid,fpoc)
      if(status /= nf90_noerr) print*,'Error reading POC'

      status = nf90_inq_varid(ncid,'MAOC',varid)
      if(status /= nf90_noerr) print*, 'Error inquiring MAOC'
      status = nf90_get_var(ncid,varid,fmaoc)
      if(status /= nf90_noerr) print*,'Error reading MAOC'

      status = nf90_inq_varid(ncid,'fm_poc',varid)
      if(status /= nf90_noerr) print*, 'Error inquiring fm_poc'
      status = nf90_get_var(ncid,varid,ffmpoc)
      if(status /= nf90_noerr) print*,'Error reading fm_poc'

      status = nf90_inq_varid(ncid,'fm_maoc',varid)
      if(status /= nf90_noerr) print*, 'Error inquiring fm_maoc'
      status = nf90_get_var(ncid,varid,ffmmaoc)
      if(status /= nf90_noerr) print*,'Error reading fm_maoc'

      status = nf90_inq_varid(ncid,'top_depth',varid)
      if(status /= nf90_noerr) print*, 'Error inquiring top depth'
      status = nf90_get_var(ncid,varid,ftop)
      if(status /= nf90_noerr) print*,'Error reading top depth'

      status = nf90_inq_varid(ncid,'bot_depth',varid)
      if(status /= nf90_noerr) print*, 'Error inquiring bottom depth'
      status = nf90_get_var(ncid,varid,fbot)
      if(status /= nf90_noerr) print*,'Error reading bottom depth'

      status = nf90_inq_varid(ncid,'c14_year',varid)
      if(status /= nf90_noerr) print*, 'Error inquiring c14 year'
      status = nf90_get_var(ncid,varid,fyear)
      if(status /= nf90_noerr) print*,'Error reading c14 year'

      status = nf90_inq_varid(ncid,'c14_region',varid)
      if(status /= nf90_noerr) print*, 'Error inquiring c14 region'
      status = nf90_get_var(ncid,varid,fregion)
      if(status /= nf90_noerr) print*,'Error reading c14 region'

      status = nf90_inq_varid(ncid,'Lon',varid)
      if(status /= nf90_noerr) print*, 'Error inquiring Lon'
      status = nf90_get_var(ncid,varid,lon)
      if(status /= nf90_noerr) print*,'Error reading Lon'

      status = nf90_inq_varid(ncid,'Lat',varid)
      if(status /= nf90_noerr) print*, 'Error inquiring Lat'
      status = nf90_get_var(ncid,varid,lat)
      if(status /= nf90_noerr) print*,'Error reading Lat'    
      
    ! Close netcdf file
    status = NF90_CLOSE(ncid)    

    ! we need to include additional data for kinetics3
   
      micparam%csoilobs(:,:) = -999.0
      do np=1, mp
   
         micparam%pft(np)    = int(fpft(np))
         micparam%siteid(np) = int(fid(np))

            micparam%top(np)         = int(ftop(np))
            micparam%bot(np)         = int(fbot(np))
            micparam%nyc14obs(np)    = int(fyear(np))     ! year when c14 is observed
            micparam%region(np)      = int(fregion(np))   ! south/north hemisphere zone of c14
            micparam%c14soilobsp(np) = ffmpoc(np)         ! poc c14 fraction modern
            micparam%c14soilobsm(np) = ffmmaoc(np)        ! maoc c14 fraction modern

         ! make sure "*delt" is not repeated in the model called by rk4
          micinput%fcnpp(np)      = fnpp(np)
          micinput%Dleaf(np)      = fanpp(np)/(24.0*365.0)*delt            !gc/m2/delt
          micinput%Droot(np)      = fbnpp(np)/(24.0*365.0)*delt            !gc/m2/delt
          !micinput%Dwood(np)      = forcdata(np,17)/(24.0*365.0)*delt     !gc/m2/delt (included in Dleaf or Droot already
		                                                                    

          micparam%xcnleaf(np)    = fcna(np)
          micparam%xcnroot(np)    = fcnb(np)
          !micparam%xcnwood(np)    = forcdata(np,20)
          micparam%fligleaf(np)   = flignin(np)
          micparam%fligroot(np)   = flignin(np)
          !micparam%fligwood(np)   = forcdata(np,23)

         do ns=1,ms
            micinput%tavg(np,ns)     = ftemp(np,ns)     ! average temperature in deg C
            micinput%wavg(np,ns)     = fmoist(np,ns)    ! average soil water content mm3/mm3
            micinput%clay(np,ns)     = fclay(np,ns)     ! clay content (fraction 0-1)
            micinput%silt(np,ns)     = fsilt(np,ns)     ! silt content (fraction 0-1)
            micinput%ph(np,ns)       = fph(np,ns)
            micinput%porosity(np,ns) = fporosity(np,ns) ! porosity mm3/mm3
            micinput%matpot(np,ns)   = fmatpot(np,ns)   ! soil matric potential -kPa

            micparam%csoilobs(np,ns)    = fsoc(np) 
            micinput%bulkd(np,ns)       = fbulkd(np)

            micparam%csoilobsp(np,ns)   = fpoc(np)
            micparam%csoilobsm(np,ns)   = fmaoc(np)
            
            !micnpool%mineralN(np,ns) = forcdata(np,7)*0.001 ! mineral N: "0.001" mg N /kg soil --> g N /kg soil
         enddo !"ns"
      enddo    ! "np=1,mp"

      ! read in the standard 14C atmospheric data for five zones
!         f14c(1) ='/g/data/w97/lw9370/combined-model/c14/code-structure/data/NH1-C14.csv'
!         f14c(2) ='/g/data/w97/lw9370/combined-model/c14/code-structure/data/NH2-C14.csv'
!         f14c(3) ='/g/data/w97/lw9370/combined-model/c14/code-structure/data/NH3-C14.csv'
!         f14c(4) ='/g/data/w97/lw9370/combined-model/c14/code-structure/data/SH12-C14.csv'
!         f14c(5) ='/g/data/w97/lw9370/combined-model/c14/code-structure/data/SH3-C14.csv'
         do nz=1,5
             call get14catm(nz,f14c(nz),micparam)
         enddo

    ! dealoocate variables
    deallocate(fsoc)
    deallocate(fbulkd)
    deallocate(fclay)
    deallocate(fsilt)
    deallocate(fph)
    deallocate(ftemp)
    deallocate(fmoist)
    deallocate(fporosity)
    deallocate(fmatpot)

    deallocate(fnpp)
    deallocate(fanpp)
    deallocate(fbnpp)
    deallocate(flignin)
    deallocate(fcna)
    deallocate(fcnb)
    deallocate(fid)
    deallocate(fpft)

    deallocate(fpoc)
    deallocate(fmaoc)
    deallocate(ffmpoc)
    deallocate(ffmmaoc)
    deallocate(ftop) !! upper depth of observed soil layer
    deallocate(fbot) !! bottom depth of observed soil layer
    deallocate(fyear) !! year at which c14 was observed
    deallocate(fregion) !! north/south hemisphere zone of c14
    deallocate(fcluster)
    deallocate(lat,lon)
    
   end subroutine getdata_c14

!> get dimeions: mp from the c fraction input file
!! 
   subroutine getdata_frc_dim(cfraction,mpx)
    use netcdf
    use mic_constant
    use mic_variable
    implicit none
    character*140 cfraction   
    integer mpx
    integer:: ncid,varid,status
   ! open .nc file
    status = nf90_open(cfraction,nf90_nowrite,ncid)
    if(status /= nf90_noerr) print*, 'Error opening c_fraction.nc'

    ! get dimension
    status = nf90_inq_dimid(ncid,'nsite',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring dimensions/nsite'
    status = nf90_inquire_dimension(ncid,varid,len=mpx)
    if(status /= nf90_noerr) print*,'Error dimensions/nsite'
 
    ! Close netcdf file
    status = NF90_CLOSE(ncid)   
   end subroutine  getdata_frc_dim   

!> read in data for model run to calculate POC and MAOC fractions
!! 
   subroutine getdata_frc(cfraction,filecluster,jglobal,bgcopt,micinput,micparam,micnpool,zse)
    use netcdf
    use mic_constant
    use mic_variable
    implicit none
    TYPE(mic_parameter), INTENT(INout)   :: micparam
    TYPE(mic_input),     INTENT(INout)   :: micinput
    TYPE(mic_npool),     INTENT(INOUT)   :: micnpool
    real(r_2)   zse(ms)
    integer jglobal,bgcopt 
    integer:: ncid,varid,status
    integer:: np,ns,i,j
    integer:: nz
    character*140 Cfraction,filecluster

    character(len = nf90_max_name):: name
    real(r_2),dimension(:),         allocatable:: fclay,fsilt,fph,ftemp,fmoist,fporosity,fmatpot
    real(r_2),dimension(:),         allocatable:: fsoc,fpoc,fmaoc,fbulkd
    real(r_2),dimension(:),         allocatable:: fnpp,fanpp,fbnpp,flignin,fcna,fcnb
    real(r_2),dimension(:),         allocatable:: fmg,fca,falo,fald,ffeo,ffed
    integer,dimension(:),           allocatable:: fid,fpft,ftop,fbot,fdataid,fcluster
    double precision, dimension(:), allocatable:: lat,lon
    ! local variation for clustering    
    integer n,msite
    

    allocate(fsoc(mp))
    allocate(fclay(mp))
    allocate(fsilt(mp))
    allocate(fph(mp))
    allocate(ftemp(mp))
    allocate(fmoist(mp))
    allocate(fporosity(mp))
    allocate(fmatpot(mp))

    allocate(fnpp(mp))
    allocate(fanpp(mp))
    allocate(fbnpp(mp))
    allocate(flignin(mp))
    allocate(fcna(mp))
    allocate(fcnb(mp))
    allocate(fid(mp))
    allocate(fpft(mp))

    ! inputdata for 14C
    allocate(fpoc(mp))
    allocate(fmaoc(mp))
    allocate(fbulkd(mp))
    allocate(ftop(mp)) !! upper depth of observed soil layer
    allocate(fbot(mp)) !! lower depth of observed soil layer
    allocate(fdataid(mp)) !! 1 for LUCAS; 2 for AUS; 3 for KG

    allocate(fca(mp))
    allocate(fmg(mp))
    allocate(falo(mp))
    allocate(fald(mp)) 
    allocate(ffeo(mp)) 
    allocate(ffed(mp)) 

    allocate(lat(mp),lon(mp)) 
    allocate(fcluster(mp))
    
   ! open .nc file
    status = nf90_open(Cfraction,nf90_nowrite,ncid)
    if(status /= nf90_noerr) print*, 'Error opening c_fraction.nc'

    ! get dimensions/profile_id
    status = nf90_inq_varid(ncid,'nsite',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring dimensions/profile_id'
    status = nf90_get_var(ncid,varid,fid)
    if(status /= nf90_noerr) print*,'Error reading profile_id'

    ! get variables
    status = nf90_inq_varid(ncid,'dataid',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring data ID'
    status = nf90_get_var(ncid,varid,fdataid)
    if(status /= nf90_noerr) print*,'Error reading data ID'

    status = nf90_inq_varid(ncid,'SOC',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring soc'
    status = nf90_get_var(ncid,varid,fsoc)
    if(status /= nf90_noerr) print*,'Error reading soc'

    status = nf90_inq_varid(ncid,'bulkd',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring bulk density'
    status = nf90_get_var(ncid,varid,fbulkd)
    if(status /= nf90_noerr) print*,'Error reading bulk density'

    status = nf90_inq_varid(ncid,'clay',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring clay'
    status = nf90_get_var(ncid,varid,fclay)
    if(status /= nf90_noerr) print*,'Error reading clay'

    status = nf90_inq_varid(ncid,'silt',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring silt'
    status = nf90_get_var(ncid,varid,fsilt)
    if(status /= nf90_noerr) print*,'Error reading silt'

    status = nf90_inq_varid(ncid,'ph',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring ph'
    status = nf90_get_var(ncid,varid,fph)
    if(status /= nf90_noerr) print*,'Error reading ph'

    status = nf90_inq_varid(ncid,'temp',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring soil temperature'
    status = nf90_get_var(ncid,varid,ftemp)
    if(status /= nf90_noerr) print*,'Error reading soil temperature'

    status = nf90_inq_varid(ncid,'moist',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring soil moisture'
    status = nf90_get_var(ncid,varid,fmoist)
    if(status /= nf90_noerr) print*,'Error reading soil moisture'

    status = nf90_inq_varid(ncid,'porosity',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring soil porosity'
    status = nf90_get_var(ncid,varid,fporosity)
    if(status /= nf90_noerr) print*,'Error reading soil porosity'

    status = nf90_inq_varid(ncid,'matpot',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring soil matric potential'
    status = nf90_get_var(ncid,varid,fmatpot)
    if(status /= nf90_noerr) print*,'Error reading soil matric potential'

    status = nf90_inq_varid(ncid,'npp',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring npp'
    status = nf90_get_var(ncid,varid,fnpp)
    if(status /= nf90_noerr) print*,'Error reading npp'

    status = nf90_inq_varid(ncid,'anpp',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring anpp'
    status = nf90_get_var(ncid,varid,fanpp)
    if(status /= nf90_noerr) print*,'Error reading anpp'

    status = nf90_inq_varid(ncid,'bnpp',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring bnpp'
    status = nf90_get_var(ncid,varid,fbnpp)
    if(status /= nf90_noerr) print*,'Error reading bnpp'

    status = nf90_inq_varid(ncid,'lignin_C',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring lignin/C'
    status = nf90_get_var(ncid,varid,flignin)
    if(status /= nf90_noerr) print*,'Error reading lignin/C'

    status = nf90_inq_varid(ncid,'cna',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring C/N aboveground'
    status = nf90_get_var(ncid,varid,fcna)
    if(status /= nf90_noerr) print*,'Error reading C/N aboveground'

    status = nf90_inq_varid(ncid,'cnb',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring C/N belowground'
    status = nf90_get_var(ncid,varid,fcnb)
    if(status /= nf90_noerr) print*,'Error reading C/N belowground'

    status = nf90_inq_varid(ncid,'pft',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring plant functional type'
    status = nf90_get_var(ncid,varid,fpft)
    if(status /= nf90_noerr) print*,'Error reading plant functional type'

    status = nf90_inq_varid(ncid,'POC',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring POC'
    status = nf90_get_var(ncid,varid,fpoc)
    if(status /= nf90_noerr) print*,'Error reading POC'

    status = nf90_inq_varid(ncid,'MAOC',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring MAOC'
    status = nf90_get_var(ncid,varid,fmaoc)
    if(status /= nf90_noerr) print*,'Error reading MAOC'

    status = nf90_inq_varid(ncid,'top_depth',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring top depth'
    status = nf90_get_var(ncid,varid,ftop)
    if(status /= nf90_noerr) print*,'Error reading top depth'

    status = nf90_inq_varid(ncid,'bot_depth',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring bottom depth'
    status = nf90_get_var(ncid,varid,fbot)
    if(status /= nf90_noerr) print*,'Error reading bottom depth'

    status = nf90_inq_varid(ncid,'Mg',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring Mg'
    status = nf90_get_var(ncid,varid,fmg)
    if(status /= nf90_noerr) print*,'Error reading Mg'
    
    status = nf90_inq_varid(ncid,'Ca',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring Ca'
    status = nf90_get_var(ncid,varid,fca)
    if(status /= nf90_noerr) print*,'Error reading Ca'

    status = nf90_inq_varid(ncid,'Alo',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring Alo'
    status = nf90_get_var(ncid,varid,falo)
    if(status /= nf90_noerr) print*,'Error reading Alo'

    status = nf90_inq_varid(ncid,'Alo',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring Alo'
    status = nf90_get_var(ncid,varid,falo)
    if(status /= nf90_noerr) print*,'Error reading Alo'

    status = nf90_inq_varid(ncid,'Ald',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring Ald'
    status = nf90_get_var(ncid,varid,fald)
    if(status /= nf90_noerr) print*,'Error reading Ald'

    status = nf90_inq_varid(ncid,'Feo',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring Feo'
    status = nf90_get_var(ncid,varid,ffeo)
    if(status /= nf90_noerr) print*,'Error reading Feo'

    status = nf90_inq_varid(ncid,'Fed',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring Fed'
    status = nf90_get_var(ncid,varid,ffed)
    if(status /= nf90_noerr) print*,'Error reading Fed'

    status = nf90_inq_varid(ncid,'Lat',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring Lat'
    status = nf90_get_var(ncid,varid,lat)
    if(status /= nf90_noerr) print*,'Error reading Lat'

    status = nf90_inq_varid(ncid,'Lon',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring Lon'
    status = nf90_get_var(ncid,varid,lon)
    if(status /= nf90_noerr) print*,'Error reading Lon'
    
    ! Close netcdf file
    status = NF90_CLOSE(ncid)    
    
    if(jglobal==1) open(100,file='inputdata_frc.txt')

    call cluster(filecluster,lat,lon,fcluster)
      micparam%bgctype=fcluster              
      micparam%csoilobs(:,:) = -999.0
      msite=0
      do np=1, mp
   
         micparam%pft(np)    = int(fpft(np))
    !     micparam%bgctype(np)= int(fpft(np))     
         micparam%siteid(np) = int(fid(np))
         micparam%dataid(np) = int(fdataid(np))
         micparam%top(np)         = max(int(zse(1))*100,int(ftop(np)))
         micparam%bot(np)         = int(fbot(np))

         ! make sure "*delt" is not repeated in the model called by rk4
          micinput%fcnpp(np)      = fnpp(np)
          micinput%Dleaf(np)      = fanpp(np)/(24.0*365.0)*delt    !gc/m2/delt
          micinput%Droot(np)      = fbnpp(np)/(24.0*365.0)*delt     !gc/m2/delt
          !micinput%Dwood(np)      = forcdata(np,17)/(24.0*365.0)*delt     !gc/m2/delt

          micparam%xcnleaf(np)    = fcna(np)
          micparam%xcnroot(np)    = fcnb(np)
          !micparam%xcnwood(np)    = forcdata(np,20)
          micparam%fligleaf(np)   = flignin(np)
          micparam%fligroot(np)   = flignin(np)
          !micparam%fligwood(np)   = forcdata(np,23)

         do ns=1,ms
            micinput%tavg(np,ns)     = ftemp(np)  ! average temperature in deg C
            micinput%wavg(np,ns)     = fmoist(np)  ! average soil water content mm3/mm3
            micinput%clay(np,ns)     = fclay(np)  ! clay content (fraction)
            micinput%silt(np,ns)     = fsilt(np)  ! silt content (fraction)
            micinput%ph(np,ns)       = fph(np)
            micinput%porosity(np,ns) = fporosity(np) !porosity mm3/mm3
            micinput%matpot(np,ns)   = fmatpot(np)  ! soil matric potential -kPa

            micparam%csoilobs(np,ns)    = fsoc(np) 
            micinput%bulkd(np,ns)       = fbulkd(np)

            micparam%csoilobsp(np,ns)   = fpoc(np)
            micparam%csoilobsm(np,ns)   = fmaoc(np)
            
            !micnpool%mineralN(np,ns) = forcdata(np,7)*0.001 ! mineral N: "0.001" mg N /kg soil --> g N /kg soil
         enddo !"ns"
         if(micparam%bgctype(np) ==bgcopt) then
            msite=msite + 1
         endif        
         if(jglobal==1) then
            write(100,901) micparam%siteid(np),micparam%dataid(np),micparam%pft(np),micparam%bgctype(nP),micparam%top(np),micparam%bot(np) , &
                         fnpp(np),fanpp(np),fbnpp(np),fcna(np),fcnb(np),flignin(np),ftemp(np),fmoist(np),fclay(np),fsilt(np),fph(np), &
                         fporosity(np),fmatpot(np),fbulkd(np),fald(np),falo(np),ffed(np),ffeo(np),fsoc(np),fpoc(np),fmaoc(np)
         endif        

      enddo    ! "np=1,mp"

    print *, 'total sites = ', msite, 'for bgcopt= ',bgcopt  
    if(jglobal==1) close(100)
901 format(6(i5,1x),25(f8.3,1x))    
    deallocate(fsoc)
    deallocate(fbulkd)
    deallocate(fclay)
    deallocate(fsilt)
    deallocate(fph)
    deallocate(ftemp)
    deallocate(fmoist)
    deallocate(fporosity)
    deallocate(fmatpot)

    deallocate(fnpp)
    deallocate(fanpp)
    deallocate(fbnpp)
    deallocate(flignin)
    deallocate(fcna)
    deallocate(fcnb)
    deallocate(fid)
    deallocate(fpft)

    deallocate(fpoc)
    deallocate(fmaoc)
    deallocate(ftop) !! upper depth of observed soil layer
    deallocate(fbot) !! bottom depth of observed soil layer
    deallocate(fdataid)

    deallocate(fca)
    deallocate(fmg)
    deallocate(falo)
    deallocate(fald) 
    deallocate(ffeo) 
    deallocate(ffed) 
    deallocate(lat,lon) 
    deallocate(fcluster)
    
   end subroutine getdata_frc

   subroutine get14catm(nz,f14cz,micparam)
   ! get the atmospheric 14C data 1941-2019 (inclusive, Hua et al. 2020)
    use mic_constant
    use mic_variable
    implicit none
    TYPE(mic_parameter), INTENT(INout)   :: micparam
    integer i, nz, ny, nc14atm(100,5)
    real(r_2)  year,c14del,sdx1,c14fm,sdx2
    character*140 f14cz
    ! give 14C zones globally
    ! 14C zone        region code
    ! NH zone 1       11
    ! NH zone 2       12
    ! NH zone 3       13
    ! SH zone 1,2     14
    ! SH zone 3       15

      micparam%c14atm(:,nz,:) = 0.0
      open(13,file=f14cz)
      do i=1,4
          read(13,*)
      enddo

      do i=1,79 !! 1941-2019
        read(13,*,end=91) year,c14del,sdx1,c14fm,sdx2
        ny = year - 1940
         if(ny<1 .or. ny>79) then
            print *, 'year', year, 'outside the range'
            stop
         else
            micparam%c14atm(ny,nz,1) = c14del !!! delta c14
            micparam%c14atm(ny,nz,2) = c14fm
         endif
      enddo
91    close(13)
   end subroutine get14catm
  
   subroutine getdata_hwsd_dim(fhwsdsoc,mpx,timex)
    use netcdf
    use mic_constant
    use mic_variable
    implicit none
    character*140 fhwsdsoc    
    integer mpx,timex
    integer:: ncid,varid,status
   ! open .nc file
    status = nf90_open(fhwsdsoc,nf90_nowrite,ncid)
    if(status /= nf90_noerr) print*, 'Error opening c_fraction.nc'

    ! get dimensions
    status = nf90_inq_dimid(ncid,'nsite',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring dimensions/nsite'
    status = nf90_inquire_dimension(ncid,varid,len=mpx)
    if(status /= nf90_noerr) print*,'Error dimensions/nsite'
  
    !
    status = nf90_inq_dimid(ncid,'time',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring dimensions/ntime'
    status = nf90_inquire_dimension(ncid,varid,len=timex)
    if(status /= nf90_noerr) print*,'Error reading profile_id'   
 
    ! Close netcdf file
    status = NF90_CLOSE(ncid)   
   end subroutine  getdata_hwsd_dim   

   subroutine getdata_hwsd(fhwsdsoc,filecluster,fmodis,fanoc,jglobal,bgcopt,jopt,jmodel,micparam,micglobal,zse)
    !use micglobal%area (area fraction) as a switch to run for selected sites during parameter optimization (jopt==0)  
    !model only runs for those sites with micglobal%area(np) > 0.0    
    use netcdf
    use mic_constant
    use mic_variable
    implicit none
    character*140 fhwsdsoc,filecluster,fmodis,fanoc
    integer jglobal,bgcopt,jopt,jmodel
    TYPE(mic_parameter),          INTENT(INout) :: micparam    
    TYPE(mic_global_input),       INTENT(INout) :: micglobal
    real(r_2) zse(ms)
    ! local variables
    integer:: ncid,varid,status
    integer:: np,ns,k,ipft,nsocobs,ilonx,jlatx
    integer:: intval,msite,isite,sitemax
    integer,           dimension(:),     allocatable     :: ivarx1,fcluster
    real,              dimension(:),     allocatable     :: varx1float
    real,              dimension(:,:),   allocatable     :: fracaoc
    double precision,  dimension(:),     allocatable     :: varx1db,avgts,avgms
    double precision,  dimension(:,:),   allocatable     :: varx2db,fsoc7,bulkd
    double precision,  dimension(:,:,:), allocatable     :: tsoil7,moist7,watpot7
    double precision,  dimension(:),     allocatable     :: fald,falo,ffed,ffeo
    double precision,  dimension(:,:),   allocatable     :: modisnpp
    double precision,  dimension(:),     allocatable     :: modisnpp_mp

    allocate(ivarx1(mp),fcluster(mp))
    allocate(varx1float(mp),varx1db(mp),avgts(mp),avgms(mp))
    allocate(varx2db(mp,ntime),fsoc7(mp,7),bulkd(mp,7))
    allocate(tsoil7(mp,7,ntime),moist7(mp,7,ntime),watpot7(mp,7,ntime))
    allocate(fald(mp),falo(mp),ffed(mp),ffeo(mp))
    allocate(modisnpp(720,360),modisnpp_mp(mp))
    allocate(fracaoc(mp,7))
   
   ! open .nc file
    print *, ' calling getdata_hwsd'
    print *,'input file', fhwsdsoc
    print *,'fansoc file', fanoc
    print *,'mp ms bgcopt=',    mp,ms,bgcopt
    
    status = nf90_open(fhwsdsoc,nf90_nowrite,ncid)
    if(status /= nf90_noerr) print*, 'Error opening c_fraction.nc'
    
    ! get variables
    status = nf90_inq_varid(ncid,'lat',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring data lat'
    status = nf90_get_var(ncid,varid,varx1db)
    if(status /= nf90_noerr) print*,'Error reading data lat'
    micglobal%lat = real(varx1db,kind=r_2)
    
    status = nf90_inq_varid(ncid,'lon',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring data lont'
    status = nf90_get_var(ncid,varid,varx1db)
    if(status /= nf90_noerr) print*,'Error reading data lon'
    micglobal%lon=real(varx1db,kind=r_2)
    
    status = nf90_inq_varid(ncid,'max_PFT',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring data PFT'
    status = nf90_get_var(ncid,varid,ivarx1)
    if(status /= nf90_noerr) print*,'Error reading data PFT'
    micglobal%pft = ivarx1

    status = nf90_inq_varid(ncid,'USDA_SoilSuborder',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring soil order'
    status = nf90_get_var(ncid,varid,ivarx1)
    if(status /= nf90_noerr) print*,'Error reading soil order'
    micglobal%sorder = ivarx1

    if(jmodel==1) then
       status = nf90_inq_varid(ncid,'isoil',varid)
       if(status /= nf90_noerr) print*, 'Error inquiring soil texturep'
       status = nf90_get_var(ncid,varid,ivarx1)
       if(status /= nf90_noerr) print*,'Error reading soil texure'
       micglobal%isoil = ivarx1
    endif
    if(jmodel==2 .or.jmodel==3) then
       status = nf90_inq_varid(ncid,'USDA_Soil_texture_class',varid)
       if(status /= nf90_noerr) print*, 'Error inquiring soil texturep'
       status = nf90_get_var(ncid,varid,ivarx1)
       if(status /= nf90_noerr) print*,'Error reading soil texure'
       micglobal%isoil = ivarx1
    endif

    status = nf90_inq_varid(ncid,'max_PFTfrac',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring max_PFTfrac'
    status = nf90_get_var(ncid,varid,varx1float)
    if(status /= nf90_noerr) print*,'Error reading max_PFTfrac'
    micglobal%area = real(varx1float,kind=r_2)    
    
    status = nf90_inq_varid(ncid,'npp',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring npp'
    status = nf90_get_var(ncid,varid,varx1db)
    if(status /= nf90_noerr) print*,'Error reading npp'
    micglobal%npp = real(varx1db,kind=r_2)     
    
    status = nf90_inq_varid(ncid,'pH',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring ph'
    status = nf90_get_var(ncid,varid,varx1float)
    if(status /= nf90_noerr) print*,'Error reading ph'
    micglobal%ph = real(varx1float,kind=r_2)
    
    status = nf90_inq_varid(ncid,'clay',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring clay'
    status = nf90_get_var(ncid,varid,varx1float)
    if(status /= nf90_noerr) print*,'Error reading clay'
    micglobal%clay = real(varx1float,kind=r_2)
    
    status = nf90_inq_varid(ncid,'silt',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring silt'
    status = nf90_get_var(ncid,varid,varx1float)
    if(status /= nf90_noerr) print*,'Error reading silt'
    micglobal%silt = real(varx1float,kind=r_2)
    
 !   status = nf90_inq_varid(ncid,'bulk_density',varid)
 !   if(status /= nf90_noerr) print*, 'Error inquiring soil bulk density'
 !   status = nf90_get_var(ncid,varid,varx1float)
 !   if(status /= nf90_noerr) print*,'Error reading bulk density'
 !   micglobal%bulkd= real(varx1float,kind=r_2)
 !   use HWSD bulk density (vary with soil layer)
     status = nf90_inq_varid(ncid,'HWSD_bulk_density',varid)
     if(status /= nf90_noerr) print*, 'Error inquiring soil bulk density'
     status = nf90_get_var(ncid,varid,bulkd)
     if(status /= nf90_noerr) print*,'Error reading bulk density'
 
    status = nf90_inq_varid(ncid,'HWSD_SOC',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring soil carbon'
    status = nf90_get_var(ncid,varid,fsoc7)
    if(status /= nf90_noerr) print*,'Error reading soil carbon'    
    
    status = nf90_inq_varid(ncid,'SoilTemp',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring soil temperature'
    status = nf90_get_var(ncid,varid,tsoil7)
    if(status /= nf90_noerr) print*,'Error reading soil temperature'
!    micglobal%tsoil=real(varx3db,kind=r_2)
    
    status = nf90_inq_varid(ncid,'SoilMoist',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring soil moisture'
    status = nf90_get_var(ncid,varid,moist7)
    if(status /= nf90_noerr) print*,'Error reading soil moisture'
!    micglobal%moist=real(varx3db,kind=r_2)
    
    status = nf90_inq_varid(ncid,'water_potential',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring soil matric potential'
    status = nf90_get_var(ncid,varid,watpot7)
    if(status /= nf90_noerr) print*,'Error reading soil matric potential'
!    micglobal%matpot=real(varx3db,kind=r_2)
    
    status = nf90_inq_varid(ncid,'Leaf_fall',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring Leaf_fall'
    status = nf90_get_var(ncid,varid,varx2db)
    if(status /= nf90_noerr) print*,'Error reading Leaf_fall'
    micglobal%dleaf=real(varx2db,kind=r_2)

    status = nf90_inq_varid(ncid,'Belowground_litter_fall',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring Belowground_litter_fall'
    status = nf90_get_var(ncid,varid,varx2db)
    if(status /= nf90_noerr) print*,'Error reading Belowground_litter_fall'
    micglobal%droot=real(varx2db,kind=r_2)

    status = nf90_inq_varid(ncid,'non_leaf_aboveground_litterfall',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring non_leaf_aboveground_litterfall'
    status = nf90_get_var(ncid,varid,varx2db)
    if(status /= nf90_noerr) print*,'Error reading non_leaf_aboveground_litterfall'
    micglobal%dwood =real(varx2db,kind=r_2)

    status = nf90_inq_varid(ncid,'Cluster',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring data Cluster'
    status = nf90_get_var(ncid,varid,ivarx1)
    if(status /= nf90_noerr) print*,'Error reading data Cluster'
    micglobal%bgctype = ivarx1
    micparam%bgctype  = ivarx1

    status = nf90_inq_varid(ncid,'Ald',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring data Ald'
    status = nf90_get_var(ncid,varid,fald)
    if(status /= nf90_noerr) print*,'Error reading data ald'

    status = nf90_inq_varid(ncid,'Alo',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring data Alo'
    status = nf90_get_var(ncid,varid,falo)
    if(status /= nf90_noerr) print*,'Error reading data alo'
    
    status = nf90_inq_varid(ncid,'Fed',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring data Fed'
    status = nf90_get_var(ncid,varid,ffed)
    if(status /= nf90_noerr) print*,'Error reading data Fed'

    status = nf90_inq_varid(ncid,'Feo',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring data Feo'
    status = nf90_get_var(ncid,varid,ffeo)
    if(status /= nf90_noerr) print*,'Error reading data Feo'
    
    ! Close netcdf file
    status = NF90_CLOSE(ncid)    

   ! check if calculated bgctype is same as the bgctype in the input data
    call cluster(filecluster,micglobal%lat,micglobal%lon,fcluster)
  !  micparam%bgctype=fcluster  
  !  micglobal%bgctype=fcluster

   ! get the ancient soc fraction for mp
     status = nf90_open(fanoc,nf90_nowrite,ncid)
     if(status /= nf90_noerr) print *, 'Error opening fanoc.nc' 
     status = nf90_inq_varid(ncid,'fanoc_mp',varid)
     if(status /= nf90_noerr) print *, 'Error inquiring fanoc_mp'
     status = nf90_get_var(ncid,varid,fracaoc)
     if(status /= nf90_noerr) print *,'Error reading fanoc_mp'
     ! Close netcdf file
     status = NF90_CLOSE(ncid) 
     
   ! if jmodel=3 use the annual modis NPP to scale the orchidee npp
    if(jmodel==3) then
       status = nf90_open(fmodis,nf90_nowrite,ncid)
       if(status /= nf90_noerr) print*, 'Error opening modisnpp'
    
       ! get variables
       status = nf90_inq_varid(ncid,'npp',varid)
       if(status /= nf90_noerr) print*, 'Error inquiring data modis_npp'
       status = nf90_get_var(ncid,varid,modisnpp)
       if(status /= nf90_noerr) print*,'Error reading data npp'
       ! Close netcdf file
       status = NF90_CLOSE(ncid) 
       
       modisnpp_mp(:) = 0.0
       do np=1,mp
          ilonx=(micglobal%lon(np) + 179.75)/0.5 + 1
          jlatx=(89.75-micglobal%lat(np))/0.5    + 1 
          modisnpp_mp(np) = max(100.0,modisnpp(ilonx,jlatx))
       enddo 
    endif
    
    do k=1,ntime
       micglobal%time(k)= real(k*1.0,kind=r_2)
    enddo

    ! print *, 'PFT=', micglobal%pft
    msite = 0
    do np=1, mp
       micglobal%siteid(np)  = np 
       ! calculate mean bulk density
       micglobal%bulkd(np) = (bulkd(np,1)*zse(1)+bulkd(np,2)*zse(2)+bulkd(np,3)*zse(3)+bulkd(np,4)*zse(4)+bulkd(np,5)*zse(5) &
                             +bulkd(np,6)*zse(6)+bulkd(np,7)*zse(7))/sum(zse(1:7))
!       micglobal%bgctype(np) = micglobal%sorder(np) 
       micglobal%poros(np)   = 1.0 - micglobal%bulkd(np)/2650.0
       micparam%siteid(np)   = micglobal%siteid(np)       
       micparam%pft(np)      = micglobal%pft(np) 
!       micparam%bgctype(np)  = micglobal%bgctype(np)       
       micparam%isoil(np)    = micglobal%isoil(np)
       micparam%sorder(np)   = micglobal%sorder(np)       
       if(jmodel==1) then      !CABLE 
          ipft =  micglobal%pft(np) 
          if(ipft<1 .or. ipft >17) then
             print *, 'PFT error at  np', jmodel,ipft,np
             stop
          endif             
          micparam%xcnleaf(np)  = cnleaf1(ipft)
          micparam%xcnroot(np)  = cnroot1(ipft)
          micparam%xcnwood(np)  = cnwood1(ipft)
          micparam%fligleaf(np) = ligleaf1(ipft)
          micparam%fligroot(np) = ligroot1(ipft)
          micparam%fligwood(np) = ligwood1(ipft)
       endif
       if(jmodel==2 .or.jmodel==3) then      !ORCHIDEE
          ipft =  micglobal%pft(np)
          if(ipft<1 .or. ipft >19) then
             print *, 'PFT error at  np', jmodel,ipft,np
             stop
          endif           
          micparam%xcnleaf(np)  = cnleaf2(ipft)
          micparam%xcnroot(np)  = cnroot2(ipft)
          micparam%xcnwood(np)  = cnwood2(ipft)
          micparam%fligleaf(np) = ligleaf2(ipft)
          micparam%fligroot(np) = ligroot2(ipft)
          micparam%fligwood(np) = ligwood2(ipft)       
       endif

       nsocobs=0

       do ns=1,ms

          micparam%csoilobs(np,ns) = real(fsoc7(np,ns),kind=r_2)
          micparam%fracaoc(np,ns)  = real(fracaoc(np,ns),kind=r_2)                
          micglobal%tsoil(np,ns,:) = real(tsoil7(np,ns,:),kind=r_2)
          micglobal%moist(np,ns,:) = real(moist7(np,ns,:),kind=r_2)
          micglobal%matpot(np,ns,:)= real(watpot7(np,ns,:),kind=r_2) 
          ! filter out sites with SOC >120 gc/kg (organic soil: Lourenco et al. 2022)
          if(micparam%csoilobs(np,ns) >=120.0) then 
             micglobal%area(np) = -1.0
          endif   
      
          if(micparam%csoilobs(np,ns) >0.0 .and. micparam%csoilobs(np,ns) < 1000.0) nsocobs = nsocobs + 1

       enddo 

       ! using "micglobal%area" to filter out some sites
       micglobal%npp(np) = sum(micglobal%dleaf(np,:) + micglobal%dwood(np,:) + micglobal%droot(np,:))     
       if(jmodel==3) then ! scale orchidee NPP using midNPP
          micglobal%dleaf(np,:) = micglobal%dleaf(np,:) * modisnpp_mp(np)/micglobal%npp(np)
          micglobal%dwood(np,:) = micglobal%dwood(np,:) * modisnpp_mp(np)/micglobal%npp(np)
          micglobal%droot(np,:) = micglobal%droot(np,:) * modisnpp_mp(np)/micglobal%npp(np)
          micglobal%npp(np) = sum(micglobal%dleaf(np,:) + micglobal%dwood(np,:) + micglobal%droot(np,:))           
       endif

       if(micglobal%npp(np)<100.0 .or. micglobal%ph(np)<3.0 .or. nsocobs==0) micglobal%area(np) = -1.0     

       if(micglobal%bgctype(np) ==bgcopt .and. micglobal%area(np) >0) msite = msite + 1 
    enddo    ! "np=1,mp"

    sitemax=300
    if(msite>2*sitemax) then

       intval = msite/sitemax; isite=0
       do np=1,mp
          if(micglobal%bgctype(np) == bgcopt .and.micglobal%area(np) > 0.0) then
             isite = isite +1
             if(int(isite/intval)*intval /= isite.or. isite>sitemax*intval) micglobal%area(np) = -1.0
          endif
          if(micglobal%area(np) > 0.0 .and. micglobal%bgctype(np) == bgcopt) then
             write(*,103) isite,np, micglobal%bgctype(np), micglobal%area(np),micglobal%npp(np),micglobal%ph(np)
          endif
       enddo
    else 

      isite=0
      do np=1,mp
         if(micglobal%area(np) > 0.0 .and. micglobal%bgctype(np) == bgcopt) then
            isite=isite+1     
            write(*,103) isite,np,micglobal%bgctype(np),micglobal%area(np),micglobal%npp(np),micglobal%ph(np)
         endif     
      enddo
      if(isite<10) print *, 'too few sites ', isite

    endif   

    micglobal%avgts(:) = sum(sum(micglobal%tsoil(:,:,:),dim=3),dim=2)/real(ms*ntime)
    micglobal%avgms(:) = sum(sum(micglobal%moist(:,:,:),dim=3),dim=2)/real(ms*ntime)    

    if(jglobal==1) then
       open(100,file='inputdata.txt')
       do np=1,mp
          write(100,101) micparam%siteid(np),micglobal%area(np),micparam%pft(np), &
          micparam%isoil(np),micparam%sorder(np),micparam%bgctype(np),   &
          micglobal%npp(np), &
          minval(micglobal%dleaf(np,:) + micglobal%dwood(np,:) + micglobal%droot(np,:)), &
          maxval(micglobal%dleaf(np,:) + micglobal%dwood(np,:) + micglobal%droot(np,:)), &
          micglobal%ph(np),micglobal%clay(np)+micglobal%silt(np),micglobal%bulkd(np), &
          micglobal%avgts(np),micglobal%avgms(np),sum(micparam%csoilobs(np,:)*zse(:))/sum(zse(:)), &
          micparam%fracaoc(np,1),micparam%fracaoc(np,3), micparam%fracaoc(np,ms)           
       enddo
       close(100) 
    endif    
101 format(i5,1x,f8.4,1x,4(i3,1x),30(f10.4,1x))
103 format(' run site', 3(i6,1x),10(f10.3,1x))

    deallocate(ivarx1,fcluster)
    deallocate(varx1float,varx1db,avgts,avgms)
    deallocate(varx2db,fsoc7)
    deallocate(tsoil7,moist7,watpot7)    
    deallocate(fald,falo,ffed,ffeo)
    deallocate(fracaoc)    
!    print *, 'exit getdata_hwsd'

end subroutine getdata_hwsd


subroutine cluster(filecluster,lat,lon,fcluster)
    ! use the nearest point to estimate the cluster 
    ! output
    ! micparam%bgctype: [1,10]
    use netcdf
    use mic_constant
    use mic_variable
    use, intrinsic :: ieee_arithmetic
    implicit none
    double precision, dimension(mp)             :: lat,lon
    integer, dimension(mp)                      :: fcluster
    double precision lathd(360), lonhd(720)
    real*4           varx_flt(720,360)
    integer          clusterhd(720,360)
    character*140    filecluster
    integer ncid,varid,status,i,j,ilon,jlat,np,icluster,freq(10)   
    
    ! read the hd by hd cluster map
    
    status = nf90_open(filecluster,nf90_nowrite,ncid)
    if(status /= nf90_noerr) print*, 'Error opening fcluster'
    
    ! get variables
    status = nf90_inq_varid(ncid,'lat',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring data lat'
    status = nf90_get_var(ncid,varid,lathd)
    
    if(status /= nf90_noerr) print*,'Error reading data lat'
    status = nf90_inq_varid(ncid,'lon',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring data lon'
    status = nf90_get_var(ncid,varid,lonhd)

    if(status /= nf90_noerr) print*,'Error reading data lon'
    status = nf90_inq_varid(ncid,'Band1',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring data Band1'
    status = nf90_get_var(ncid,varid,varx_flt)
    if(status /= nf90_noerr) print*,'Error reading data Band1'
    ! close the file
    status = NF90_close(ncid)
    if(status /= nf90_noerr) call nc_abort(status, 'Error in clsoing fcluster')    

    clusterhd = -999
    do i = 1, 720
      do j = 1, 360
        if (.not. ieee_is_finite(varx_flt(i,j))) then
          clusterhd(i,j) = -999
        else if (varx_flt(i,j) < 0.5 .or. varx_flt(i,j) > 10.5) then
          clusterhd(i,j) = -999
        else
          clusterhd(i,j) = nint(varx_flt(i,j))
        end if
      end do
    end do

    fcluster = -1
    do np=1,mp
       ilon = max(1, min(720, int((lon(np) + 179.75)/0.5 +1)))
       jlat = max(1, min(360, int((lat(np) + 89.75)/0.5  +1)))

       if(clusterhd(ilon,jlat) < 0 .or. clusterhd(ilon,jlat) >10) then
          freq(1:10) = 0
          do i=max(1,ilon-5),min(720,ilon+5)
             do j=max(1,jlat-5),min(360,jlat+5)
                icluster= clusterhd(i,j)
                if(icluster>0 .and. icluster<11) then
                   freq(icluster) = freq(icluster) +1
                endif
             enddo
          enddo
          fcluster(np) = maxloc(freq,dim=1)
       else
          fcluster(np) = clusterhd(ilon,jlat)
       endif
    enddo
101 format('freq ',i6,1x,i2,1x,2(f7.3,1x),15(i3,1x))    
end subroutine cluster


subroutine screenout(runmodel,jmodel,bgcopt,xopt,cost)
    use mic_constant
    implicit none
    character*10 runmodel
    integer jmodel,bgcopt
    real*8,    dimension(16)           :: xopt
    real*8     cost
    write(*,901) runmodel,jmodel,bgcopt,cost,xopt(1:14)
901 format(a10,2(i3,1x),f12.3,1x,14(f7.3,1x))    

end subroutine screenout


  
   subroutine getdata_aust_dim(faustsoc,mpx,timex)
    use netcdf
    use mic_constant
    use mic_variable
    implicit none
    character*140 faustsoc    
    integer mpx,timex
    integer:: ncid,varid,status
   ! open .nc file
    status = nf90_open(faustsoc,nf90_nowrite,ncid)
    if(status /= nf90_noerr) print*, 'Error opening faustsoc.nc'

    ! get dimensions
    status = nf90_inq_dimid(ncid,'msite',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring dimensions/nsite'
    status = nf90_inquire_dimension(ncid,varid,len=mpx)
    if(status /= nf90_noerr) print*,'Error dimensions/nsite'
    !
    status = nf90_inq_dimid(ncid,'mday',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring dimensions/ntime'
    status = nf90_inquire_dimension(ncid,varid,len=timex)
    if(status /= nf90_noerr) print*,'Error reading profile_id'   
 
    ! Close netcdf file
    status = NF90_CLOSE(ncid)   
   end subroutine  getdata_aust_dim   

   subroutine getdata_aust(faustsoc,jglobal,bgcopt,jopt,jmodel,micparam,micglobal,zse)
    !use micglobal%area (area fraction) as a switch to run for selected sites during parameter optimization (jopt==0)  
    !model only runs for those sites with micglobal%area(np) > 0.0    
    use netcdf
    use mic_constant
    use mic_variable
    implicit none
    real, dimension(9)  :: cnleaf,cnwood,cnroot,fracleaf,fracwood,fracroot,ligcleaf,ligcwood,ligcroot
    data cnleaf/41.653,73.565,81.04,66.675,35.33,62.898,64.967,64.0,20.0/
    data cnwood/71.272,106.111,124.84,128.762,59.924,83.557,105.973,105.97,100.0/
    data cnroot/36.0,36.0,36.0,36.0,36.0,36.0,36.0,36.0,36.0/
    data fracleaf/0.03,0.053,0.029,0.025,0.096,0.055,0.086,0.101,0.53/
    data fracwood/0.693,0.816,0.709,0.738,0.633,0.634,0.586,0.628,0.0/
    data fracroot/0.277,0.131,0.262,0.237,0.271,0.311,0.328,0.271,0.47/
    data ligcleaf/0.28,0.28,0.28,0.28,0.28,0.28,0.28,0.28,0.28/
    data ligcwood/0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4/
    data ligcroot/0.28,0.28,0.28,0.28,0.28,0.28,0.28,0.28,0.28/
    character*140 faustsoc
    integer jglobal,bgcopt,jopt,jmodel
    TYPE(mic_parameter),          INTENT(INout) :: micparam    
    TYPE(mic_global_input),       INTENT(INout) :: micglobal
    real(r_2) zse(ms)
    ! local variables
    integer:: ncid,varid,status
    integer:: np,k,ipft,ns
    integer,  dimension(:),     allocatable     :: ivarx1
    real,     dimension(:),     allocatable     :: varx1float,avgts,avgms,poc,hoc,roc
    real,     dimension(:,:),   allocatable     :: soc3,varx2float
    real,     dimension(:,:),   allocatable     :: npp10y,tsoil10y,moist10y10,moist10y100
    real,     dimension(:,:,:), allocatable     :: varx3float

    allocate(ivarx1(mp))
    allocate(varx1float(mp),avgts(mp),avgms(mp),soc3(mp,3),poc(mp),hoc(mp),roc(mp))
    allocate(varx2float(mp,3),npp10y(mp,ntime),tsoil10y(mp,ntime),moist10y10(mp,ntime),moist10y100(mp,ntime))
    allocate(varx3float(mp,10,ntime))

   
   ! open .nc file
    print *, ' calling getdata_aust'
    print *,'input file', faustsoc
    print *,'mp ms bgcopt=',    mp,ms,bgcopt
    
    status = nf90_open(faustsoc,nf90_nowrite,ncid)
    if(status /= nf90_noerr) print*, 'Error opening faustsoc.nc'
    
    ! get variables
    status = nf90_inq_varid(ncid,'lat',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring data lat'
    status = nf90_get_var(ncid,varid,varx1float)
    if(status /= nf90_noerr) print*,'Error reading data lat'
    micglobal%lat = real(varx1float,kind=r_2)
    
    status = nf90_inq_varid(ncid,'lon',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring data lon'
    status = nf90_get_var(ncid,varid,varx1float)
    if(status /= nf90_noerr) print*,'Error reading data lon'
    micglobal%lon=real(varx1float,kind=r_2)
    
    status = nf90_inq_varid(ncid,'vtype',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring data PFT'
    status = nf90_get_var(ncid,varid,ivarx1)
    if(status /= nf90_noerr) print*,'Error reading data PFT'
    micglobal%pft = ivarx1
    
    status = nf90_inq_varid(ncid,'nsite',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring nsite'
    status = nf90_get_var(ncid,varid,ivarx1)
    if(status /= nf90_noerr) print*,'Error reading nsite'
    micglobal%siteid = ivarx1
    
    status = nf90_inq_varid(ncid,'ph',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring ph'
    status = nf90_get_var(ncid,varid,varx1float)
    if(status /= nf90_noerr) print*,'Error reading ph'
    micglobal%ph = real(varx1float,kind=r_2)    

    status = nf90_inq_varid(ncid,'clay',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring clay'
    status = nf90_get_var(ncid,varid,varx1float)
    if(status /= nf90_noerr) print*,'Error reading clay'
    micglobal%clay = real(varx1float,kind=r_2)

    status = nf90_inq_varid(ncid,'silt',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring silt'
    status = nf90_get_var(ncid,varid,varx1float)
    if(status /= nf90_noerr) print*,'Error reading silt'
    micglobal%silt = real(varx1float,kind=r_2)

    status = nf90_inq_varid(ncid,'bulkd',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring bulkd'
    status = nf90_get_var(ncid,varid,varx1float)
    if(status /= nf90_noerr) print*,'Error reading bulkd'
    micglobal%bulkd = real(varx1float,kind=r_2)

    status = nf90_inq_varid(ncid,'soc',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring soc'
    status = nf90_get_var(ncid,varid,varx2float)
    if(status /= nf90_noerr) print*,'Error reading soc'
    soc3 = real(varx2float,kind=r_2)

    status = nf90_inq_varid(ncid,'poc',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring poc'
    status = nf90_get_var(ncid,varid,varx1float)
    if(status /= nf90_noerr) print*,'Error reading poc'
    poc = real(varx1float,kind=r_2)

    status = nf90_inq_varid(ncid,'hoc',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring hoc'
    status = nf90_get_var(ncid,varid,varx1float)
    if(status /= nf90_noerr) print*,'Error reading hoc'
    hoc = real(varx1float,kind=r_2)

    status = nf90_inq_varid(ncid,'roc',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring roc'
    status = nf90_get_var(ncid,varid,varx1float)
    if(status /= nf90_noerr) print*,'Error reading roc'
    roc = real(varx1float,kind=r_2)
    
    status = nf90_inq_varid(ncid,'npp',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring npp'
    status = nf90_get_var(ncid,varid,varx3float)
    if(status /= nf90_noerr) print*,'Error reading npp'
    npp10y = sum(real(varx3float,kind=r_2),dim=2)/10.0  !(mp,ntime)     
    
    status = nf90_inq_varid(ncid,'tsoil',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring tsoil'
    status = nf90_get_var(ncid,varid,varx3float)
    if(status /= nf90_noerr) print*,'Error reading tsoil'
    tsoil10y = sum(real(varx3float,kind=r_2),dim=2)/10.0   !(mp,ntime)
    
    status = nf90_inq_varid(ncid,'moist10',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring moist10'
    status = nf90_get_var(ncid,varid,varx3float)
    if(status /= nf90_noerr) print*,'Error reading moist10'
    moist10y10 = sum(real(varx3float,kind=r_2),dim=2)/10.0  !(mp,ntime)
    
    status = nf90_inq_varid(ncid,'moist100',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring moist100'
    status = nf90_get_var(ncid,varid,varx3float)
    if(status /= nf90_noerr) print*,'Error reading moist100'
    moist10y100= sum(real(varx3float,kind=r_2),dim=2)/10.0 !(mp,ntime)    
    
    ! Close netcdf file
    status = NF90_CLOSE(ncid)    

    ! calculate or assign other inputs/parameters
    do np=1,mp
       ipft = micglobal%pft(np)
       micglobal%poros(np)      = 1.0 - micglobal%bulkd(np)/2650.0
       micparam%siteid(np)      = micglobal%siteid(np)       
       micparam%pft(np)         = micglobal%pft(np) 
       micparam%bgctype(np)     = micglobal%pft(np)
       micparam%isoil(np)       = -1
       micparam%sorder(np)      = -1
       micglobal%area(np)       = 1.0       
   
       micglobal%matpot(np,:,:)  = -100.0    !kPa
      
       micparam%xcnleaf(np)  = cnleaf(ipft)
       micparam%xcnroot(np)  = cnroot(ipft)
       micparam%xcnwood(np)  = cnwood(ipft)
       micparam%fligleaf(np) = ligcleaf(ipft)
       micparam%fligroot(np) = ligcroot(ipft)
       micparam%fligwood(np) = ligcwood(ipft)

       micglobal%dleaf(np,:) = fracleaf(ipft) * npp10y(np,:)
       micglobal%dwood(np,:) = fracwood(ipft) * npp10y(np,:)
       micglobal%droot(np,:) = fracroot(ipft) * npp10y(np,:)

       do ns=1,ms
          if(ns<=3) then
             micparam%csoilobs(np,ns) = soc3(np,ns)
          else
             micparam%csoilobs(np,ns) = soc3(np,3)
          endif
          if(ns==1) then
             micglobal%moist(np,ns,:)    = moist10y10(np,:)
          else
             micglobal%moist(np,ns,:) = moist10y100(np,:)
          endif          
          micglobal%tsoil(np,ns,:)  = tsoil10y(np,:)         
          micparam%csoilobsp(np,ns) = poc(np)   ! assign layer POC conc to all layers
          micparam%csoilobsm(np,ns) = hoc(np)   ! assign layer POC conc to all layers
       enddo
       micglobal%avgts(np)   = sum(micglobal%tsoil(np,1,:))/real(ntime)
       micglobal%avgms(np)   = sum(micglobal%moist(np,2,:))/real(ntime)
       micglobal%npp(np)     = sum(npp10y(np,:))
    enddo

    do k=1,ntime
       micglobal%time(k)= real(k*1.0,kind=r_2)
    enddo

    if(jglobal==1) then
       open(100,file='inputdata.txt')
       do np=1,mp
          write(100,101) micparam%siteid(np),micglobal%area(np),micparam%pft(np), &
          micparam%isoil(np),micparam%sorder(np),micparam%bgctype(np),   &
          micglobal%npp(np),sum(micglobal%dleaf(np,:))+sum(micglobal%dwood(np,:))+sum(micglobal%droot(np,:)), &
          micglobal%ph(np),micglobal%clay(np)+micglobal%silt(np),micglobal%bulkd(np), &
          micglobal%avgts(np),micglobal%avgms(np),sum(micparam%csoilobs(np,:)*zse(:))/sum(zse(:))        
       enddo
       close(100) 
    endif    
101 format(i5,1x,f8.4,1x,4(i3,1x),20(f10.4,1x))
103 format(' run site', 3(i6,1x),10(f10.3,1x))

    deallocate(ivarx1)
    deallocate(varx1float,avgts,avgms,soc3,poc,hoc,roc)
    deallocate(varx2float)
    deallocate(varx3float,npp10y,tsoil10y,moist10y10,moist10y100)
!    print *, 'exit getdata_hwsd'

end subroutine getdata_aust

end module mesc_inout_module

