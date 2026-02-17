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
  SUBROUTINE nc_abort( ok, message )
    USE netcdf
    ! Input arguments
    CHARACTER(LEN=*), INTENT(IN) :: message
    INTEGER, INTENT(IN) :: ok

    WRITE(*,*) message ! error from subroutine
    WRITE(*,*) NF90_STRERROR(ok) ! netcdf error details

    STOP

  END SUBROUTINE nc_abort

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
  subroutine getparam_global(micpxdef)
    use mic_constant
    use mic_variable
    implicit none
    TYPE(mic_param_xscale)    :: micpxdef
    integer npft,ipft,n
    real(r_2), dimension(20)    :: x   ! local variables
    
    open(100,file='parameters_global.csv')
    read(100,*)
    do npft=1,mpft
       read(100,*) ipft, (x(n),n=1,17)


       micpxdef%xav(npft)        = x(1)
       micpxdef%xak(npft)        = x(2)
       micpxdef%xfm(npft)        = x(3)
       micpxdef%xfs(npft)        = x(4)
       micpxdef%xtvmic(npft)     = x(5)
       micpxdef%xtvp(npft)       = x(6)
       micpxdef%xtvc(npft)       = x(7)
       micpxdef%xtvac(npft)      = x(8)
       micpxdef%xkba(npft)       = x(9)
       micpxdef%xqmaxcoeff(npft) = x(10)
       micpxdef%xdiffsoc(npft)   = x(11)
       micpxdef%xnpp(npft)       = x(12)
       micpxdef%xrootbeta(npft)  = x(13)
       micpxdef%xvmaxbeta(npft)  = x(14) 
       ! the following parameters are fixed to 1.0	   
       micpxdef%xfp2ax(npft)     = x(15)
       micpxdef%xbeta(npft)      = x(16)
       micpxdef%xdesorp(npft)    = x(17)   
    enddo
    close(100)
    
!    print *, 'xdesorp=', micpxdef%xdesorp(:)
!    print *, 'xtvc=',    micpxdef%xtvc(npft)
    
  end subroutine getparam_global
 
!> read in global atmospheric C14 data and input data for model run with 14C
!! input 1: file 1 "frac14c" with all observed C14, carbon input, soil properties and other site-sepefici
!!        parameter for model run
!! input 2: file 2" f14c" with atmospheric 14C in five different zones
!! output: all data read in here are written into "micparam", "micinpout" and "micnpool"
!! "fcluster" is not read in yet
!!
  subroutine getdata_c14(frac14c,f14c,micinput,micparam,micnpool,zse)
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
   SUBROUTINE getdata_frc(cfraction,filecluster,jglobal,bgcopt,micinput,micparam,micnpool,zse)
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
    
   end SUBROUTINE getdata_frc

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

   subroutine getdata_hwsd(fhwsdsoc,filecluster,fmodis,jglobal,bgcopt,jopt,jmodel,micparam,micglobal,zse)
    !use micglobal%area (area fraction) as a switch to run for selected sites during parameter optimization (jopt==0)  
    !model only runs for those sites with micglobal%area(np) > 0.0    
    use netcdf
    use mic_constant
    use mic_variable
    implicit none
    character*140 fhwsdsoc,filecluster,fmodis
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
    double precision,  dimension(:),     allocatable     :: varx1db,avgts,avgms
    double precision,  dimension(:,:),   allocatable     :: varx2db,fsoc7
    double precision,  dimension(:,:,:), allocatable     :: tsoil7,moist7,watpot7
    double precision,  dimension(:),     allocatable     :: fald,falo,ffed,ffeo
    double precision,  dimension(:,:),   allocatable     :: modisnpp
    double precision,  dimension(:),     allocatable     :: modisnpp_mp

    allocate(ivarx1(mp),fcluster(mp))
    allocate(varx1float(mp),varx1db(mp),avgts(mp),avgms(mp))
    allocate(varx2db(mp,ntime),fsoc7(mp,7))
    allocate(tsoil7(mp,7,ntime),moist7(mp,7,ntime),watpot7(mp,7,ntime))
    allocate(fald(mp),falo(mp),ffed(mp),ffeo(mp))
    allocate(modisnpp(720,360),modisnpp_mp(mp))
   
   ! open .nc file
    print *, ' calling getdata_hwsd'
    print *,'input file', fhwsdsoc
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
    
    status = nf90_inq_varid(ncid,'bulk_density',varid)
    if(status /= nf90_noerr) print*, 'Error inquiring soil bulk density'
    status = nf90_get_var(ncid,varid,varx1float)
    if(status /= nf90_noerr) print*,'Error reading bulk density'
    micglobal%bulkd= real(varx1float,kind=r_2)

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
    micparam%bgctype=fcluster  
    micglobal%bgctype=fcluster

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
!       micglobal%bgctype(np) = micglobal%sorder(np) 
       micglobal%poros(np)   = 1.0 - micglobal%bulkd(np)/2650.0
       micparam%siteid(np)   = micglobal%siteid(np)       
       micparam%pft(np)      = micglobal%pft(np) 
!       micparam%bgctype(np)  = micglobal%bgctype(np)       
       micparam%isoil(np)    = micglobal%isoil(np)
       micparam%sorder(np)   = micglobal%sorder(np)       
       if(jmodel==1) then      !CABLE 
          ipft =  micglobal%pft(np)      
          micparam%xcnleaf(np)  = cnleaf1(ipft)
          micparam%xcnroot(np)  = cnroot1(ipft)
          micparam%xcnwood(np)  = cnwood1(ipft)
          micparam%fligleaf(np) = ligleaf1(ipft)
          micparam%fligroot(np) = ligroot1(ipft)
          micparam%fligwood(np) = ligwood1(ipft)
       endif
       if(jmodel==2 .or.jmodel==3) then      !ORCHIDEE
          ipft =  micglobal%pft(np)      
          micparam%xcnleaf(np)  = cnleaf2(ipft)
          micparam%xcnroot(np)  = cnroot2(ipft)
          micparam%xcnwood(np)  = cnwood2(ipft)
          micparam%fligleaf(np) = ligleaf2(ipft)
          micparam%fligroot(np) = ligroot2(ipft)
          micparam%fligwood(np) = ligwood2(ipft)       
       endif

       nsocobs=0
       do ns=1,ms
          ! assign the observed layer 1 data to the first 4 modelled layers 
          if(ns<=4) then 
             micparam%csoilobs(np,ns) = real(fsoc7(np,1),kind=r_2)
             micglobal%tsoil(np,ns,:) = real(tsoil7(np,1,:),kind=r_2)
             micglobal%moist(np,ns,:) = real(moist7(np,1,:),kind=r_2)
             micglobal%matpot(np,ns,:)= real(watpot7(np,1,:),kind=r_2)  
          else
             micparam%csoilobs(np,ns) = real(fsoc7(np,ns-3),kind=r_2)
             micglobal%tsoil(np,ns,:) = real(tsoil7(np,ns-3,:),kind=r_2)
             micglobal%moist(np,ns,:) = real(moist7(np,ns-3,:),kind=r_2)
             micglobal%matpot(np,ns,:)= real(watpot7(np,ns-3,:),kind=r_2)  
             ! filter out sites with SOC >12 gc/kg (organic soil: Lourenco ett al. 2022)
             if(micparam%csoilobs(np,ns) >=12.0) then 
                micglobal%area(np) = -1.0
             endif   
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
          micglobal%npp(np),sum(micglobal%dleaf(np,:))+sum(micglobal%dwood(np,:))+sum(micglobal%droot(np,:)), &
          micglobal%ph(np),micglobal%clay(np)+micglobal%silt(np),micglobal%bulkd(np), &
          micglobal%avgts(np),micglobal%avgms(np),sum(micparam%csoilobs(np,:)*zse(:))/sum(zse(:))        
       enddo
       close(100) 
    endif    
101 format(i5,1x,f8.4,1x,4(i3,1x),20(f10.4,1x))
103 format(' run site', 3(i6,1x),10(f10.3,1x))

    deallocate(ivarx1,fcluster)
    deallocate(varx1float,varx1db,avgts,avgms)
    deallocate(varx2db,fsoc7)
    deallocate(tsoil7,moist7,watpot7)    
    deallocate(fald,falo,ffed,ffeo)
!    print *, 'exit getdata_hwsd'

end subroutine getdata_hwsd


SUBROUTINE cluster(filecluster,lat,lon,fcluster)
    ! use the nearest point to estimate the cluster 
    ! output
    ! micparam%bgctype: [1,10]
    use netcdf
    use mic_constant
    use mic_variable
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

    clusterhd = int(varx_flt)
    fcluster = -1
    do np=1,mp
       ilon = int((lon(np) + 179.75)/0.5 +1)
       jlat = int((lat(np) + 89.75)/0.5  +1)
       if(clusterhd(ilon,jlat) < 0 .or. clusterhd(ilon,jlat) >10) then
          freq(1:10) = 0
          do i=max(1,ilon-5),min(720,ilon+5)
          do j=max(1,jlat-5),min(360,jlat+5)
             icluster= clusterhd(i,j)
             if(icluster>0 .and. icluster<11) then
                freq(icluster) = freq(icluster) +1
             endif   
             if(sum(freq(:)) >0) then
                fcluster(np) = maxloc(freq,dim=1)
             endif 
          enddo
          enddo 
!         write(*,101) np,fcluster(np),lon(np),lat(np),ilon,jlat,freq(:)          
       else
          fcluster(np) = clusterhd(ilon,jlat)
       endif   
    enddo    
101 format('freq ',i6,1x,i2,1x,2(f7.3,1x),15(i3,1x))    
END SUBROUTINE cluster


SUBROUTINE screenout(runmodel,jmodel,bgcopt,xopt,cost)
    use mic_constant
    implicit none
    character*10 runmodel
    integer jmodel,bgcopt
    real*8,    dimension(16)           :: xopt
    real*8     cost
    write(*,901) runmodel,jmodel,bgcopt,cost,xopt(1:14)
901 format(a10,2(i3,1x),f12.3,1x,14(f7.3,1x))    

END SUBROUTINE screenout
! ##############mesc_inout.f90###########################
