module mesc_inout_module
  use precision_module, only : dp, sp
  use netcdf, only : nf90_clobber, nf90_close, nf90_create, nf90_def_dim, &
                     nf90_def_var, nf90_enddef, nf90_float, nf90_get_var, &
                     nf90_global, nf90_inq_dimid, nf90_inq_varid, &
                     nf90_inquire_dimension, nf90_max_name, nf90_noerr, &
                     nf90_nowrite, nf90_open, nf90_put_att, nf90_put_var, &
                     nf90_redef, nf90_strerror
  use mic_constant, only : mp, ms, mcpool, nlat, nlon, ntime, mpft, mbgc
  use mic_variable, only : mic_input, mic_parameter, mic_cpool, mic_npool, &
                           mic_global_input, mic_output, mic_param_xscale
  implicit none

  ! All module members are public by default
  public

contains

  !> this part reads restart file that includes all the pool sizes from previous model run
  !! read the C and N pool sizes and assign them to the "miccpool" and "micnpool"
  !! input:  netcdf file frestart_in
  !! output: miccpool and micnppol
  !!
  subroutine vmic_restart_read(miccpool,micnpool,frestart_in)
    type(mic_cpool), intent(inout) :: miccpool    !! carbon pool state (cpool[mp,ms,mcpool] [mg C cm-3])
    type(mic_npool), intent(inout) :: micnpool    !! nitrogen pool state (mineralN[mp,ms] [mg N cm-3])
    character(len=140), intent(in) :: frestart_in !! restart filename

    ! local variables
    integer :: mpx,msx,mcpoolx                  !! array dimensions
    integer :: status,ncid,varid                !! variables for NetCDF reading
    real(dp), dimension(mp,ms,mcpool) :: fcpool !! carbon pools
    real(dp), dimension(mp,ms)        :: fnpool !! nitrogen pools

    ! open restart file
    status = nf90_open(frestart_in,nf90_nowrite,ncid)
    if(status /= nf90_noerr) CALL nc_abort(STATUS, "Error opening "//frestart_in)

    ! get dimensions
    status = nf90_inq_dimid(ncid,"mp",varid)
    if(status /= nf90_noerr) CALL nc_abort(STATUS, "Error inquiring dimensions mp_id")
    status = nf90_inquire_dimension(ncid,varid,len=mpx)
    if(status /= nf90_noerr) CALL nc_abort(STATUS, "Error reading mp")

    status = nf90_inq_dimid(ncid,"ms",varid)
    if(status /= nf90_noerr) CALL nc_abort(STATUS,"Error inquiring dimensions ms_id")
    status = nf90_inquire_dimension(ncid,varid,len=msx)
    if(status /= nf90_noerr) CALL nc_abort(STATUS, "Error reading ms")

    status = nf90_inq_dimid(ncid,"mcpool",varid)
    if(status /= nf90_noerr) CALL nc_abort(STATUS, "Error inquiring dimensions mccpool_id")
    status = nf90_inquire_dimension(ncid,varid,len=mcpoolx)
    if(status /= nf90_noerr) CALL nc_abort(STATUS,"Error reading mcpool")

    ! get variables
    status = nf90_inq_varid(ncid,"mic_cpool",varid)
    if(status /= nf90_noerr) CALL nc_abort(STATUS, "Error inquiring miccpoolc")
    status = nf90_get_var(ncid,varid,fcpool)
    if(status /= nf90_noerr) CALL nc_abort(STATUS,"Error reading fcpool")

    status = nf90_inq_varid(ncid,"mic_npool",varid)
    if(status /= nf90_noerr) CALL nc_abort(STATUS, "Error inquiring micnpoolc")
    status = nf90_get_var(ncid,varid,fnpool)
    if(status /= nf90_noerr) CALL nc_abort(STATUS,"Error reading fnpool")

    ! close the file
    status = NF90_close(ncid)
    if(status /= nf90_noerr) call nc_abort(status, "Error in clsoing netCDF input file")

    ! assign the values from the restart file
    if(mpx/=mp .or. msx/=ms .or. mcpoolx/=mcpool) then
       print *, "dimensions do not match! ", mp,mpx,ms,msx,mcpool,mcpoolx
       STOP
    end if
    miccpool%cpool    = fcpool
    micnpool%mineralN = fnpool

  end subroutine vmic_restart_read

  !> write out model pool sizes into restart file
  !! input: frestart_out
  !! output: miccpool%cpool, micnpool%npool
  !!
  subroutine vmic_restart_write(frestart_out,miccpool,micnpool)
    character(len=99), intent(in)  :: frestart_out !! restart filename
    type(mic_cpool), intent(inout) :: miccpool     !! carbon pool state (cpool[mp,ms,mcpool] [mg C cm-3])
    type(mic_npool), intent(inout) :: micnpool     !! nitrogen pool state (mineralN[mp,ms] [mg N cm-3])

    ! local variables for writing netcdf file
    integer   :: STATUS
    integer   :: FILE_ID, mp_ID, miccarb_ID, soil_ID
    CHARACTER :: CDATE*10
    integer   :: cmic_ID, nmic_ID
    integer   :: values(10)
    real(dp)  :: missreal

    missreal=-1.0e10
    call date_and_time(values=values)
    WRITE(CDATE, '(I4.4,"-",I2.2,"-",I2.2)') values(1),values(2),values(3)

    ! Create NetCDF file:
    STATUS = NF90_create(frestart_out, NF90_CLOBBER, FILE_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, "Error creating restart file ")

    WRITE(*,*) "writing mic restart", frestart_out
    ! Put the file in define mode:
    STATUS = NF90_redef(FILE_ID)

    STATUS = NF90_PUT_ATT( FILE_ID, NF90_GLOBAL, "Valid restart date", CDATE )

    ! Define dimensions:
    ! mp (number of patches)
    STATUS = NF90_def_dim(FILE_ID, "mp"   , mp     , mp_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, "Error defining mp dimension ")

    ! ms: number of soil layers
    STATUS = NF90_DEF_DIM(FILE_ID, "ms", ms, soil_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, "Error defining soil dimension " )

    ! mcpool: number of soil carbon pools
    STATUS = NF90_def_dim(FILE_ID, "mcpool", mcpool, miccarb_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, "Error defining mic_carbon_pools dimension " )

    STATUS = NF90_def_var(FILE_ID,"mic_cpool",NF90_FLOAT,[mp_ID,soil_ID,miccarb_ID],cmic_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, "Error defining mic_cpool variable " )

    STATUS = NF90_def_var(FILE_ID,"mic_npool",NF90_FLOAT,[mp_ID,soil_ID],nmic_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, "Error defining mic_npool variable " )

    ! End define mode:
    STATUS = NF90_enddef(FILE_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, "Error ending define mode " )

    ! PUT VARS
    STATUS = NF90_PUT_VAR(FILE_ID, cmic_ID, REAL(miccpool%cpool, 4) )
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, "Error writing mic_cpool variable " )

    STATUS = NF90_PUT_VAR(FILE_ID, nmic_ID, REAL(micnpool%mineralN, 4) )
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, "Error writing mic_npool variable ")

    ! Close NetCDF file:
    STATUS = NF90_close(FILE_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, "Error closing restart file "  )

    write(*, *) "restart file written to ", frestart_out

  end subroutine vmic_restart_write

  !> abort model run in case when error occurs during reading/writing netcdf file
  !! input integer variable ok
  !! output: character string "message"
  !!
  subroutine nc_abort( ok, message )
    CHARACTER(LEN=*), INTENT(IN) :: message !! Error message
    INTEGER, INTENT(IN) :: ok               !! Error status

    WRITE(*,*) message ! error from subroutine
    WRITE(*,*) NF90_STRERROR(ok) ! netcdf error details

    ERROR STOP

  end subroutine nc_abort

  !> write out fluxes into a netcdf file
  !> input:  micoutput%cinput,micoutput%rsoil,micoutput%cleach in gc/m2/year for each "mp"
  !! output: netcdf foutput
  !! "micinput" not used yet
  !!
  !! NOTE: fluxcinput written to file (=cinputm) may not represent total NPP.
  !!       Total NPP should be the sum of cinputm + cinputs.
  subroutine vmic_output_write(foutput,micinput,micoutput)
    character(len=99), intent(in)    :: foutput    !! Filename to write to
    type(mic_input),   intent(inout) :: micinput   !! environmental and plant inputs per plot (litter, soil state, C input)
    type(mic_output),  intent(inout) :: micoutput  !! output fluxes per plot (cinput, respiration, leaching)
    real(dp)  :: missreal
    integer   :: STATUS
    integer   :: FILE_ID, mp_ID
    CHARACTER :: CDATE*10
    integer   :: cinput_ID, rsoil_ID, cleach_ID
    integer   :: values(10)

    missreal=-1.0e10
    call date_and_time(values=values)
    WRITE(CDATE, '(I4.4,"-",I2.2,"-",I2.2)') values(1),values(2),values(3)
    ! Create NetCDF file:
    STATUS = NF90_create(foutput, NF90_CLOBBER, FILE_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, "Error creating output file ")

    WRITE(*,*) "writing output file", foutput
    print *, CDATE

    ! Put the file in define mode:
    STATUS = NF90_redef(FILE_ID)

    STATUS = NF90_PUT_ATT( FILE_ID, NF90_GLOBAL, "Valid output date", CDATE  )

    ! Define dimensions:
    ! mp (number of patches)
    STATUS = NF90_def_dim(FILE_ID, "mp"   , mp     , mp_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, "Error defining mp dimension ")

    STATUS = NF90_def_var(FILE_ID,"Cinput",NF90_FLOAT,[mp_ID],cinput_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, "Error defining NPP " )


    STATUS = NF90_def_var(FILE_ID,"rsoil",NF90_FLOAT,[mp_ID],rsoil_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, "Error defining rsoil " )


    STATUS = NF90_def_var(FILE_ID,"Cleach",NF90_FLOAT,[mp_ID],cleach_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, "Error defining cleach " )

    ! End define mode:
    STATUS = NF90_enddef(FILE_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, "Error ending define mode " )

    ! put attributes
    STATUS = NF90_PUT_ATT(FILE_ID,cinput_ID,"unit","g C m-2 year-1")
    STATUS = NF90_PUT_ATT(FILE_ID,cinput_ID,"missing_value", real(missreal,4))

    STATUS = NF90_PUT_ATT(FILE_ID,rsoil_ID,"unit","g C m-2 year-1")
    STATUS = NF90_PUT_ATT(FILE_ID,rsoil_ID,"missing_value", real(missreal,4))

    STATUS = NF90_PUT_ATT(FILE_ID,cleach_ID,"unit","g C m-2 year-1")
    STATUS = NF90_PUT_ATT(FILE_ID,cleach_ID,"missing_value", real(missreal,4))

    ! PUT VARS
    STATUS = NF90_PUT_VAR(FILE_ID, cinput_ID, REAL(micoutput%fluxcinput,4) )
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, "Error writing NPP " )

    STATUS = NF90_PUT_VAR(FILE_ID, rsoil_ID, REAL(micoutput%fluxrsoil,4) )
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, "Error writing Rsoil ")

    STATUS = NF90_PUT_VAR(FILE_ID, cleach_ID, REAL(micoutput%fluxcleach,4) )
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, "Error writing Cleach ")

    ! Close NetCDF file:
    STATUS = NF90_close(FILE_ID)
    IF(STATUS /= NF90_NOERR) CALL nc_abort(STATUS, "Error closing restart file "  )

    write(*, *) "output written to ", foutput

  end subroutine vmic_output_write

  !> Get PFT-dependent model parameter values (up to 20 parameters)
  subroutine getparam_global(fglobalparam,jmodel,micpxdef)
    use mic_constant, only : xrootcable, xrootorchidee

    character(len=140),     intent(in)    :: fglobalparam
      !! Parameter filename (currently hard-coded to "parameters_global.csv")
    integer,                intent(in)    :: jmodel
      !! Code for land surface model (1=CABLE, 2=ORCHIDEE, 3=ORCHIDEE + modis_npp)
    TYPE(mic_param_xscale), intent(inout) :: micpxdef
      !! Object holding parameter values

    ! Local variables
    integer                 :: ibgc,ipft,n
    real(dp), dimension(14) :: x

    open(100,file=fglobalparam)
    read(100,*)
    do ibgc=1,mbgc
       read(100,*) ipft, (x(n),n=1,14)
       ! ensure x(1:16) are consistent with "vmic_param_xscale"
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
    end do
    close(100)

    do ipft=1,mpft
       if (jmodel==1) then
          micpxdef%xrootbeta(ipft) = xrootcable(ipft)
       else if (jmodel==2 .or. jmodel==3) then
          micpxdef%xrootbeta(ipft) = xrootorchidee(ipft)
       end if
    end do
  end subroutine getparam_global

  !> get number of patches
  !! input: hartd-wired parameter filename "fpatch"
  !! output: write the parameter values to "mpx"
  !!
  !! reads in global patch area fraction and calculates the number of land cells using sum(PFTfrac(lon,lat,pft))
  subroutine getpatch_global(fpatch,jmodel,mpx)
    character(len=140), intent(in)    :: fpatch
      !! Patch area fraction filename (PFTfrac for CABLE, maxvegetfrac for ORCHIDEE)
    integer,            intent(in)    :: jmodel
      !! Land-surface model code (1=CABLE, 2=ORCHIDEE, 3=ORCHIDEE+modis_npp)
    integer,            intent(inout) :: mpx
      !! number of land cells (patches) with non-zero vegetation fraction

    ! Local variables
    real(dp), dimension(:,:,:),   allocatable :: xfield3 !! 3D field to read into
    real(sp), dimension(:,:,:,:), allocatable :: xfield4 !! 4D field to read into
    integer :: i,j,np,maxpft
    integer :: ncid1,ok,varid !! Variables for handling NetCDF files

    print *, "patch filename", fpatch
    select case (jmodel)

    case (1)
      allocate(xfield3(nlon,nlat,17))

      ok = NF90_OPEN(fpatch,0,ncid1)
      IF (ok /= NF90_NOERR) CALL nc_abort(ok,"Error opening file"//fpatch)
      ok = NF90_INQ_VARID(ncid1,"PFTfrac",varid)
      ok = NF90_GET_VAR(ncid1,varid,xfield3)
      ok = NF90_close(ncid1)

      xfield3 = max(0.0,xfield3)
      np=0
      do i=1,nlon
      do j=1,nlat
         if(sum(xfield3(i,j,:))>0.9) then
            maxpft= maxloc(xfield3(i,j,:),dim=1)
            if(maxpft >0 .and. maxpft <14) np=np+1
         end if
      end do
      end do

      deallocate(xfield3)

    case (2)
       allocate(xfield4(nlon,nlat,19,1))
       ok = NF90_OPEN(fpatch,0,ncid1)
       IF (ok /= NF90_NOERR) CALL nc_abort(ok,"Error opening file"//fpatch)
       ok = NF90_INQ_VARID(ncid1,"maxvegetfrac",varid)
       ok = NF90_GET_VAR(ncid1,varid,xfield4)
       ok = NF90_close(ncid1)

       xfield4 = max(0.0,xfield4)
       np=0
       do i=1,nlon
       do j=1,nlat
          if(sum(xfield4(i,j,:,1))>0.9) then
             maxpft= maxloc(xfield4(i,j,:,1),dim=1)
             if(maxpft >0 .and. maxpft <=mpft) np=np+1
          end if
       end do
       end do
     deallocate(xfield4)

     case default
     write(6,"(a,i0,a)") "ERROR getpatch_global: Invalid model '", jmodel, "'"
     stop 999

    end select

    mpx = np
  end subroutine getpatch_global

  !> Get global forcing from CABLE, averaging for each land cell using PFTfrac
  subroutine getdata_global4_cable(fglobal,jglobal,bgcopt,jopt,jmodel,micglobal,micparam,zse)
  use mic_constant, only : cnleaf1, cnroot1, cnwood1, ligleaf1, ligroot1, ligwood1

    character(len=140),     INTENT(IN)    :: fglobal(10)
      !! Parameter filename (currently hard-coded to "fglobal_cable")
    TYPE(mic_global_input), INTENT(INOUT) :: micglobal
      !! Object holding global-scale parameters
    TYPE(mic_parameter),    INTENT(INOUT) :: micparam
      !! Object holding model parameters per plot and soil layer
    logical,                INTENT(IN)    :: jglobal
      !! Flag indicating whether this is a global simulation
    integer,                INTENT(IN)    :: bgcopt
      !! Biogeochemical type to simulate
    logical,                INTENT(IN)    :: jopt
      !! Flag indicating whether to optimize parameters or use a lookup table
    integer,                INTENT(IN)    :: jmodel
      !! Land-surface model code (1=CABLE, 2=ORCHIDEE, 3=ORCHIDEE+modis_npp)
    real(dp),               INTENT(IN)    :: zse(ms)
      !! Soil layer thickness (m)

    ! local variables
    real(dp), dimension(nlon)           :: lon
    real(dp), dimension(nlat)           :: lat
    real(dp), dimension(ntime)          :: time
    real(dp), dimension(nlon,nlat,mpft) :: patchfrac
    integer :: ncid1,ncid3,ok,lonid,latid,timeid,varid,n,np,ns
    integer :: i,j,k,npx,isoilx,sorderx
    integer, dimension(:),        allocatable :: ilon,jlat, fcluster
    integer, dimension(:,:),      allocatable :: varx2_int
    real(dp), dimension(:),       allocatable :: varmp1_db
    real(sp), dimension(:,:),     allocatable :: varx2_flt
    real(dp), dimension(:,:),     allocatable :: varx2_db,varmp2_db
    real(dp), dimension(:,:,:),   allocatable :: varx3time_db,varx3ms_db,varx3ms5_db
    real(dp), dimension(:,:,:),   allocatable :: varx3_db,varmp3_db,varsoc3_db,varbulk_db,varaoc_db
    real(dp), dimension(:,:,:,:), allocatable :: varx4_db
    real(dp), dimension(:),       allocatable :: falo,fald,ffeo,ffed
    integer  :: maxpft,pft, msite,sitemax,intval,isite
    real(dp) :: bulkd2


    allocate(ilon(mp),jlat(mp),fcluster(mp))
    allocate(varx2_int(nlon,nlat),varx2_flt(nlon,nlat))
    allocate(varmp1_db(mp))
    allocate(varx2_db(nlon,nlat),varmp2_db(mp,ntime))
    allocate(varx3time_db(nlon,nlat,ntime),varx3ms_db(nlon,nlat,ms),varx3ms5_db(nlon,nlat,5))
    allocate(varx3_db(nlon,nlat,mpft),varmp3_db(mp,ms,ntime))
    allocate(varsoc3_db(nlon,nlat,ms),varbulk_db(nlon,nlat,ms),varaoc_db(nlon,nlat,ms))
    allocate(varx4_db(nlon,nlat,ms,ntime))
    allocate(falo(mp),fald(mp),ffeo(mp),ffed(mp))


  ! file 7: with aoc fraction
    ok = NF90_OPEN(fglobal(7),0,ncid1)
    IF (ok /= NF90_NOERR) CALL nc_abort(ok,"Error opening file"//fglobal(7))
    print *, "global input1 = ", fglobal(7)
    ok = NF90_INQ_VARID(ncid1,"aoc_fraction",varid)
    ok = NF90_GET_VAR(ncid1,varid,varaoc_db)
    ok = NF90_close(ncid1)


  ! file 1: time-invarying data
    ok = NF90_OPEN(fglobal(1),0,ncid3)
    IF (ok /= NF90_NOERR) CALL nc_abort(ok,"Error opening file"//fglobal(1))
    print *, "global input1 = ", fglobal(1)

    ok = NF90_INQ_VARID(ncid3,"lon",lonid)
    ok = NF90_GET_VAR(ncid3,lonid,lon)

    ok = NF90_INQ_VARID(ncid3,"lat",latid)
    ok = NF90_GET_VAR(ncid3,latid,lat)

    ok = NF90_INQ_VARID(ncid3,"PFTfrac",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3_db)
    patchfrac(:,:,:) = real(varx3_db(:,:,:),kind=dp)

    ok = NF90_INQ_VARID(ncid3,"HWSD_SOC",varid)
    ok = NF90_GET_VAR(ncid3,varid,varsoc3_db)

    ok = NF90_INQ_VARID(ncid3,"HWSD_bulk_density",varid)
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
             micglobal%lon(np)         = lon(i)
             micglobal%lat(np)         = lat(j)
             micparam%csoilobs(np,:)   = real(varsoc3_db(i,j,:),kind=dp)
             bulkd2                    = ( varbulk_db(i,j,1)*dble(zse(1))+varbulk_db(i,j,2)*dble(zse(2))+varbulk_db(i,j,3)*dble(zse(3)) )&
                                         /sum(dble(zse(1:3)))
             micglobal%bulkd(np)       = max(500.0_dp,min(1800.0_dp,real(bulkd2,kind=dp)))
             micglobal%patchfrac(np,:) = patchfrac(i,j,:)
             micglobal%pft(np)         = maxpft
             micparam%fracaoc(np,:)    = max(0.0_dp,min(0.7_dp,real(varaoc_db(i,j,:),kind=dp)))
          end if
       end if
    end do
    end do

    if(np/=mp) then
      print *, "np is not equal to mp", np,mp
      STOP
    end if


    ok = NF90_INQ_VARID(ncid3,"area",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_flt)
    varx2_db = real(varx2_flt,kind=8)
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    micglobal%area(:)=max(0.0, real(varmp1_db(:),kind=dp)) *(1.0e-12)

    ok = NF90_INQ_VARID(ncid3,"SoilOrder",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_db)
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    micglobal%sorder(:) = int(varmp1_db(:))

    ok = NF90_INQ_VARID(ncid3,"isoil",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_db)
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    micglobal%isoil(:) = int(varmp1_db(:))

    ok = NF90_INQ_VARID(ncid3,"npp",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3_db)
    varx3_db = max(0.0,varx3_db)
    call lonlat2mpx3(ilon,jlat,patchfrac,varx3_db,varmp1_db)
    micglobal%npp = real(varmp1_db,kind=dp)
    micglobal%npp = max(100.0,micglobal%npp)
    print *, "npp", maxval(micglobal%npp), minval(micglobal%npp),sum(micglobal%npp)/real(mp)

    ok = NF90_INQ_VARID(ncid3,"lignin_CWD",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3_db)
    call lonlat2mpx3(ilon,jlat,patchfrac,varx3_db,varmp1_db)
    micglobal%ligwood = real(varmp1_db,kind=dp)
    print *, "ligwood", maxval(micglobal%ligwood), minval(micglobal%ligwood),sum(micglobal%ligwood)/real(mp)

    ok = NF90_INQ_VARID(ncid3,"lignin_leaf",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3_db)
    call lonlat2mpx3(ilon,jlat,patchfrac,varx3_db,varmp1_db)
    micglobal%ligleaf = real(varmp1_db,kind=dp)
    print *, "ligleaf", maxval(micglobal%ligleaf), minval(micglobal%ligleaf),sum(micglobal%ligleaf)/real(mp)

    ok = NF90_INQ_VARID(ncid3,"lignin_root",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3_db)
    call lonlat2mpx3(ilon,jlat,patchfrac,varx3_db,varmp1_db)
    micglobal%ligroot = real(varmp1_db,kind=dp)
    print *, "ligroot", maxval(micglobal%ligroot), minval(micglobal%ligroot),sum(micglobal%ligroot)/real(mp)

    ok = NF90_INQ_VARID(ncid3,"CN_ratio_leaf",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3_db)
    call lonlat2mpx3(ilon,jlat,patchfrac,varx3_db,varmp1_db)
    micglobal%cnleaf = real(spread(varmp1_db,dim=2,ncopies=ntime),kind=dp)
    print *, "cnleaf", maxval(micglobal%cnleaf), minval(micglobal%cnleaf),sum(micglobal%cnleaf)/real(size(micglobal%cnleaf))

    ok = NF90_INQ_VARID(ncid3,"CN_ratio_noleaf",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3_db)
    call lonlat2mpx3(ilon,jlat,patchfrac,varx3_db,varmp1_db)
    micglobal%cnwood = real(spread(varmp1_db,dim=2,ncopies=ntime),kind=dp)
    print *, "cnwood", maxval(micglobal%cnwood), minval(micglobal%cnwood),sum(micglobal%cnwood)/real(size(micglobal%cnwood))

    ok = NF90_INQ_VARID(ncid3,"CN_ratio_belowground",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3_db)
    call lonlat2mpx3(ilon,jlat,patchfrac,varx3_db,varmp1_db)
    micglobal%cnroot = real(spread(varmp1_db,dim=2,ncopies=ntime),kind=dp)
    print *, "cnroot", maxval(micglobal%cnroot), minval(micglobal%cnroot),sum(micglobal%cnroot)/real(size(micglobal%cnroot))

    ok = NF90_close(ncid3)

    ! read in the HWSD soil properties and soil cluster
    ok = NF90_OPEN(fglobal(3),0,ncid3)
    IF (ok /= NF90_NOERR) CALL nc_abort(ok,"Error opening file"//fglobal(3))
    print *, "global input1 = ", fglobal(3)

    ok = NF90_INQ_VARID(ncid3,"Bulk_density",varid)
    ok = NF90_GET_VAR(ncid3,varid,varbulk_db)

    ok = NF90_INQ_VARID(ncid3,"Clay_fraction",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3ms_db)
    call lonlat2mpx3a(ilon, jlat, 3, dble(zse(1:3)), varbulk_db(:,:,1:3), varx3ms_db(:,:,1:3), varmp1_db)
    micglobal%clay(:) = real(varmp1_db(:)*0.01,kind=dp)
    print *, "clay", maxval(micglobal%clay), minval(micglobal%clay),sum(micglobal%clay)/real(mp)

    ok = NF90_INQ_VARID(ncid3,"Silt_fraction",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3ms_db)
    call lonlat2mpx3a(ilon, jlat, 3, dble(zse(1:3)), varbulk_db(:,:,1:3), varx3ms_db(:,:,1:3), varmp1_db)
    micglobal%silt(:) = real(varmp1_db(:)*0.01,kind=dp)
    print *, "silt", maxval(micglobal%silt), minval(micglobal%silt),sum(micglobal%silt)/real(mp)

    ok = NF90_INQ_VARID(ncid3,"ph",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3ms_db)
    call lonlat2mpx3a(ilon, jlat, 3, dble(zse(1:3)), varbulk_db(:,:,1:3), varx3ms_db(:,:,1:3), varmp1_db)
    micglobal%ph = real(varmp1_db,kind=dp)
    micglobal%ph =min(9.5,max(3.5,micglobal%ph))
    print *, "ph", maxval(micglobal%ph), minval(micglobal%ph),sum(micglobal%ph)/real(mp)

    ok = NF90_INQ_VARID(ncid3,"Ald",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3ms5_db)
    call lonlat2mpx3a(ilon, jlat, 3, dble(zse(1:3)), varbulk_db(:,:,1:3), varx3ms5_db(:,:,1:3), varmp1_db)
    fald = real(varmp1_db,kind=dp)
    print *, "Ald", maxval(fald), minval(fald),sum(fald)/real(mp)

    ok = NF90_INQ_VARID(ncid3,"Alo",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3ms5_db)
    call lonlat2mpx3a(ilon, jlat, 3, dble(zse(1:3)), varbulk_db(:,:,1:3), varx3ms5_db(:,:,1:3), varmp1_db)
    falo = real(varmp1_db,kind=dp)
    print *, "Alo", maxval(falo), minval(falo),sum(falo)/real(mp)

    ok = NF90_INQ_VARID(ncid3,"Fed",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3ms5_db)
    call lonlat2mpx3a(ilon, jlat, 3, dble(zse(1:3)), varbulk_db(:,:,1:3), varx3ms5_db(:,:,1:3), varmp1_db)
    ffed = real(varmp1_db,kind=dp)
    print *, "fed", maxval(ffed), minval(ffed),sum(ffed)/real(mp)

    ok = NF90_INQ_VARID(ncid3,"Feo",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3ms5_db)
    call lonlat2mpx3a(ilon, jlat, 3, dble(zse(1:3)), varbulk_db(:,:,1:3), varx3ms5_db(:,:,1:3), varmp1_db)
    ffeo = real(varmp1_db,kind=dp)
    print *, "feo", maxval(ffeo), minval(ffeo),sum(ffeo)/real(mp)

    ok = NF90_INQ_VARID(ncid3,"Cluster",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_int)
    call lonlat2mpx2int(ilon, jlat, varx2_int, fcluster)
    print *, "Cluster", maxval(fcluster), minval(fcluster)

    micparam%bgctype =fcluster
    micglobal%bgctype=fcluster
    ok = NF90_close(ncid3)

    ! check the time-invariant data and replace bad values withy default values
    do np=1,mp
       pft = micglobal%pft(np)
       if(min(micglobal%ligwood(np),micglobal%ligleaf(np),micglobal%ligroot(np))<0.0 .or. &
          max(micglobal%ligwood(np),micglobal%ligleaf(np),micglobal%ligroot(np))>1.0) then
          micglobal%ligleaf(np) = ligleaf1(pft)
          micglobal%ligwood(np) = ligwood1(pft)
          micglobal%ligroot(np) = ligroot1(pft)
       end if

       if(min(micglobal%cnleaf(np,1),micglobal%cnwood(np,1),micglobal%cnroot(np,1))<10.0 .or. &
          max(micglobal%cnleaf(np,1),micglobal%cnwood(np,1),micglobal%cnroot(np,1))>1000.0) then
          micglobal%cnleaf(np,:) = cnleaf1(pft)
          micglobal%cnwood(np,:) = cnwood1(pft)
          micglobal%cnroot(np,:) = cnroot1(pft)
       end if
       ! replacing negative values of metal oxide with their global means in kg/m2
     !  if(fald(np)<0.0) fald(np) =0.46_dp
     !  if(falo(np)<0.0) falo(np) =0.39_dp
     !  if(ffed(np)<0.0) ffed(np) =2.74_dp
     !  if(ffeo(np)<0.0) ffeo(np) =3.53_dp
       micparam%siteid(np)       = np
       micglobal%poros(:)  = 1.0 - micglobal%bulkd(:)/2650.0
       ! replace "NaN" with -1 for soil pH and clay and silt fractions
       if(micglobal%ph(np) /= micglobal%ph(np)) micglobal%ph(np)=-1
       if(micglobal%silt(np) /= micglobal%silt(np)) micglobal%silt(np)=-1.0
       if(micglobal%clay(np) /= micglobal%clay(np)) micglobal%clay(np)=-1.0
    end do
    ! call "cluster_hwsd" to use ORCHIDEE centroid
    micglobal%bgctype = -1
    micparam%bgctype  = -1
    call cluster_hwsd(2,micglobal%bgctype,micparam%csoilobs,micglobal%clay,micglobal%silt,micglobal%ph,fald,falo,ffed,ffeo,fcluster)
    micparam%bgctype =fcluster
    micglobal%bgctype=fcluster

    ! reading time-varying data
    ! temporary solution
    do n=1,ntime
       micglobal%time(n) = n
    end do

    print *, "reading time-varying data", fglobal(2)

  ! file 2: daily aboveground leaf fall (g C/m2/day)     ! Open netcdf file
    ok = NF90_OPEN(fglobal(2),0,ncid3)
    IF (ok /= NF90_NOERR) CALL nc_abort(ok,"Error opening file"//fglobal(2))

    ok = NF90_INQ_VARID(ncid3,"Leaf_fall",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3time_db)
    varx3time_db = max(0.0, varx3time_db)
    call lonlat2mpx3time(ilon,jlat,varx3time_db,varmp2_db)
    micglobal%dleaf = real(varmp2_db,kind=dp)
    print *, "dleaf", minval(micglobal%dleaf),maxval(micglobal%dleaf), &
                      sum(micglobal%dleaf)/real(size(micglobal%dleaf))

    ok = NF90_INQ_VARID(ncid3,"non_leaf_aboveground_litterfall",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3time_db)
    varx3time_db = max(0.0, varx3time_db)
    call lonlat2mpx3time(ilon,jlat,varx3time_db,varmp2_db)
    micglobal%dwood = real(varmp2_db,kind=dp)
    print *, "dwood", minval(micglobal%dwood),maxval(micglobal%dwood), &
                      sum(micglobal%dwood)/real(size(micglobal%dwood))

    ok = NF90_INQ_VARID(ncid3,"Belowground_litter_fall",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3time_db)
    varx3time_db = max(0.0, varx3time_db)
    call lonlat2mpx3time(ilon,jlat,varx3time_db,varmp2_db)
    micglobal%droot = real(varmp2_db,kind=dp)
    print *, "droot", minval(micglobal%droot),maxval(micglobal%droot), &
                      sum(micglobal%droot)/real(size(micglobal%droot))

    ok = NF90_INQ_VARID(ncid3,"SoilTemp",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx4_db)
    call lonlat2mpx4b(ilon,jlat,-100.0d0,50.0d0,0.0d0,varx4_db,varmp3_db)
    micglobal%tsoil = real(varmp3_db,kind=dp)
    print *, "tsoil", minval(micglobal%tsoil),maxval(micglobal%tsoil), &
                      sum(micglobal%tsoil)/real(size(micglobal%tsoil))

    ok = NF90_INQ_VARID(ncid3,"SoilMoist",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx4_db)
    call lonlat2mpx4b(ilon,jlat,0.0d0,0.8d0,0.15d0,varx4_db,varmp3_db)
    micglobal%moist = real(varmp3_db,kind=dp)
    print *, "moist", minval(micglobal%moist),maxval(micglobal%moist), &
                      sum(micglobal%moist)/real(size(micglobal%moist))

    ok = NF90_INQ_VARID(ncid3,"water_potential",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx4_db)
    call lonlat2mpx4b(ilon,jlat,-1000.0d0,0.0d0,-100.0d0,varx4_db,varmp3_db)
    micglobal%matpot = real(varmp3_db,kind=dp)
    print *, "matpot", minval(micglobal%matpot),maxval(micglobal%matpot), &
                       sum(micglobal%matpot)/real(size(micglobal%matpot))

    ok = NF90_close(ncid3)


    ! filter out land cells with "bgctype<0"
  !  print *, 'calculations are not done for the following cells'

    msite = 0
    do np=1,mp
       if(micparam%bgctype(np) <1 .or. micparam%bgctype(np) >mbgc &
         .or. minval(micparam%csoilobs(np,:)) < 0.0               &
         .or. maxval(micparam%csoilobs(np,:)) > 120.0) then
  !        print *, np, micparam%bgctype(np),micglobal%area(np),micglobal%isoil(np), &
  !                 micglobal%sorder(np),micglobal%bgctype(np), micglobal%npp(np)
          micparam%bgctype(np)= mbgc
          micglobal%area(np)  = -1.0
       end if
       ! replacing NPP in the time-invariant input file using the mean of time-varying input
       micglobal%npp(np) = 365.0 * sum(micglobal%dleaf(np,:) + micglobal%dwood(np,:) + micglobal%droot(np,:)) &
                         /real(size(micglobal%dleaf(np,:)))
       if(micglobal%isoil(np) <0  .or. micglobal%isoil(np) >12)   micglobal%isoil(np)=12
       if(micglobal%sorder(np) <0 .or. micglobal%sorder(np) >12)  micglobal%sorder(np)=12
       if(micglobal%bgctype(np) ==bgcopt .and. micglobal%area(np) >0) msite = msite + 1
    end do

    ! assign time-invariance properties from "micglobal" to "micparam"
    micparam%pft        = micglobal%pft
    micparam%isoil      = micglobal%isoil
    micparam%sorder     = micglobal%sorder
    micparam%fligleaf   = micglobal%ligleaf
    micparam%fligroot   = micglobal%ligroot
    micparam%fligwood   = micglobal%ligwood
    micparam%xcnleaf(:) = micglobal%cnleaf(:,1)
    micparam%xcnroot(:) = micglobal%cnroot(:,1)
    micparam%xcnwood(:) = micglobal%cnwood(:,1)

    sitemax=1000
    if(msite>2*sitemax) then

       intval = msite/sitemax; isite=0
       do np=1,mp
          if(micglobal%bgctype(np) == bgcopt .and.micglobal%area(np) > 0.0) then
             isite = isite +1
             if(int(isite/intval)*intval /= isite.or. isite>sitemax*intval) micglobal%area(np) = -1.0
          end if
 !         if(micglobal%area(np) > 0.0 .and. micglobal%bgctype(np) == bgcopt) then
 !            write(*,103) isite,np, micglobal%bgctype(np), micglobal%area(np),micglobal%npp(np),micglobal%ph(np)
 !         endif
       end do
    else

      isite=0
      do np=1,mp
         if(micglobal%area(np) > 0.0 .and. micglobal%bgctype(np) == bgcopt) then
            isite=isite+1
 !           write(*,103) isite,np,micglobal%bgctype(np),micglobal%area(np),micglobal%npp(np),micglobal%ph(np)
         end if
      end do
      if(isite<10) print *, "too few sites ", isite

    end if

    micglobal%avgts(:) = sum(sum(micglobal%tsoil(:,:,:),dim=3),dim=2)/real(ms*ntime)
    micglobal%avgms(:) = sum(sum(micglobal%moist(:,:,:),dim=3),dim=2)/real(ms*ntime)

! write out time-invariant input data
    if(jglobal) then
       open(31,file=fglobal(5))
       do np=1,mp
          write(31,101) micglobal%lon(np),micglobal%lat(np),ilon(np),jlat(np),                 &
          micparam%siteid(np),micglobal%area(np),micparam%pft(np),                             &
          micparam%isoil(np),micparam%sorder(np),micparam%bgctype(np),fcluster(np), micglobal%npp(np),       &
          minval(micglobal%dleaf(np,:) + micglobal%dwood(np,:) + micglobal%droot(np,:))*365.0, &
          maxval(micglobal%dleaf(np,:) + micglobal%dwood(np,:) + micglobal%droot(np,:))*365.0, &
          micglobal%ph(np),micglobal%clay(np)*100.0,micglobal%silt(np)*100.0,                  &
          fald(np),falo(np),ffed(np),ffeo(np), micglobal%bulkd(np),                            &
          micglobal%avgts(np),micglobal%avgms(np), max(-1.0,micparam%csoilobs(np,:)),          &
          micparam%fracaoc(np,1),micparam%fracaoc(np,3), micparam%fracaoc(np,ms)

       end do
       close(31)
    end if
101 format(2(f8.3,1x),2(i6,1x),i10,1x,es16.8,1x,5(i5,1x),*(es16.8,1x))
103 format(3(i5,1x),3(f10.4,1x))

    deallocate(ilon,jlat,fcluster)
    deallocate(varx2_int,varx2_flt)
    deallocate(varmp1_db)
    deallocate(varx3time_db,varx3ms_db,varx3ms5_db)
    deallocate(varx2_db,varmp2_db)
    deallocate(varx3_db,varmp3_db)
    deallocate(varsoc3_db,varbulk_db,varaoc_db)
    deallocate(varx4_db)
    deallocate(falo,fald,ffeo,ffed)


  end subroutine getdata_global4_cable

  !> Get global forcing from ORCHIDEE, averaging for each land cell using PFTfrac
  subroutine getdata_global4_orchidee(fglobal,jglobal,bgcopt,jopt,jmodel,micglobal,micparam,zse)
  use mic_constant, only : cnleaf2, cnroot2, cnwood2, ligleaf2, ligroot2, ligwood2

    character(len=140),     INTENT(IN)    :: fglobal(10)
      !! Parameter filename (currently hard-coded to "fglobal_cable")
    TYPE(mic_global_input), INTENT(INOUT) :: micglobal
      !! Object holding global-scale parameters
    TYPE(mic_parameter),    INTENT(INOUT) :: micparam
      !! Object holding model parameters per plot and soil layer
    logical,                INTENT(IN)    :: jglobal
      !! Flag indicating whether this is a global simulation
    integer,                INTENT(IN)    :: bgcopt
      !! Biogeochemical type to simulate
    logical,                INTENT(IN)    :: jopt
      !! Flag indicating whether to optimize parameters or use a lookup table
    integer,                INTENT(IN)    :: jmodel
      !! Land-surface model code (1=CABLE, 2=ORCHIDEE, 3=ORCHIDEE+modis_npp)
    real(dp),               INTENT(IN)    :: zse(ms)
      !! Soil layer thickness (m)

    ! local variables
    real(dp), dimension(nlon)           :: lon
    real(dp), dimension(nlat)           :: lat
    real(sp),    dimension(nlon)        :: lon_flt
    real(sp),    dimension(nlat)        :: lat_flt
    real(dp), dimension(ntime)          :: time
    real(dp), dimension(nlon,nlat,mpft) :: patchfrac
    integer :: ncid1,ncid3,ok,lonid,latid,timeid,varid,n,np,ns
    integer :: i,j,k,npx,isoilx,sorderx,ilonx,jlatx
    integer, dimension(:),        allocatable :: ilon,jlat, fcluster
    integer, dimension(:,:),      allocatable :: varx2_int
    real(sp), dimension(:,:),     allocatable :: varx2_flt
    real(sp), dimension(:,:,:,:), allocatable :: varx4_flt
    real(dp), dimension(:),       allocatable :: varmp1_db
    real(dp), dimension(:,:),     allocatable :: varx2_db,varmp2_db
    real(dp), dimension(:,:,:),   allocatable :: varx3time_db,varx3ms_db,varx3ms5_db
    real(dp), dimension(:,:,:),   allocatable :: varx3_db,varmp3_db,varsoc3_db,varbulk_db,varaoc_db
    real(dp), dimension(:,:,:,:), allocatable :: varx4_db
    real(dp), dimension(:),       allocatable :: falo,fald,ffeo,ffed
    real(dp), dimension(:,:),     allocatable :: modisnpp
    real(dp), dimension(:),       allocatable :: modisnpp_mp
    integer  :: maxpft,pft, msite,sitemax,intval,isite
    real(dp) :: bulkd2

    ! data
    real(sp), dimension(12)    :: sandx,clayx,siltx,porex,bulkdx,fcpx,wiltx
    data sandx/0.93,0.81,0.63,0.17,0.06,0.40,0.54,0.08,0.30,0.48,0.06,0.15/
    data clayx/0.03,0.06,0.11,0.19,0.10,0.20,0.27,0.33,0.33,0.41,0.46,0.55/
    data siltx/0.04,0.13,0.26,0.64,0.84,0.40,0.19,0.59,0.37,0.11,0.48,0.30/
    data porex/0.43,0.41,0.41,0.45,0.46,0.43,0.39,0.43,0.41,0.38,0.36,0.38/
    data bulkdx/1510.5,1563.5,1563.5,1457.5,1431.0,1510.5,1616.5,1510.5,1563.5,1643.0,1696.0,1643.0/
    data fcpx/0.0493,0.0710,0.1218,0.2402,0.2582,0.1654,0.1695,0.3383,0.2697,0.2672,0.337,0.3469/
    data wiltx/0.0450,0.0570,0.0657,0.1039,0.0901,0.0884,0.1112,0.1967,0.1496,0.1704,0.2665,0.2707/

    allocate(ilon(mp),jlat(mp),fcluster(mp))
    allocate(varx2_int(nlon,nlat),varx2_flt(nlon,nlat))
    allocate(varx4_flt(nlon,nlat,mpft,1))
    allocate(varx2_db(nlon,nlat))
    allocate(varx3time_db(nlon,nlat,ntime),varx3ms_db(nlon,nlat,ms),varx3ms5_db(nlon,nlat,5))
    allocate(varx3_db(nlon,nlat,mpft),varsoc3_db(nlon,nlat,ms),varbulk_db(nlon,nlat,ms),varaoc_db(nlon,nlat,ms))
    allocate(varx4_db(nlon,nlat,ms,ntime))

    allocate(varmp1_db(mp))
    allocate(varmp2_db(mp,ntime))
    allocate(varmp3_db(mp,ms,ntime))

    allocate(falo(mp),fald(mp),ffeo(mp),ffed(mp))
    allocate(modisnpp(nlon,nlat),modisnpp_mp(mp))

  ! file 7: with aoc fraction
    ok = NF90_OPEN(fglobal(7),0,ncid1)
    IF (ok /= NF90_NOERR) CALL nc_abort(ok,"Error opening file"//fglobal(7))
    print *, "global input1 = ", fglobal(7)
    ok = NF90_INQ_VARID(ncid1,"aoc_fraction",varid)
    ok = NF90_GET_VAR(ncid1,varid,varaoc_db)
    ok = NF90_close(ncid1)

  ! file 1: time-invarying data
    ok = NF90_OPEN(fglobal(1),0,ncid3)
    IF (ok /= NF90_NOERR) CALL nc_abort(ok,"Error opening file"//fglobal(1))
    print *, "global input1 = ", fglobal(1)

    ok = NF90_INQ_VARID(ncid3,"lon",lonid)
    ok = NF90_GET_VAR(ncid3,lonid,lon_flt)
    lon(:) = real(lon_flt(:),kind=dp)

    ok = NF90_INQ_VARID(ncid3,"lat",latid)
    ok = NF90_GET_VAR(ncid3,latid,lat_flt)
    lat(:) = real(lat_flt(:),kind=dp)

    ok = NF90_INQ_VARID(ncid3,"maxvegetfrac",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx4_flt)
    patchfrac(:,:,:) = real(varx4_flt(:,:,:,1),kind=dp)

    ok = NF90_INQ_VARID(ncid3,"HWSD_SOC",varid)
    ok = NF90_GET_VAR(ncid3,varid,varsoc3_db)

    ok = NF90_INQ_VARID(ncid3,"HWSD_bulk_density",varid)
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
             micglobal%lon(np)         = lon(i)
             micglobal%lat(np)         = lat(j)
             micparam%csoilobs(np,:)   = real(varsoc3_db(i,j,:),kind=dp)
             bulkd2                    = ( varbulk_db(i,j,1)*dble(zse(1))+varbulk_db(i,j,2)*dble(zse(2))+varbulk_db(i,j,3)*dble(zse(3)) )&
                                         /sum(dble(zse(1:3)))
             micglobal%bulkd(np)       = max(500.0_dp,min(1800.0_dp,real(bulkd2,kind=dp)))
             micglobal%patchfrac(np,:) = patchfrac(i,j,:)
             micglobal%pft(np)         = maxpft
             micparam%fracaoc(np,:)    = max(0.0_dp,min(0.7_dp,real(varaoc_db(i,j,:),kind=dp)))
          end if
       end if
    end do
    end do

    if(np/=mp) then
      print *, "np is not equal to mp", np,mp
      STOP
    end if


    ok = NF90_INQ_VARID(ncid3,"cell_area",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_db)
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    micglobal%area(:)=max(0.0, real(varmp1_db(:),kind=dp)) *(1.0e-12)

    ok = NF90_INQ_VARID(ncid3,"USDA_Soil_texture_class",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_db)
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    micglobal%isoil(:) = int(varmp1_db(:))

    ok = NF90_INQ_VARID(ncid3,"USDA_SoilSuborder",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_flt)
    varx2_db = real(varx2_flt,kind=8)
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    micglobal%sorder(:) = int(varmp1_db(:))

    ok = NF90_INQ_VARID(ncid3,"npp",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_db)
    call lonlat2mpx2(ilon,jlat,varx2_db,varmp1_db)
    micglobal%npp = real(varmp1_db,kind=dp)
    micglobal%npp = max(100.0,micglobal%npp)
    print *, "npp", maxval(micglobal%npp), minval(micglobal%npp),sum(micglobal%npp)/real(mp)


    ok = NF90_close(ncid3)



    ! read in the HWSD soil properties and soil cluster
    ok = NF90_OPEN(fglobal(3),0,ncid3)
    IF (ok /= NF90_NOERR) CALL nc_abort(ok,"Error opening file"//fglobal(3))
    print *, "global input1 = ", fglobal(3)

    ok = NF90_INQ_VARID(ncid3,"Bulk_density",varid)
    ok = NF90_GET_VAR(ncid3,varid,varbulk_db)

    ok = NF90_INQ_VARID(ncid3,"Clay_fraction",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3ms_db)
    call lonlat2mpx3a(ilon, jlat, 3, dble(zse(1:3)), varbulk_db(:,:,1:3), varx3ms_db(:,:,1:3), varmp1_db)
    micglobal%clay(:) = real(varmp1_db(:)*0.01,kind=dp)
    print *, "clay", maxval(micglobal%clay), minval(micglobal%clay),sum(micglobal%clay)/real(mp)

    ok = NF90_INQ_VARID(ncid3,"Silt_fraction",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3ms_db)
    call lonlat2mpx3a(ilon, jlat, 3, dble(zse(1:3)), varbulk_db(:,:,1:3), varx3ms_db(:,:,1:3), varmp1_db)
    micglobal%silt(:) = real(varmp1_db(:)*0.01,kind=dp)
    print *, "silt", maxval(micglobal%silt), minval(micglobal%silt),sum(micglobal%silt)/real(mp)

    ok = NF90_INQ_VARID(ncid3,"ph",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3ms_db)
    call lonlat2mpx3a(ilon, jlat, 3, dble(zse(1:3)), varbulk_db(:,:,1:3), varx3ms_db(:,:,1:3), varmp1_db)
    micglobal%ph = real(varmp1_db,kind=dp)
    micglobal%ph =min(9.5,max(3.5,micglobal%ph))
    print *, "ph", maxval(micglobal%ph), minval(micglobal%ph),sum(micglobal%ph)/real(mp)

    ok = NF90_INQ_VARID(ncid3,"Ald",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3ms5_db)
    call lonlat2mpx3a(ilon, jlat, 3, dble(zse(1:3)), varbulk_db(:,:,1:3), varx3ms5_db(:,:,1:3), varmp1_db)
    fald = real(varmp1_db,kind=dp)
    print *, "Ald", maxval(fald), minval(fald),sum(fald)/real(mp)

    ok = NF90_INQ_VARID(ncid3,"Alo",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3ms5_db)
    call lonlat2mpx3a(ilon, jlat, 3, dble(zse(1:3)), varbulk_db(:,:,1:3), varx3ms5_db(:,:,1:3), varmp1_db)
    falo = real(varmp1_db,kind=dp)
    print *, "Alo", maxval(falo), minval(falo),sum(falo)/real(mp)

    ok = NF90_INQ_VARID(ncid3,"Fed",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3ms5_db)
    call lonlat2mpx3a(ilon, jlat, 3, dble(zse(1:3)), varbulk_db(:,:,1:3), varx3ms5_db(:,:,1:3), varmp1_db)
    ffed = real(varmp1_db,kind=dp)
    print *, "fed", maxval(ffed), minval(ffed),sum(ffed)/real(mp)

    ok = NF90_INQ_VARID(ncid3,"Feo",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3ms5_db)
    call lonlat2mpx3a(ilon, jlat, 3, dble(zse(1:3)), varbulk_db(:,:,1:3), varx3ms5_db(:,:,1:3), varmp1_db)
    ffeo = real(varmp1_db,kind=dp)
    print *, "feo", maxval(ffeo), minval(ffeo),sum(ffeo)/real(mp)


    ok = NF90_INQ_VARID(ncid3,"Cluster",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx2_int)
    call lonlat2mpx2int(ilon, jlat, varx2_int, fcluster)
    print *, "Cluster", maxval(fcluster), minval(fcluster)

    micparam%bgctype =fcluster
    micglobal%bgctype=fcluster
    ok = NF90_close(ncid3)


    ! check the time-invariant data and replace bad values withy default values
    do np=1,mp
       pft = micglobal%pft(np)
       ns  = micglobal%isoil(np)
       i=ilon(np);j=jlat(np)
       IF(pft <0 .or. pft >mpft .or. ns <1 .or. ns >12) then
          micglobal%area(np) = -1.0
          pft= 1; ns =1
       end if

       micglobal%ligleaf(np)  = ligleaf2(pft)
       micglobal%ligwood(np)  = ligwood2(pft)
       micglobal%ligroot(np)  = ligroot2(pft)
       micglobal%cnleaf(np,:) = cnleaf2(pft)
       micglobal%cnwood(np,:) = cnwood2(pft)
       micglobal%cnroot(np,:) = cnroot2(pft)

       ! replacing negative values of metal oxide with their global means in kg/m2
       !if(fald(np)<0.0) fald(np) =0.46_dp
       !if(falo(np)<0.0) falo(np) =0.39_dp
       !if(ffed(np)<0.0) ffed(np) =2.74_dp
       !if(ffeo(np)<0.0) ffeo(np) =3.53_dp
       micparam%siteid(np)    = np
       micglobal%poros(:)  = 1.0 - micglobal%bulkd(:)/2650.0
       ! replace "NaN" with -1 for soil pH and clay and silt fractions
       if(micglobal%ph(np) /= micglobal%ph(np)) micglobal%ph(np)=-1
       if(micglobal%silt(np) /= micglobal%silt(np)) micglobal%silt(np)=-1.0
       if(micglobal%clay(np) /= micglobal%clay(np)) micglobal%clay(np)=-1.0
    end do

   ! use the cluster centres to estimate bgctype
   ! micglobal%bgctype=-1
   ! call cluster_hwsd(jmodel,micglobal%bgctype,micparam%csoilobs,micglobal%clay,micglobal%silt,micglobal%ph,fald,falo,ffed,ffeo,fcluster)
   ! micparam%bgctype =fcluster
   ! micglobal%bgctype=fcluster

    ! reading time-varying data
    ! temporary solution
    do n=1,ntime
       micglobal%time(n) = n
    end do

    print *, "reading time-varying data", fglobal(2)

  ! file 2: daily aboveground leaf fall (g C/m2/day)     ! Open netcdf file
    ok = NF90_OPEN(fglobal(2),0,ncid3)
    IF (ok /= NF90_NOERR) CALL nc_abort(ok,"Error opening file"//fglobal(2))

    ok = NF90_INQ_VARID(ncid3,"Leaf_fall",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3time_db)
    varx3time_db = max(0.0, varx3time_db)
    call lonlat2mpx3time(ilon,jlat,varx3time_db,varmp2_db)
    micglobal%dleaf = real(varmp2_db,kind=dp)
    print *, "dleaf", minval(micglobal%dleaf),maxval(micglobal%dleaf), &
                      sum(micglobal%dleaf)/real(size(micglobal%dleaf))

    ok = NF90_INQ_VARID(ncid3,"non_leaf_aboveground_litterfall",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3time_db)
    varx3time_db = max(0.0, varx3time_db)
    call lonlat2mpx3time(ilon,jlat,varx3time_db,varmp2_db)
    micglobal%dwood = real(varmp2_db,kind=dp)
    print *, "dwood", minval(micglobal%dwood),maxval(micglobal%dwood), &
                      sum(micglobal%dwood)/real(size(micglobal%dwood))

    ok = NF90_INQ_VARID(ncid3,"Belowground_litter_fall",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx3time_db)
    varx3time_db = max(0.0, varx3time_db)
    call lonlat2mpx3time(ilon,jlat,varx3time_db,varmp2_db)
    micglobal%droot = real(varmp2_db,kind=dp)
    print *, "droot", minval(micglobal%droot),maxval(micglobal%droot), &
                      sum(micglobal%droot)/real(size(micglobal%droot))

    ok = NF90_INQ_VARID(ncid3,"SoilTemp",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx4_db)
    call lonlat2mpx4b(ilon,jlat,-100.0d0,50.0d0,0.0d0,varx4_db,varmp3_db)
    micglobal%tsoil = real(varmp3_db,kind=dp)
    print *, "tsoil", minval(micglobal%tsoil),maxval(micglobal%tsoil), &
                      sum(micglobal%tsoil)/real(size(micglobal%tsoil))

    ok = NF90_INQ_VARID(ncid3,"SoilMoist",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx4_db)
    call lonlat2mpx4b(ilon,jlat,0.0d0,0.8d0,0.15d0,varx4_db,varmp3_db)
    micglobal%moist = real(varmp3_db,kind=dp)
    print *, "moist", minval(micglobal%moist),maxval(micglobal%moist), &
                      sum(micglobal%moist)/real(size(micglobal%moist))

    ok = NF90_INQ_VARID(ncid3,"water_potential",varid)
    ok = NF90_GET_VAR(ncid3,varid,varx4_db)
    call lonlat2mpx4b(ilon,jlat,-1000.0d0,0.0d0,-100.0d0,varx4_db,varmp3_db)
    micglobal%matpot = real(varmp3_db,kind=dp)
    print *, "matpot", minval(micglobal%matpot),maxval(micglobal%matpot), &
                       sum(micglobal%matpot)/real(size(micglobal%matpot))

    ok = NF90_close(ncid3)


    ! use modis-npp to rescale the orchidee NPP and carbon inputs to soil
    if(jmodel==3) then
       ok = nf90_open(fglobal(6),nf90_nowrite,ncid3)
       if(ok /= nf90_noerr) print*, "Error opening modisnpp"

       ! get variables
       ok = nf90_inq_varid(ncid3,"npp",varid)
       if(ok /= nf90_noerr) print*, "Error inquiring data modis_npp"
       ok = nf90_get_var(ncid3,varid,modisnpp)
       if(ok /= nf90_noerr) print*,"Error reading data npp"
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
       end do
    end if

    ! filter out land cells with "bgctype<0"
  !  print *, 'calculations are not done for the following cells'
    msite=0
    do np=1,mp
       if(micparam%bgctype(np) <1 .or. micparam%bgctype(np) >mbgc &
         .or. minval(micparam%csoilobs(np,:)) < 0.0               &
         .or. maxval(micparam%csoilobs(np,:)) > 120.0) then
    !      print *, np, micparam%bgctype(np),micglobal%area(np),micglobal%isoil(np), &
    !               micglobal%sorder(np),micglobal%bgctype(np), micglobal%npp(np)
          micparam%bgctype(np)= mbgc
          micglobal%area(np)  = -1.0
       end if
       ! replacing NPP in the time-invariant input file using the mean of time-varying input
       micglobal%npp(np) = 365.0 * sum(micglobal%dleaf(np,:) + micglobal%dwood(np,:) + micglobal%droot(np,:)) &
                         /real(size(micglobal%dleaf(np,:)))
       if(micglobal%isoil(np) <0  .or. micglobal%isoil(np) >12)   micglobal%isoil(np)=12
       if(micglobal%sorder(np) <0 .or. micglobal%sorder(np) >12)  micglobal%sorder(np)=12
       if(micglobal%bgctype(np) ==bgcopt .and. micglobal%area(np) >0) msite = msite + 1
    end do

    ! assign time-invariance properties from "micglobal" to "micparam"
    micparam%pft        = micglobal%pft
  !  micparam%bgctype    = micglobal%bgctype
    micparam%isoil      = micglobal%isoil
    micparam%sorder     = micglobal%sorder
    micparam%fligleaf   = micglobal%ligleaf
    micparam%fligroot   = micglobal%ligroot
    micparam%fligwood   = micglobal%ligwood
    micparam%xcnleaf(:) = micglobal%cnleaf(:,1)
    micparam%xcnroot(:) = micglobal%cnroot(:,1)
    micparam%xcnwood(:) = micglobal%cnwood(:,1)


    sitemax=2000
    if(msite>2*sitemax) then

       intval = msite/sitemax; isite=0
       do np=1,mp
          if(micglobal%bgctype(np) == bgcopt .and.micglobal%area(np) > 0.0) then
             isite = isite +1
             if(int(isite/intval)*intval /= isite.or. isite>sitemax*intval) micglobal%area(np) = -1.0
          end if
   !       if(micglobal%area(np) > 0.0 .and. micglobal%bgctype(np) == bgcopt) then
   !          write(*,103) isite,np, micglobal%bgctype(np), micglobal%area(np),micglobal%npp(np),micglobal%ph(np)
   !       endif
       end do
    else

      isite=0
      do np=1,mp
         if(micglobal%area(np) > 0.0 .and. micglobal%bgctype(np) == bgcopt) then
            isite=isite+1
   !         write(*,103) isite,np,micglobal%bgctype(np),micglobal%area(np),micglobal%npp(np),micglobal%ph(np)
         end if
      end do
      if(isite<10) print *, "too few sites ", isite

    end if

    micglobal%avgts(:) = sum(sum(micglobal%tsoil(:,:,:),dim=3),dim=2)/real(ms*ntime)
    micglobal%avgms(:) = sum(sum(micglobal%moist(:,:,:),dim=3),dim=2)/real(ms*ntime)

! write out time-invariant input data
    if(jglobal) then
       open(31,file=fglobal(5))
       do np=1,mp
          write(31,101) micglobal%lon(np),micglobal%lat(np),ilon(np),jlat(np),                 &
          micparam%siteid(np),micglobal%area(np),micparam%pft(np),                             &
          micparam%isoil(np),micparam%sorder(np),micparam%bgctype(np),fcluster(np), micglobal%npp(np),       &
          minval(micglobal%dleaf(np,:) + micglobal%dwood(np,:) + micglobal%droot(np,:))*365.0, &
          maxval(micglobal%dleaf(np,:) + micglobal%dwood(np,:) + micglobal%droot(np,:))*365.0, &
          micglobal%ph(np),micglobal%clay(np)*100.0,micglobal%silt(np)*100.0,                  &
          fald(np),falo(np),ffed(np),ffeo(np), micglobal%bulkd(np),                            &
          micglobal%avgts(np),micglobal%avgms(np), max(-1.0,micparam%csoilobs(np,:)),          &
          micparam%fracaoc(np,1),micparam%fracaoc(np,3), micparam%fracaoc(np,ms)

       end do
       close(31)
    end if
101 format(2(f8.3,1x),2(i6,1x),i10,1x,es16.8,1x,5(i5,1x),*(es16.8,1x))
103 format(3(i5,1x),3(f10.4,1x))

    deallocate(ilon,jlat,fcluster)
    deallocate(varx2_int,varx2_flt)
    deallocate(varx4_flt)
    deallocate(varx2_db)
    deallocate(varx3time_db,varx3ms_db,varx3ms5_db)
    deallocate(varx3_db,varsoc3_db,varbulk_db,varaoc_db)
    deallocate(varx4_db)

    deallocate(varmp1_db)
    deallocate(varmp2_db)
    deallocate(varmp3_db)

    deallocate(falo,fald,ffeo,ffed)
    deallocate(modisnpp,modisnpp_mp)

end subroutine getdata_global4_orchidee


!> compute cluster based on the z-transformed value of HWSD top 60cm soil properties
!! input: clay, silt, ph, ald, alo, fed and feo
!! output: cluster (integer)
!!
subroutine cluster_hwsd(jmodel,bgctype,socobs,fclay,fsilt,fph,fald,falo,ffed,ffeo,fcluster)
  integer,                    intent(in)    :: jmodel
      !! Land-surface model code (1=CABLE, 2=ORCHIDEE, 3=ORCHIDEE+modis_npp)
  integer,  dimension(mp),    intent(in)    :: bgctype
      !! Biogeochemical type
  real(dp), dimension(mp,ms), intent(in)    :: socobs
      !! observed soil organic carbon profile [mg C cm-3] per plot
  real(dp), dimension(mp),    intent(in)    :: fclay,fsilt,fph,fald,falo,ffed,ffeo
      !! soil properties: clay+silt fractions, pH, aluminum+iron oxides (dissolved+oxidal)
  integer,  dimension(mp),    intent(inout) :: fcluster
      !! soil cluster assignment (1-10, from HWSD centroid k-means clustering)

  ! Local variables
  real(dp), dimension(10,2)   :: claymid,siltmid,phmid,aldmid,alomid,fedmid,feomid
  real(dp), dimension(2)      :: clayavg,siltavg,phavg,aldavg,aloavg,fedavg,feoavg
  real(dp), dimension(2)      :: claysd,siltsd,phsd,aldsd,alosd,fedsd,feosd
  real(dp), dimension(7)      :: z
  real(dp), dimension(10,7)   :: xdist
  integer :: np,m,j
  ! results from K means cluster analysis  done 20260508 by Lingfei Wang
  data claymid/-0.9525_dp,-0.8011_dp,1.1920_dp,-0.5146_dp,0.1999_dp,-0.2399_dp,-0.7019_dp,1.4337_dp,0.8914_dp,-1.0750_dp,  &
                1.4969_dp,-1.0316_dp,-0.8065_dp,-0.1341_dp,1.1403_dp,1.5560_dp,-1.082_dp,0.6068_dp,-0.3449_dp,0.1033_dp/

  data siltmid/0.3693_dp,-0.9586_dp,-0.5314_dp,1.3111_dp,0.2016_dp,1.2570_dp,0.3701_dp,-0.7761_dp,-0.2799_dp,-0.4098_dp,   &
               -0.0865_dp,0.2881_dp,-1.0136_dp,1.1445_dp,-0.5746_dp,-0.8688_dp,-0.1002_dp,-0.4546_dp,1.3359_dp,0.1012_dp/

  data phmid/-0.8845_dp,0.5324_dp,-0.6359_dp,-0.3306_dp,1.5959_dp,-0.0262_dp,-1.1008_dp,-0.5988_dp,-0.0312_dp,-1.2470_dp,  &
              0.7591_dp,-1.0547_dp,0.3730_dp,0.0215_dp,-0.7063_dp,-0.6784_dp,-1.1424_dp,-0.5016_dp,-0.2510_dp,1.5791_dp/

  data aldmid/0.5629_dp,-0.8189_dp,2.1928_dp,-0.2735_dp,-0.8350_dp,-0.3037_dp,1.8790_dp,1.3780_dp,0.1905_dp,-0.4836_dp,   &
              -0.2553_dp,1.1344_dp,-0.7533_dp,-0.3713_dp,2.4355_dp,1.6124_dp,-0.2846_dp,0.5503_dp,-0.1318_dp,-0.8379_dp/

  data alomid/1.7405_dp,-0.7472_dp,1.7091_dp,-0.0801_dp,-0.6616_dp,-0.2281_dp,3.9982_dp,0.2820_dp,-0.0875_dp,-0.3417_dp,  &
              -0.1036_dp,3.0859_dp,-0.7085_dp,-0.3075_dp,2.0749_dp,0.4096_dp,-0.0546_dp,0.0595_dp,0.0642_dp,-0.6969_dp/

  data fedmid/-0.2133_dp,-0.8057_dp,1.4228_dp,-0.1518_dp,-0.7321_dp,-0.3320_dp,0.6954_dp,1.9225_dp,0.5566_dp,-0.6816_dp,  &
               0.3769_dp,0.0985_dp,-0.7497_dp,-0.4041_dp,1.5073_dp,2.1344_dp,-0.5428_dp,0.7526_dp,-0.0019_dp,-0.8012_dp/

  data feomid/1.0284_dp,-0.7249_dp,0.6078_dp,1.8211_dp,-0.9538_dp,0.3540_dp,2.3080_dp,-0.3343_dp,-0.4294_dp,0.0334_dp,   &
             -0.4503_dp,1.6601_dp,-0.6227_dp,0.1070_dp,0.6945_dp,-0.3107_dp,0.2227_dp,-0.3815_dp,1.7193_dp,-0.9859_dp/

  data clayavg/20.3759_dp,20.5158_dp/
  data siltavg/27.8392_dp,28.6576_dp/
  data phavg/5.7890_dp,5.8697_dp/
  data aldavg/0.4653_dp,0.4491_dp/
  data aloavg/0.4091_dp,0.3955_dp/
  data fedavg/2.7099_dp,2.6748_dp/
  data feoavg/0.6246_dp,0.6269_dp/

  data claysd/8.6170_dp,8.9668_dp/
  data siltsd/9.0690_dp,9.7478_dp/
  data phsd/0.9142_dp,0.9715_dp/
  data aldsd/0.3838_dp,0.3856_dp/
  data alosd/0.2970_dp,0.2898_dp/
  data fedsd/1.4412_dp,1.4988_dp/
  data feosd/0.4149_dp,0.4343_dp/

    fcluster(:)=bgctype(:)
    j=jmodel
    do np=1,mp
       if(bgctype(np)>1 .and. bgctype(np) < 10) then
          fcluster(np) = bgctype(np)
       else
         if(min(fclay(np),fsilt(np),fph(np),fald(np),falo(np),ffed(np),ffeo(np)) >0.0 .and. minval(socobs(np,:)) > 0.0) then
            xdist(:,:) = 1.0e6
            z(1) = (fclay(np)*100.0 - clayavg(j))/claysd(j)
            z(2) = (fsilt(np)*100.0 - siltavg(j))/siltsd(j)
            z(3) = (fph(np)   - phavg(j))/phsd(j)
            z(4) = (fald(np)  - aldavg(j))/aldsd(j)
            z(5) = (falo(np)  - aloavg(j))/alosd(j)
            z(6) = (ffed(np)  - fedavg(j))/fedsd(j)
            z(7) = (ffeo(np)  - feoavg(j))/feosd(j)
            do m=1,10
               xdist(m,1) = (z(1) - claymid(m,j))**2
               xdist(m,2) = (z(2) - siltmid(m,j))**2
               xdist(m,3) = (z(3) - phmid(m,j))**2
               xdist(m,4) = (z(4) - aldmid(m,j))**2
               xdist(m,5) = (z(5) - alomid(m,j))**2
               xdist(m,6) = (z(6) - fedmid(m,j))**2
               xdist(m,7) = (z(7) - feomid(m,j))**2
            end do
            fcluster(np) = MINLOC(sum(xdist,dim=2),dim=1)
         end if
       end if
    end do
end subroutine cluster_hwsd

!> map 2d variable into 1d
!! input: varx2_db(nlon,nlat)
!! output: varmp1_db(mp)
!!
subroutine lonlat2mpx2(ilon, jlat, varx2_db, varmp1_db)
    ! map varx2_db(nlon,nlat) to varmp1_db(mp)

    integer,  dimension(mp),        intent(in)    :: ilon,jlat
    real(dp), dimension(nlon,nlat), intent(in)    :: varx2_db
    real(dp), dimension(mp),        intent(inout) :: varmp1_db
    integer :: np

    ! Initialize output (optional)
    varmp1_db = 0.0d0

    do np = 1, mp
        ! Assign value
        varmp1_db(np) = varx2_db(ilon(np), jlat(np))
    end do

end subroutine lonlat2mpx2


!> map 2d variable into 1d
!! input: varx2_db(nlon,nlat)
!! output: varmp1_db(mp)
!!
subroutine lonlat2mpx2int(ilon, jlat, varx2_int, varmp1_int)
    ! map varx2_int(nlon,nlat) to varmp1_int(mp)

    integer, dimension(mp),        intent(in)    :: ilon,jlat
    integer, dimension(nlon,nlat), intent(in)    :: varx2_int
    integer, dimension(mp),        intent(inout) :: varmp1_int
    integer :: np

    ! Initialize output (optional)
    varmp1_int = 0

    do np = 1, mp
        ! Assign value
        varmp1_int(np) = varx2_int(ilon(np), jlat(np))
    end do

end subroutine lonlat2mpx2int

!> mapping 3d double real variables to 1d (mp)
!! input: varx3_db
!! output: varmp1_db
!!
subroutine lonlat2mpx3(ilon, jlat, patchfrac, varx3_db, varmp1_db)
! map varx3_db(nlon,nlat,mpft) to varmp1_db(mp)

    integer,  dimension(mp),             intent(in)    :: ilon,jlat
    real(dp), dimension(nlon,nlat,mpft), intent(in)    :: patchfrac
    real(dp), dimension(nlon,nlat,mpft), intent(in)    :: varx3_db
    real(dp), dimension(mp),             intent(inout) :: varmp1_db
    integer :: np
    real(dp), dimension(mpft)               :: varx_slice, weights
    real(dp) :: areatot

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


!> mapping 3d double real variables to 1d (mp)
!! input: varx3_db
!! output: varmp1_db
!!
subroutine lonlat2mpx3a(ilon, jlat, ms3, zse3, bulkd3, varx3_db, varmp1_db)
! map varx3_db(nlon,nlat,1:3) to varmp1_db(mp)
    integer,                            intent(in)    :: ms3
    integer,  dimension(mp),            intent(in)    :: ilon,jlat
    real(dp), dimension(nlon,nlat,ms3), intent(in)    :: bulkd3
    real(dp), dimension(ms3),           intent(in)    :: zse3
    real(dp), dimension(nlon,nlat,ms3), intent(in)    :: varx3_db
    real(dp), dimension(mp),            intent(inout) :: varmp1_db
    integer :: np,ns
    real(dp), dimension(ms3)                :: varx_slice, weights

    ! Initialize output
    varmp1_db = 0.0d0

    do np = 1, mp
       varx_slice= 0.0; weights=0.0
        do ns=1,ms3
           varx_slice(ns) =  varx3_db(ilon(np), jlat(np), ns) * bulkd3(ilon(np), jlat(np), ns) * zse3(ns)
           weights(ns)    =  bulkd3(ilon(np), jlat(np), ns) * zse3(ns)
        end do
        varmp1_db(np) = sum(varx_slice ) / sum(weights)
    end do

end subroutine lonlat2mpx3a

!> mapping 3d double real variables to 2d (mp)
!! input: varx3_db
!! output: varmp2_db
!!
subroutine lonlat2mpx3time(ilon, jlat, varx3time_db, varmp2_db)
! map varx3time_db(nlon,nlat,time) to varmp2_db(mp,time)

    integer,  dimension(mp),              intent(in)    :: ilon,jlat
    real(dp), dimension(mp,ntime),        intent(inout) :: varmp2_db
    real(dp), dimension(nlon,nlat,ntime), intent(inout) :: varx3time_db
    integer :: np

    ! Initialize output
    varmp2_db = 0.0d0

    do np = 1, mp

       if (ilon(np) < 1 .or. ilon(np) > nlon .or. &
           jlat(np) < 1 .or. jlat(np) > nlat) then
           write(*,*) "ERROR in lonlat2mpx3a"
           write(*,*) "np=", np
           write(*,*) "ilon=", ilon(np), " valid range 1:", nlon
           write(*,*) "jlat=", jlat(np), " valid range 1:", nlat
           stop
       end if

       varmp2_db(np,:) = varx3time_db(ilon(np), jlat(np), :)
    end do

end subroutine lonlat2mpx3time

!> mapping 4d double real variables to 3d (mp)
!! input: varx4_db(nlon,nlat,ms,ntime)
!! output: varmp3_db(mp,ms,ntime)
!!
subroutine lonlat2mpx4b(ilon,jlat,xmin,xmax,xdef,varx4_db,varmp3_db)
! map varx4_db(nlon,nlat,ms,ntime) to varmp3_db(mp,ms,ntime)

    integer,  dimension(mp),                 intent(in)    :: ilon, jlat
    real(dp), dimension(nlon,nlat,ms,ntime), intent(in)    :: varx4_db
    real(dp),                                intent(in)    :: xmin, xmax,xdef
    real(dp), dimension(mp,ms,ntime),        intent(inout) :: varmp3_db
    integer :: np,ns,nt

    ! Initialize output
    varmp3_db = xdef

    do np = 1, mp
       if (ilon(np) < 1 .or. ilon(np) > nlon .or. &
           jlat(np) < 1 .or. jlat(np) > nlat) then
           write(*,*) "ERROR in lonlat2mpx4: invalid grid index"
           write(*,*) "np=", np
           write(*,*) "ilon=", ilon(np), " valid 1:", nlon
           write(*,*) "jlat=", jlat(np), " valid 1:", nlat
           stop
       end if
       varmp3_db(np,:,:) = varx4_db(ilon(np),jlat(np),:,:)

       do ns=1,ms
       do nt =1,ntime
          if(varmp3_db(np,ns,nt) <xmin .or. varmp3_db(np,ns,nt) >xmax) then
             varmp3_db(np,:,:)=xdef
          end if
       end do
       end do
    end do

end subroutine lonlat2mpx4b


  !> read in global atmospheric C14 data and input data for model run with 14C
  !! input 1: file 1 "frac14c" with all observed C14, carbon input, soil properties and other site-sepefici
  !!        parameter for model run
  !! input 2: file 2" f14c" with atmospheric 14C in five different zones
  !! output: all data read in here are written into "micparam", "micinpout" and "micnpool"
  !! "fcluster" is not read in yet
  !!
  subroutine getdata_c14(frac14c,f14c,filecluster,micinput,micparam,micnpool,zse)
    use mic_constant, only : delt

    character(len=140),  INTENT(IN)    :: frac14c,f14c(5)
    character(len=140),  INTENT(IN)    :: filecluster ! cluster filename (not used)
    TYPE(mic_parameter), INTENT(INout) :: micparam
    TYPE(mic_input),     INTENT(INout) :: micinput
    TYPE(mic_npool),     INTENT(INOUT) :: micnpool
    real(dp),            INTENT(IN)    :: zse(ms)       ! soil layer thickness in m-2
    ! local variables
    integer:: ncid,varid,status
    integer:: np,ns,i,j
    integer:: nz

    character(len = nf90_max_name):: name
    real(dp),dimension(:,:),allocatable:: fclay,fsilt,fph,ftemp,fmoist,fporosity,fmatpot
    real(dp),dimension(:),  allocatable:: fsoc,fpoc,fmaoc,ffmpoc,ffmmaoc,fbulkd
    real(dp),dimension(:),  allocatable:: fnpp,fanpp,fbnpp,flignin,fcna,fcnb
    integer,  dimension(:),  allocatable:: fid,fpft,ftop,fbot,fyear,fregion,fcluster
    real(dp),   dimension(:),  allocatable:: lat,lon

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
    if(status /= nf90_noerr) print*, "Error opening frc_c14.nc"

    ! get dimensions/profile_id
    status = nf90_inq_varid(ncid,"nsite",varid)
    if(status /= nf90_noerr) print*, "Error inquiring dimensions/profile_id"
    status = nf90_get_var(ncid,varid,fid)
    if(status /= nf90_noerr) print*,"Error reading profile_id"

    ! get variables
    status = nf90_inq_varid(ncid,"SOC",varid)
    if(status /= nf90_noerr) print*, "Error inquiring soc"
    status = nf90_get_var(ncid,varid,fsoc)
    if(status /= nf90_noerr) print*,"Error reading soc"

    status = nf90_inq_varid(ncid,"bulkd",varid)
    if(status /= nf90_noerr) print*, "Error inquiring bulk density"
    status = nf90_get_var(ncid,varid,fbulkd)
    if(status /= nf90_noerr) print*,"Error reading bulk density"

    status = nf90_inq_varid(ncid,"clay",varid)
    if(status /= nf90_noerr) print*, "Error inquiring clay"
    status = nf90_get_var(ncid,varid,fclay)
    if(status /= nf90_noerr) print*,"Error reading clay"

    status = nf90_inq_varid(ncid,"silt",varid)
    if(status /= nf90_noerr) print*, "Error inquiring silt"
    status = nf90_get_var(ncid,varid,fsilt)
    if(status /= nf90_noerr) print*,"Error reading silt"

    status = nf90_inq_varid(ncid,"ph",varid)
    if(status /= nf90_noerr) print*, "Error inquiring ph"
    status = nf90_get_var(ncid,varid,fph)
    if(status /= nf90_noerr) print*,"Error reading ph"

    status = nf90_inq_varid(ncid,"temp",varid)
    if(status /= nf90_noerr) print*, "Error inquiring soil temperature"
    status = nf90_get_var(ncid,varid,ftemp)
    if(status /= nf90_noerr) print*,"Error reading soil temperature"

    status = nf90_inq_varid(ncid,"moist",varid)
    if(status /= nf90_noerr) print*, "Error inquiring soil moisture"
    status = nf90_get_var(ncid,varid,fmoist)
    if(status /= nf90_noerr) print*,"Error reading soil moisture"

    status = nf90_inq_varid(ncid,"porosity",varid)
    if(status /= nf90_noerr) print*, "Error inquiring soil porosity"
    status = nf90_get_var(ncid,varid,fporosity)
    if(status /= nf90_noerr) print*,"Error reading soil porosity"

    status = nf90_inq_varid(ncid,"matpot",varid)
    if(status /= nf90_noerr) print*, "Error inquiring soil matric potential"
    status = nf90_get_var(ncid,varid,fmatpot)
    if(status /= nf90_noerr) print*,"Error reading soil matric potential"

    status = nf90_inq_varid(ncid,"npp",varid)
    if(status /= nf90_noerr) print*, "Error inquiring npp"
    status = nf90_get_var(ncid,varid,fnpp)
    if(status /= nf90_noerr) print*,"Error reading npp"

    status = nf90_inq_varid(ncid,"anpp",varid)
    if(status /= nf90_noerr) print*, "Error inquiring anpp"
    status = nf90_get_var(ncid,varid,fanpp)
    if(status /= nf90_noerr) print*,"Error reading anpp"

    status = nf90_inq_varid(ncid,"bnpp",varid)
    if(status /= nf90_noerr) print*, "Error inquiring bnpp"
    status = nf90_get_var(ncid,varid,fbnpp)
    if(status /= nf90_noerr) print*,"Error reading bnpp"

    status = nf90_inq_varid(ncid,"lignin_C",varid)
    if(status /= nf90_noerr) print*, "Error inquiring lignin/C"
    status = nf90_get_var(ncid,varid,flignin)
    if(status /= nf90_noerr) print*,"Error reading lignin/C"

    status = nf90_inq_varid(ncid,"cna",varid)
    if(status /= nf90_noerr) print*, "Error inquiring C/N aboveground"
    status = nf90_get_var(ncid,varid,fcna)
    if(status /= nf90_noerr) print*,"Error reading C/N aboveground"

    status = nf90_inq_varid(ncid,"cnb",varid)
    if(status /= nf90_noerr) print*, "Error inquiring C/N belowground"
    status = nf90_get_var(ncid,varid,fcnb)
    if(status /= nf90_noerr) print*,"Error reading C/N belowground"

    status = nf90_inq_varid(ncid,"pft",varid)
    if(status /= nf90_noerr) print*, "Error inquiring plant functional type"
    status = nf90_get_var(ncid,varid,fpft)
    if(status /= nf90_noerr) print*,"Error reading plant functional type"

      status = nf90_inq_varid(ncid,"POC",varid)
      if(status /= nf90_noerr) print*, "Error inquiring POC"
      status = nf90_get_var(ncid,varid,fpoc)
      if(status /= nf90_noerr) print*,"Error reading POC"

      status = nf90_inq_varid(ncid,"MAOC",varid)
      if(status /= nf90_noerr) print*, "Error inquiring MAOC"
      status = nf90_get_var(ncid,varid,fmaoc)
      if(status /= nf90_noerr) print*,"Error reading MAOC"

      status = nf90_inq_varid(ncid,"fm_poc",varid)
      if(status /= nf90_noerr) print*, "Error inquiring fm_poc"
      status = nf90_get_var(ncid,varid,ffmpoc)
      if(status /= nf90_noerr) print*,"Error reading fm_poc"

      status = nf90_inq_varid(ncid,"fm_maoc",varid)
      if(status /= nf90_noerr) print*, "Error inquiring fm_maoc"
      status = nf90_get_var(ncid,varid,ffmmaoc)
      if(status /= nf90_noerr) print*,"Error reading fm_maoc"

      status = nf90_inq_varid(ncid,"top_depth",varid)
      if(status /= nf90_noerr) print*, "Error inquiring top depth"
      status = nf90_get_var(ncid,varid,ftop)
      if(status /= nf90_noerr) print*,"Error reading top depth"

      status = nf90_inq_varid(ncid,"bot_depth",varid)
      if(status /= nf90_noerr) print*, "Error inquiring bottom depth"
      status = nf90_get_var(ncid,varid,fbot)
      if(status /= nf90_noerr) print*,"Error reading bottom depth"

      status = nf90_inq_varid(ncid,"c14_year",varid)
      if(status /= nf90_noerr) print*, "Error inquiring c14 year"
      status = nf90_get_var(ncid,varid,fyear)
      if(status /= nf90_noerr) print*,"Error reading c14 year"

      status = nf90_inq_varid(ncid,"c14_region",varid)
      if(status /= nf90_noerr) print*, "Error inquiring c14 region"
      status = nf90_get_var(ncid,varid,fregion)
      if(status /= nf90_noerr) print*,"Error reading c14 region"

      status = nf90_inq_varid(ncid,"Lon",varid)
      if(status /= nf90_noerr) print*, "Error inquiring Lon"
      status = nf90_get_var(ncid,varid,lon)
      if(status /= nf90_noerr) print*,"Error reading Lon"

      status = nf90_inq_varid(ncid,"Lat",varid)
      if(status /= nf90_noerr) print*, "Error inquiring Lat"
      status = nf90_get_var(ncid,varid,lat)
      if(status /= nf90_noerr) print*,"Error reading Lat"

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
         end do !"ns"
      end do    ! "np=1,mp"

      ! read in the standard 14C atmospheric data for five zones
!         f14c(1) ='/g/data/w97/lw9370/combined-model/c14/code-structure/data/NH1-C14.csv'
!         f14c(2) ='/g/data/w97/lw9370/combined-model/c14/code-structure/data/NH2-C14.csv'
!         f14c(3) ='/g/data/w97/lw9370/combined-model/c14/code-structure/data/NH3-C14.csv'
!         f14c(4) ='/g/data/w97/lw9370/combined-model/c14/code-structure/data/SH12-C14.csv'
!         f14c(5) ='/g/data/w97/lw9370/combined-model/c14/code-structure/data/SH3-C14.csv'
         do nz=1,5
             call get14catm(nz,f14c(nz),micparam)
         end do

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
    character(len=140), intent(in)    :: cfraction
    integer,            intent(inout) :: mpx
    integer :: ncid,varid,status
    ! open .nc file
    status = nf90_open(cfraction,nf90_nowrite,ncid)
    if(status /= nf90_noerr) print*, "Error opening c_fraction.nc"

    ! get dimension
    status = nf90_inq_dimid(ncid,"nsite",varid)
    if(status /= nf90_noerr) print*, "Error inquiring dimensions/nsite"
    status = nf90_inquire_dimension(ncid,varid,len=mpx)
    if(status /= nf90_noerr) print*,"Error dimensions/nsite"

    ! Close netcdf file
    status = NF90_CLOSE(ncid)
  end subroutine  getdata_frc_dim

  !> read in data for model run to calculate POC and MAOC fractions
  !!
  subroutine getdata_frc(cfraction,jglobal,bgcopt,micinput,micparam,micnpool,micglobal,zse)
    use mic_constant, only : delt
    character(len=140),     INTENT(IN)    :: Cfraction
    logical,                INTENT(IN)    :: jglobal
    integer,                INTENT(IN)    :: bgcopt
    TYPE(mic_parameter),    INTENT(INout) :: micparam
    TYPE(mic_input),        INTENT(INout) :: micinput
    TYPE(mic_npool),        INTENT(INOUT) :: micnpool
    TYPE(mic_global_input), INTENT(INout) :: micglobal
    real(dp),               INTENT(IN)    :: zse(ms)
    integer:: ncid,varid,status
    integer:: np,ns,i,j
    integer:: nz

    character(len = nf90_max_name):: name
    real(dp),dimension(:),         allocatable:: fclay,fsilt,fph,ftemp,fmoist,fporosity,fmatpot
    real(dp),dimension(:),         allocatable:: fsoc,fpoc,fmaoc,fbulkd
    real(dp),dimension(:),         allocatable:: fnpp,fanpp,fbnpp,flignin,fcna,fcnb
    real(dp),dimension(:),         allocatable:: fmg,fca,falo,fald,ffeo,ffed
    integer,dimension(:),           allocatable:: fid,fpft,ftop,fbot,fdataid,fcluster
    real(dp), dimension(:), allocatable:: lat,lon
    ! local variation for clustering
    integer :: n,msite


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
    if(status /= nf90_noerr) print*, "Error opening c_fraction.nc"

    ! get dimensions/profile_id
    status = nf90_inq_varid(ncid,"nsite",varid)
    if(status /= nf90_noerr) print*, "Error inquiring dimensions/profile_id"
    status = nf90_get_var(ncid,varid,fid)
    if(status /= nf90_noerr) print*,"Error reading profile_id"

    ! get variables
    status = nf90_inq_varid(ncid,"dataid",varid)
    if(status /= nf90_noerr) print*, "Error inquiring data ID"
    status = nf90_get_var(ncid,varid,fdataid)
    if(status /= nf90_noerr) print*,"Error reading data ID"

    status = nf90_inq_varid(ncid,"SOC",varid)
    if(status /= nf90_noerr) print*, "Error inquiring soc"
    status = nf90_get_var(ncid,varid,fsoc)
    if(status /= nf90_noerr) print*,"Error reading soc"

    status = nf90_inq_varid(ncid,"bulkd",varid)
    if(status /= nf90_noerr) print*, "Error inquiring bulk density"
    status = nf90_get_var(ncid,varid,fbulkd)
    if(status /= nf90_noerr) print*,"Error reading bulk density"

    status = nf90_inq_varid(ncid,"clay",varid)
    if(status /= nf90_noerr) print*, "Error inquiring clay"
    status = nf90_get_var(ncid,varid,fclay)
    if(status /= nf90_noerr) print*,"Error reading clay"

    status = nf90_inq_varid(ncid,"silt",varid)
    if(status /= nf90_noerr) print*, "Error inquiring silt"
    status = nf90_get_var(ncid,varid,fsilt)
    if(status /= nf90_noerr) print*,"Error reading silt"

    status = nf90_inq_varid(ncid,"ph",varid)
    if(status /= nf90_noerr) print*, "Error inquiring ph"
    status = nf90_get_var(ncid,varid,fph)
    if(status /= nf90_noerr) print*,"Error reading ph"

    status = nf90_inq_varid(ncid,"temp",varid)
    if(status /= nf90_noerr) print*, "Error inquiring soil temperature"
    status = nf90_get_var(ncid,varid,ftemp)
    if(status /= nf90_noerr) print*,"Error reading soil temperature"

    status = nf90_inq_varid(ncid,"moist",varid)
    if(status /= nf90_noerr) print*, "Error inquiring soil moisture"
    status = nf90_get_var(ncid,varid,fmoist)
    if(status /= nf90_noerr) print*,"Error reading soil moisture"

    status = nf90_inq_varid(ncid,"porosity",varid)
    if(status /= nf90_noerr) print*, "Error inquiring soil porosity"
    status = nf90_get_var(ncid,varid,fporosity)
    if(status /= nf90_noerr) print*,"Error reading soil porosity"

    status = nf90_inq_varid(ncid,"matpot",varid)
    if(status /= nf90_noerr) print*, "Error inquiring soil matric potential"
    status = nf90_get_var(ncid,varid,fmatpot)
    if(status /= nf90_noerr) print*,"Error reading soil matric potential"

    status = nf90_inq_varid(ncid,"npp",varid)
    if(status /= nf90_noerr) print*, "Error inquiring npp"
    status = nf90_get_var(ncid,varid,fnpp)
    if(status /= nf90_noerr) print*,"Error reading npp"

    status = nf90_inq_varid(ncid,"anpp",varid)
    if(status /= nf90_noerr) print*, "Error inquiring anpp"
    status = nf90_get_var(ncid,varid,fanpp)
    if(status /= nf90_noerr) print*,"Error reading anpp"

    status = nf90_inq_varid(ncid,"bnpp",varid)
    if(status /= nf90_noerr) print*, "Error inquiring bnpp"
    status = nf90_get_var(ncid,varid,fbnpp)
    if(status /= nf90_noerr) print*,"Error reading bnpp"

    status = nf90_inq_varid(ncid,"lignin_C",varid)
    if(status /= nf90_noerr) print*, "Error inquiring lignin/C"
    status = nf90_get_var(ncid,varid,flignin)
    if(status /= nf90_noerr) print*,"Error reading lignin/C"

    status = nf90_inq_varid(ncid,"cna",varid)
    if(status /= nf90_noerr) print*, "Error inquiring C/N aboveground"
    status = nf90_get_var(ncid,varid,fcna)
    if(status /= nf90_noerr) print*,"Error reading C/N aboveground"

    status = nf90_inq_varid(ncid,"cnb",varid)
    if(status /= nf90_noerr) print*, "Error inquiring C/N belowground"
    status = nf90_get_var(ncid,varid,fcnb)
    if(status /= nf90_noerr) print*,"Error reading C/N belowground"

    status = nf90_inq_varid(ncid,"pft",varid)
    if(status /= nf90_noerr) print*, "Error inquiring plant functional type"
    status = nf90_get_var(ncid,varid,fpft)
    if(status /= nf90_noerr) print*,"Error reading plant functional type"

    status = nf90_inq_varid(ncid,"POC",varid)
    if(status /= nf90_noerr) print*, "Error inquiring POC"
    status = nf90_get_var(ncid,varid,fpoc)
    if(status /= nf90_noerr) print*,"Error reading POC"

    status = nf90_inq_varid(ncid,"MAOC",varid)
    if(status /= nf90_noerr) print*, "Error inquiring MAOC"
    status = nf90_get_var(ncid,varid,fmaoc)
    if(status /= nf90_noerr) print*,"Error reading MAOC"

    status = nf90_inq_varid(ncid,"top_depth",varid)
    if(status /= nf90_noerr) print*, "Error inquiring top depth"
    status = nf90_get_var(ncid,varid,ftop)
    if(status /= nf90_noerr) print*,"Error reading top depth"

    status = nf90_inq_varid(ncid,"bot_depth",varid)
    if(status /= nf90_noerr) print*, "Error inquiring bottom depth"
    status = nf90_get_var(ncid,varid,fbot)
    if(status /= nf90_noerr) print*,"Error reading bottom depth"

    status = nf90_inq_varid(ncid,"Mg",varid)
    if(status /= nf90_noerr) print*, "Error inquiring Mg"
    status = nf90_get_var(ncid,varid,fmg)
    if(status /= nf90_noerr) print*,"Error reading Mg"

    status = nf90_inq_varid(ncid,"Ca",varid)
    if(status /= nf90_noerr) print*, "Error inquiring Ca"
    status = nf90_get_var(ncid,varid,fca)
    if(status /= nf90_noerr) print*,"Error reading Ca"

    status = nf90_inq_varid(ncid,"Alo",varid)
    if(status /= nf90_noerr) print*, "Error inquiring Alo"
    status = nf90_get_var(ncid,varid,falo)
    if(status /= nf90_noerr) print*,"Error reading Alo"

    status = nf90_inq_varid(ncid,"Ald",varid)
    if(status /= nf90_noerr) print*, "Error inquiring Ald"
    status = nf90_get_var(ncid,varid,fald)
    if(status /= nf90_noerr) print*,"Error reading Ald"

    status = nf90_inq_varid(ncid,"Feo",varid)
    if(status /= nf90_noerr) print*, "Error inquiring Feo"
    status = nf90_get_var(ncid,varid,ffeo)
    if(status /= nf90_noerr) print*,"Error reading Feo"

    status = nf90_inq_varid(ncid,"Fed",varid)
    if(status /= nf90_noerr) print*, "Error inquiring Fed"
    status = nf90_get_var(ncid,varid,ffed)
    if(status /= nf90_noerr) print*,"Error reading Fed"

    status = nf90_inq_varid(ncid,"Lat",varid)
    if(status /= nf90_noerr) print*, "Error inquiring Lat"
    status = nf90_get_var(ncid,varid,lat)
    if(status /= nf90_noerr) print*,"Error reading Lat"

    status = nf90_inq_varid(ncid,"Lon",varid)
    if(status /= nf90_noerr) print*, "Error inquiring Lon"
    status = nf90_get_var(ncid,varid,lon)
    if(status /= nf90_noerr) print*,"Error reading Lon"

    ! Close netcdf file
    status = NF90_CLOSE(ncid)

    if(jglobal) open(100,file="inputdata_frc.txt")

    ! converting metal oxide from cmol/kg to kg/m2
    ! metal-oxide(cmol/kg) = metal-oxide(kg/m2) *100/(h*bd*MW)
    ! h: soil thickness in m; bdL bulk density in kg/m3
    ! MW=0.02698 for Al =0.05585 for Fe
    do np=1,mp
       fald(np) = fald(np) * (fbot(np)-ftop(np))*0.01 * fbulkd(np) * 0.0298 /100.0
       falo(np) = falo(np) * (fbot(np)-ftop(np))*0.01 * fbulkd(np) * 0.0298 /100.0
       ffed(np) = ffed(np) * (fbot(np)-ftop(np))*0.01 * fbulkd(np) * 0.05585 /100.0
       ffeo(np) = ffeo(np) * (fbot(np)-ftop(np))*0.01 * fbulkd(np) * 0.05585 /100.0
       do ns=1,ms
           micparam%csoilobs(np,ns)    = fsoc(np)
       end do
    end do

    micparam%bgctype = 0
    call cluster_hwsd(2,micparam%bgctype,micparam%csoilobs,fclay,fsilt,fph,fald,falo,ffed,ffeo,fcluster)

      micparam%bgctype=fcluster
    !  micparam%csoilobs(:,:) = -999.0
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
          micinput%Dleaf(np)      = fanpp(np)/(24.0*365.0)*delt     !gc/m2/delt
          micinput%Droot(np)      = fbnpp(np)/(24.0*365.0)*delt     !gc/m2/delt
          !micinput%Dwood(np)      = forcdata(np,17)/(24.0*365.0)*delt     !gc/m2/delt

          micparam%xcnleaf(np)    = fcna(np)
          micparam%xcnroot(np)    = fcnb(np)
          !micparam%xcnwood(np)    = forcdata(np,20)
          micparam%fligleaf(np)   = flignin(np)
          micparam%fligroot(np)   = flignin(np)
          !micparam%fligwood(np)   = forcdata(np,23)

          ! for merging
          micglobal%npp(np)     = fnpp(np)
          micglobal%dleaf(np,:) = fanpp(np)/(365.0*delt)
          micglobal%droot(np,:) = fbnpp(np)/(365.0*delt)
          micglobal%dwood(np,:) = 0.0
          micglobal%area(np)    = 1.0

         do ns=1,ms
            micinput%tavg(np,ns)     = ftemp(np)  ! average temperature in deg C
            micinput%wavg(np,ns)     = fmoist(np)  ! average soil water content mm3/mm3
            micinput%clay(np,ns)     = fclay(np)  ! clay content (fraction)
            micinput%silt(np,ns)     = fsilt(np)  ! silt content (fraction)
            micinput%ph(np,ns)       = fph(np)
            micinput%porosity(np,ns) = fporosity(np) !porosity mm3/mm3
            micinput%matpot(np,ns)   = fmatpot(np)  ! soil matric potential -kPa

        !    micparam%csoilobs(np,ns)    = fsoc(np)
            micinput%bulkd(np,ns)       = fbulkd(np)

            micparam%csoilobsp(np,ns)   = fpoc(np)
            micparam%csoilobsm(np,ns)   = fmaoc(np)

            !micnpool%mineralN(np,ns) = forcdata(np,7)*0.001 ! mineral N: "0.001" mg N /kg soil --> g N /kg soil
            ! for merging
            micglobal%tsoil(np,ns,:) = ftemp(np)    ! average temperature in deg C
            micglobal%moist(np,ns,:) = fmoist(np)   ! average soil water content mm3/mm3
            micglobal%matpot(np,ns,:)= fmatpot(np)  ! soil matric potential -kPa
         end do !"ns"
         ! for merging
         micglobal%clay(np)       = fclay(np)       ! clay content (fraction)
         micglobal%silt(np)       = fsilt(np)       ! silt content (fraction)
         micglobal%ph(np)         = fph(np)
         micglobal%poros(np)      = fporosity(np)   ! porosity mm3/mm3
         micglobal%bulkd(np)      = fbulkd(np)
      end do  ! np=1,mp

      micglobal%avgts(:) = sum(sum(micglobal%tsoil(:,:,:),dim=3),dim=2)/real(ms*ntime)
      micglobal%avgms(:) = sum(sum(micglobal%moist(:,:,:),dim=3),dim=2)/real(ms*ntime)

      do np=1,mp

         if(micparam%bgctype(np) ==bgcopt) then
            msite=msite + 1
         end if
         if(jglobal) then
            write(100,901) micparam%siteid(np),micparam%dataid(np),micparam%pft(np),micparam%bgctype(np),micparam%top(np),micparam%bot(np) , &
                         fnpp(np),fanpp(np),fbnpp(np),fcna(np),fcnb(np),flignin(np),ftemp(np),fmoist(np),fclay(np),fsilt(np),fph(np), &
                         fporosity(np),fmatpot(np),fbulkd(np),fald(np),falo(np),ffed(np),ffeo(np),fsoc(np),fpoc(np),fmaoc(np)
         end if

      end do    ! "np=1,mp"

    print *, "total sites = ", msite, "for bgcopt= ",bgcopt
    if(jglobal) close(100)
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
    integer,             INTENT(IN) :: nz
    character(len=140),  INTENT(IN) :: f14cz
    TYPE(mic_parameter), INTENT(INout)   :: micparam
    integer :: i, ny, nc14atm(100,5)
    real(dp)  :: year,c14del,sdx1,c14fm,sdx2
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
      end do

      do i=1,79 !! 1941-2019
        read(13,*,end=91) year,c14del,sdx1,c14fm,sdx2
        ny = year - 1940
         if(ny<1 .or. ny>79) then
            print *, "year", year, "outside the range"
            stop
         else
            micparam%c14atm(ny,nz,1) = c14del !!! delta c14
            micparam%c14atm(ny,nz,2) = c14fm
         end if
      end do
91    close(13)
   end subroutine get14catm

   subroutine getdata_hwsd_dim(fhwsdsoc,mpx,timex)
    character(len=140), intent(in)    :: fhwsdsoc
    integer,            intent(inout) :: mpx,timex
    integer :: ncid,varid,status
   ! open .nc file
    status = nf90_open(fhwsdsoc,nf90_nowrite,ncid)
    if(status /= nf90_noerr) print*, "Error opening c_fraction.nc"

    ! get dimensions
    status = nf90_inq_dimid(ncid,"nsite",varid)
    if(status /= nf90_noerr) print*, "Error inquiring dimensions/nsite"
    status = nf90_inquire_dimension(ncid,varid,len=mpx)
    if(status /= nf90_noerr) print*,"Error dimensions/nsite"

    !
    status = nf90_inq_dimid(ncid,"time",varid)
    if(status /= nf90_noerr) print*, "Error inquiring dimensions/ntime"
    status = nf90_inquire_dimension(ncid,varid,len=timex)
    if(status /= nf90_noerr) print*,"Error reading profile_id"

    ! Close netcdf file
    status = NF90_CLOSE(ncid)
   end subroutine  getdata_hwsd_dim

  subroutine getdata_hwsd(fhwsdsoc,fmodis,fanoc,jglobal,bgcopt,jopt,jmodel,micparam,micglobal,zse)
    use mic_constant, only : cnleaf1, cnroot1, cnwood1, ligleaf1, ligroot1, ligwood1, &
                             cnleaf2, cnroot2, cnwood2, ligleaf2, ligroot2, ligwood2

    !use micglobal%area (area fraction) as a switch to run for selected sites during parameter optimization (jopt==0)
    !model only runs for those sites with micglobal%area(np) > 0.0
    character(len=140),           INTENT(IN) :: fhwsdsoc,fmodis,fanoc
    logical,                      INTENT(IN) :: jglobal
    integer,                      INTENT(IN) :: bgcopt
    logical,                      INTENT(IN) :: jopt
    integer,                      INTENT(IN) :: jmodel
    TYPE(mic_parameter),          INTENT(INout) :: micparam
    TYPE(mic_global_input),       INTENT(INout) :: micglobal
    real(dp),                     INTENT(IN) :: zse(ms)
    ! local variables
    integer:: ncid,varid,status
    integer:: np,ns,k,ipft,nsocobs,ilonx,jlatx
    integer:: intval,msite,isite,sitemax
    integer,           dimension(:),     allocatable     :: ivarx1,fcluster
    real,              dimension(:),     allocatable     :: varx1float
    real,              dimension(:,:),   allocatable     :: fracaoc
    real(dp),  dimension(:),     allocatable     :: varx1db,avgts,avgms
    real(dp),  dimension(:,:),   allocatable     :: varx2db,fsoc7,bulkd
    real(dp),  dimension(:,:,:), allocatable     :: tsoil7,moist7,watpot7
    real(dp),  dimension(:),     allocatable     :: fald,falo,ffed,ffeo
    real(dp),  dimension(:,:),   allocatable     :: modisnpp
    real(dp),  dimension(:),     allocatable     :: modisnpp_mp

    allocate(ivarx1(mp),fcluster(mp))
    allocate(varx1float(mp),varx1db(mp),avgts(mp),avgms(mp))
    allocate(varx2db(mp,ntime),fsoc7(mp,7),bulkd(mp,7))
    allocate(tsoil7(mp,7,ntime),moist7(mp,7,ntime),watpot7(mp,7,ntime))
    allocate(fald(mp),falo(mp),ffed(mp),ffeo(mp))
    allocate(modisnpp(720,360),modisnpp_mp(mp))
    allocate(fracaoc(mp,7))

   ! open .nc file
    print *, " calling getdata_hwsd"
    print *,"input file", fhwsdsoc
    print *,"fansoc file", fanoc
    print *,"mp ms bgcopt=",    mp,ms,bgcopt

    status = nf90_open(fhwsdsoc,nf90_nowrite,ncid)
    if(status /= nf90_noerr) print*, "Error opening c_fraction.nc"

    ! get variables
    status = nf90_inq_varid(ncid,"lat",varid)
    if(status /= nf90_noerr) print*, "Error inquiring data lat"
    status = nf90_get_var(ncid,varid,varx1db)
    if(status /= nf90_noerr) print*,"Error reading data lat"
    micglobal%lat = real(varx1db,kind=dp)

    status = nf90_inq_varid(ncid,"lon",varid)
    if(status /= nf90_noerr) print*, "Error inquiring data lont"
    status = nf90_get_var(ncid,varid,varx1db)
    if(status /= nf90_noerr) print*,"Error reading data lon"
    micglobal%lon=real(varx1db,kind=dp)

    status = nf90_inq_varid(ncid,"max_PFT",varid)
    if(status /= nf90_noerr) print*, "Error inquiring data PFT"
    status = nf90_get_var(ncid,varid,ivarx1)
    if(status /= nf90_noerr) print*,"Error reading data PFT"
    micglobal%pft = ivarx1

    status = nf90_inq_varid(ncid,"USDA_SoilSuborder",varid)
    if(status /= nf90_noerr) print*, "Error inquiring soil order"
    status = nf90_get_var(ncid,varid,ivarx1)
    if(status /= nf90_noerr) print*,"Error reading soil order"
    micglobal%sorder = ivarx1

    if(jmodel==1) then
       status = nf90_inq_varid(ncid,"isoil",varid)
       if(status /= nf90_noerr) print*, "Error inquiring soil texturep"
       status = nf90_get_var(ncid,varid,ivarx1)
       if(status /= nf90_noerr) print*,"Error reading soil texure"
       micglobal%isoil = ivarx1
    end if
    if(jmodel==2 .or.jmodel==3) then
       status = nf90_inq_varid(ncid,"USDA_Soil_texture_class",varid)
       if(status /= nf90_noerr) print*, "Error inquiring soil texturep"
       status = nf90_get_var(ncid,varid,ivarx1)
       if(status /= nf90_noerr) print*,"Error reading soil texure"
       micglobal%isoil = ivarx1
    end if

    status = nf90_inq_varid(ncid,"max_PFTfrac",varid)
    if(status /= nf90_noerr) print*, "Error inquiring max_PFTfrac"
    status = nf90_get_var(ncid,varid,varx1float)
    if(status /= nf90_noerr) print*,"Error reading max_PFTfrac"
    micglobal%area = real(varx1float,kind=dp)

    status = nf90_inq_varid(ncid,"npp",varid)
    if(status /= nf90_noerr) print*, "Error inquiring npp"
    status = nf90_get_var(ncid,varid,varx1db)
    if(status /= nf90_noerr) print*,"Error reading npp"
    micglobal%npp = real(varx1db,kind=dp)

    status = nf90_inq_varid(ncid,"pH",varid)
    if(status /= nf90_noerr) print*, "Error inquiring ph"
    status = nf90_get_var(ncid,varid,varx1float)
    if(status /= nf90_noerr) print*,"Error reading ph"
    micglobal%ph = real(varx1float,kind=dp)

    status = nf90_inq_varid(ncid,"clay",varid)
    if(status /= nf90_noerr) print*, "Error inquiring clay"
    status = nf90_get_var(ncid,varid,varx1float)
    if(status /= nf90_noerr) print*,"Error reading clay"
    micglobal%clay = real(varx1float,kind=dp)

    status = nf90_inq_varid(ncid,"silt",varid)
    if(status /= nf90_noerr) print*, "Error inquiring silt"
    status = nf90_get_var(ncid,varid,varx1float)
    if(status /= nf90_noerr) print*,"Error reading silt"
    micglobal%silt = real(varx1float,kind=dp)

 !   status = nf90_inq_varid(ncid,'bulk_density',varid)
 !   if(status /= nf90_noerr) print*, 'Error inquiring soil bulk density'
 !   status = nf90_get_var(ncid,varid,varx1float)
 !   if(status /= nf90_noerr) print*,'Error reading bulk density'
 !   micglobal%bulkd= real(varx1float,kind=dp)
 !   use HWSD bulk density (vary with soil layer)
     status = nf90_inq_varid(ncid,"HWSD_bulk_density",varid)
     if(status /= nf90_noerr) print*, "Error inquiring soil bulk density"
     status = nf90_get_var(ncid,varid,bulkd)
     if(status /= nf90_noerr) print*,"Error reading bulk density"

    status = nf90_inq_varid(ncid,"HWSD_SOC",varid)
    if(status /= nf90_noerr) print*, "Error inquiring soil carbon"
    status = nf90_get_var(ncid,varid,fsoc7)
    if(status /= nf90_noerr) print*,"Error reading soil carbon"

    status = nf90_inq_varid(ncid,"SoilTemp",varid)
    if(status /= nf90_noerr) print*, "Error inquiring soil temperature"
    status = nf90_get_var(ncid,varid,tsoil7)
    if(status /= nf90_noerr) print*,"Error reading soil temperature"
!    micglobal%tsoil=real(varx3db,kind=dp)

    status = nf90_inq_varid(ncid,"SoilMoist",varid)
    if(status /= nf90_noerr) print*, "Error inquiring soil moisture"
    status = nf90_get_var(ncid,varid,moist7)
    if(status /= nf90_noerr) print*,"Error reading soil moisture"
!    micglobal%moist=real(varx3db,kind=dp)

    status = nf90_inq_varid(ncid,"water_potential",varid)
    if(status /= nf90_noerr) print*, "Error inquiring soil matric potential"
    status = nf90_get_var(ncid,varid,watpot7)
    if(status /= nf90_noerr) print*,"Error reading soil matric potential"
!    micglobal%matpot=real(varx3db,kind=dp)

    status = nf90_inq_varid(ncid,"Leaf_fall",varid)
    if(status /= nf90_noerr) print*, "Error inquiring Leaf_fall"
    status = nf90_get_var(ncid,varid,varx2db)
    if(status /= nf90_noerr) print*,"Error reading Leaf_fall"
    micglobal%dleaf=real(varx2db,kind=dp)

    status = nf90_inq_varid(ncid,"Belowground_litter_fall",varid)
    if(status /= nf90_noerr) print*, "Error inquiring Belowground_litter_fall"
    status = nf90_get_var(ncid,varid,varx2db)
    if(status /= nf90_noerr) print*,"Error reading Belowground_litter_fall"
    micglobal%droot=real(varx2db,kind=dp)

    status = nf90_inq_varid(ncid,"non_leaf_aboveground_litterfall",varid)
    if(status /= nf90_noerr) print*, "Error inquiring non_leaf_aboveground_litterfall"
    status = nf90_get_var(ncid,varid,varx2db)
    if(status /= nf90_noerr) print*,"Error reading non_leaf_aboveground_litterfall"
    micglobal%dwood =real(varx2db,kind=dp)

    status = nf90_inq_varid(ncid,"Cluster",varid)
    if(status /= nf90_noerr) print*, "Error inquiring data Cluster"
    status = nf90_get_var(ncid,varid,ivarx1)
    if(status /= nf90_noerr) print*,"Error reading data Cluster"
    micglobal%bgctype = ivarx1
    micparam%bgctype  = ivarx1

    status = nf90_inq_varid(ncid,"Ald",varid)
    if(status /= nf90_noerr) print*, "Error inquiring data Ald"
    status = nf90_get_var(ncid,varid,fald)
    if(status /= nf90_noerr) print*,"Error reading data ald"

    status = nf90_inq_varid(ncid,"Alo",varid)
    if(status /= nf90_noerr) print*, "Error inquiring data Alo"
    status = nf90_get_var(ncid,varid,falo)
    if(status /= nf90_noerr) print*,"Error reading data alo"

    status = nf90_inq_varid(ncid,"Fed",varid)
    if(status /= nf90_noerr) print*, "Error inquiring data Fed"
    status = nf90_get_var(ncid,varid,ffed)
    if(status /= nf90_noerr) print*,"Error reading data Fed"

    status = nf90_inq_varid(ncid,"Feo",varid)
    if(status /= nf90_noerr) print*, "Error inquiring data Feo"
    status = nf90_get_var(ncid,varid,ffeo)
    if(status /= nf90_noerr) print*,"Error reading data Feo"

    ! Close netcdf file
    status = NF90_CLOSE(ncid)

   ! now calculate soil cluster using parameters for ORCHIDEE (jmodel=2)
    do np=1,mp
       do ns=1,ms
          micparam%csoilobs(np,ns) = real(fsoc7(np,ns),kind=dp)
       end do
    end do

    micglobal%bgctype = 0
    micparam%bgctype  = 0
    call cluster_hwsd(2,micglobal%bgctype,micparam%csoilobs,micglobal%clay,micglobal%silt,micglobal%ph,  &
                      real(fald,kind=dp),real(falo,kind=dp),real(ffed,kind=dp),real(ffeo,kind=dp),fcluster)

    micparam%bgctype  = fcluster
    micglobal%bgctype = fcluster

   ! get the ancient soc fraction for mp
     status = nf90_open(fanoc,nf90_nowrite,ncid)
     if(status /= nf90_noerr) print *, "Error opening fanoc.nc"
     status = nf90_inq_varid(ncid,"fanoc_mp",varid)
     if(status /= nf90_noerr) print *, "Error inquiring fanoc_mp"
     status = nf90_get_var(ncid,varid,fracaoc)
     if(status /= nf90_noerr) print *,"Error reading fanoc_mp"
     ! Close netcdf file
     status = NF90_CLOSE(ncid)

   ! if jmodel=3 use the annual modis NPP to scale the orchidee npp
    if(jmodel==3) then
       status = nf90_open(fmodis,nf90_nowrite,ncid)
       if(status /= nf90_noerr) print*, "Error opening modisnpp"

       ! get variables
       status = nf90_inq_varid(ncid,"npp",varid)
       if(status /= nf90_noerr) print*, "Error inquiring data modis_npp"
       status = nf90_get_var(ncid,varid,modisnpp)
       if(status /= nf90_noerr) print*,"Error reading data npp"
       ! Close netcdf file
       status = NF90_CLOSE(ncid)

       modisnpp_mp(:) = 0.0
       do np=1,mp
          ilonx=(micglobal%lon(np) + 179.75)/0.5 + 1
          jlatx=(89.75-micglobal%lat(np))/0.5    + 1
          modisnpp_mp(np) = max(100.0,modisnpp(ilonx,jlatx))
       end do
    end if

    do k=1,ntime
       micglobal%time(k)= real(k*1.0,kind=dp)
    end do

    ! print *, 'PFT=', micglobal%pft
    msite = 0
    do np=1, mp
       micglobal%siteid(np)  = np
       ! calculate mean bulk density
       micglobal%bulkd(np) = (bulkd(np,1)*zse(1)+bulkd(np,2)*zse(2)+bulkd(np,3)*zse(3)+bulkd(np,4)*zse(4)+bulkd(np,5)*zse(5) &
                             +bulkd(np,6)*zse(6)+bulkd(np,7)*zse(7))/sum(zse(1:7))

       micglobal%poros(np)   = 1.0 - micglobal%bulkd(np)/2650.0
       micparam%siteid(np)   = micglobal%siteid(np)
       micparam%pft(np)      = micglobal%pft(np)
       micparam%isoil(np)    = micglobal%isoil(np)
       micparam%sorder(np)   = micglobal%sorder(np)
       if(jmodel==1) then      !CABLE
          ipft =  micglobal%pft(np)
          if(ipft<1 .or. ipft >17) then
             print *, "PFT error at  np", jmodel,ipft,np
             stop
          end if
          micparam%xcnleaf(np)  = cnleaf1(ipft)
          micparam%xcnroot(np)  = cnroot1(ipft)
          micparam%xcnwood(np)  = cnwood1(ipft)
          micparam%fligleaf(np) = ligleaf1(ipft)
          micparam%fligroot(np) = ligroot1(ipft)
          micparam%fligwood(np) = ligwood1(ipft)
       end if
       if(jmodel==2 .or.jmodel==3) then      !ORCHIDEE
          ipft =  micglobal%pft(np)
          if(ipft<1 .or. ipft >19) then
             print *, "PFT error at  np", jmodel,ipft,np
             stop
          end if
          micparam%xcnleaf(np)  = cnleaf2(ipft)
          micparam%xcnroot(np)  = cnroot2(ipft)
          micparam%xcnwood(np)  = cnwood2(ipft)
          micparam%fligleaf(np) = ligleaf2(ipft)
          micparam%fligroot(np) = ligroot2(ipft)
          micparam%fligwood(np) = ligwood2(ipft)
       end if

       nsocobs=0

       do ns=1,ms

          micparam%csoilobs(np,ns) = real(fsoc7(np,ns),kind=dp)
          micparam%fracaoc(np,ns)  = real(fracaoc(np,ns),kind=dp)
          micglobal%tsoil(np,ns,:) = real(tsoil7(np,ns,:),kind=dp)
          micglobal%moist(np,ns,:) = real(moist7(np,ns,:),kind=dp)
          micglobal%matpot(np,ns,:)= real(watpot7(np,ns,:),kind=dp)
          ! filter out sites with SOC >120 gc/kg (organic soil: Lourenco et al. 2022)
          if(micparam%csoilobs(np,ns) >=120.0) then
             micglobal%area(np) = -1.0
          end if

          if(micparam%csoilobs(np,ns) >0.0 .and. micparam%csoilobs(np,ns) < 1000.0) nsocobs = nsocobs + 1

       end do

       ! using "micglobal%area" to filter out some sites
       micglobal%npp(np) = sum(micglobal%dleaf(np,:) + micglobal%dwood(np,:) + micglobal%droot(np,:))
       if(jmodel==3) then ! scale orchidee NPP using midNPP
          micglobal%dleaf(np,:) = micglobal%dleaf(np,:) * modisnpp_mp(np)/micglobal%npp(np)
          micglobal%dwood(np,:) = micglobal%dwood(np,:) * modisnpp_mp(np)/micglobal%npp(np)
          micglobal%droot(np,:) = micglobal%droot(np,:) * modisnpp_mp(np)/micglobal%npp(np)
          micglobal%npp(np) = sum(micglobal%dleaf(np,:) + micglobal%dwood(np,:) + micglobal%droot(np,:))
       end if

       if(micglobal%npp(np)<100.0 .or. micglobal%ph(np)<3.0 .or. nsocobs==0) micglobal%area(np) = -1.0

       if(micglobal%bgctype(np) ==bgcopt .and. micglobal%area(np) >0) msite = msite + 1
    end do    ! "np=1,mp"

    sitemax=300
    if(msite>2*sitemax) then

       intval = msite/sitemax; isite=0
       do np=1,mp
          if(micglobal%bgctype(np) == bgcopt .and.micglobal%area(np) > 0.0) then
             isite = isite +1
             if(int(isite/intval)*intval /= isite.or. isite>sitemax*intval) micglobal%area(np) = -1.0
          end if
          if(micglobal%area(np) > 0.0 .and. micglobal%bgctype(np) == bgcopt) then
             write(*,103) isite,np, micglobal%bgctype(np), micglobal%area(np),micglobal%npp(np),micglobal%ph(np)
          end if
       end do
    else

      isite=0
      do np=1,mp
         if(micglobal%area(np) > 0.0 .and. micglobal%bgctype(np) == bgcopt) then
            isite=isite+1
            write(*,103) isite,np,micglobal%bgctype(np),micglobal%area(np),micglobal%npp(np),micglobal%ph(np)
         end if
      end do
      if(isite<10) print *, "too few sites ", isite

    end if

    micglobal%avgts(:) = sum(sum(micglobal%tsoil(:,:,:),dim=3),dim=2)/real(ms*ntime)
    micglobal%avgms(:) = sum(sum(micglobal%moist(:,:,:),dim=3),dim=2)/real(ms*ntime)

    if(jglobal) then
       open(100,file="inputdata.txt")
       do np=1,mp
          write(100,101) micparam%siteid(np),micglobal%area(np),micparam%pft(np), &
          micparam%isoil(np),micparam%sorder(np),micparam%bgctype(np),   &
          micglobal%npp(np), &
          minval(micglobal%dleaf(np,:) + micglobal%dwood(np,:) + micglobal%droot(np,:)), &
          maxval(micglobal%dleaf(np,:) + micglobal%dwood(np,:) + micglobal%droot(np,:)), &
          micglobal%ph(np),micglobal%clay(np)+micglobal%silt(np),micglobal%bulkd(np), &
          micglobal%avgts(np),micglobal%avgms(np),sum(micparam%csoilobs(np,:)*zse(:))/sum(zse(:)), &
          micparam%fracaoc(np,1),micparam%fracaoc(np,3), micparam%fracaoc(np,ms)
       end do
       close(100)
    end if
101 format(i5,1x,f8.4,1x,4(i3,1x),30(f10.4,1x))
103 format(" run site", 3(i6,1x),10(f10.3,1x))

    deallocate(ivarx1,fcluster)
    deallocate(varx1float,varx1db,avgts,avgms)
    deallocate(varx2db,fsoc7)
    deallocate(tsoil7,moist7,watpot7)
    deallocate(fald,falo,ffed,ffeo)
    deallocate(fracaoc)
!    print *, 'exit getdata_hwsd'

end subroutine getdata_hwsd


subroutine screenout(runmodel,jmodel,bgcopt,xopt,cost)
    character(len=10),       intent(in) :: runmodel
    integer,                 intent(in) :: jmodel,bgcopt
    real(dp), dimension(16), intent(in) :: xopt
    real(dp),                intent(in) :: cost
    write(*,901) runmodel,jmodel,bgcopt,cost,xopt(1:14)
901 format(a10,2(i3,1x),f12.3,1x,14(f7.3,1x))

end subroutine screenout



   subroutine getdata_aust_dim(faustsoc,mpx,timex)
    character(len=140), intent(in)    :: faustsoc
    integer,            intent(inout) :: mpx,timex
    integer:: ncid,varid,status
   ! open .nc file
    status = nf90_open(faustsoc,nf90_nowrite,ncid)
    if(status /= nf90_noerr) print*, "Error opening faustsoc.nc"

    ! get dimensions
    status = nf90_inq_dimid(ncid,"msite",varid)
    if(status /= nf90_noerr) print*, "Error inquiring dimensions/nsite"
    status = nf90_inquire_dimension(ncid,varid,len=mpx)
    if(status /= nf90_noerr) print*,"Error dimensions/nsite"
    !
    status = nf90_inq_dimid(ncid,"mday",varid)
    if(status /= nf90_noerr) print*, "Error inquiring dimensions/ntime"
    status = nf90_inquire_dimension(ncid,varid,len=timex)
    if(status /= nf90_noerr) print*,"Error reading profile_id"

    ! Close netcdf file
    status = NF90_CLOSE(ncid)
   end subroutine  getdata_aust_dim

   subroutine getdata_aust(faustsoc,jglobal,bgcopt,jopt,jmodel,micparam,micglobal,zse)
    !use micglobal%area (area fraction) as a switch to run for selected sites during parameter optimization (jopt==0)
    !model only runs for those sites with micglobal%area(np) > 0.0
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
    character(len=140),           INTENT(IN) :: faustsoc
    logical,                      INTENT(IN) :: jglobal
    integer,                      INTENT(IN) :: bgcopt
    logical,                      INTENT(IN) :: jopt
    integer,                      INTENT(IN) :: jmodel
    TYPE(mic_parameter),          INTENT(INout) :: micparam
    TYPE(mic_global_input),       INTENT(INout) :: micglobal
    real(dp),                     INTENT(IN) :: zse(ms)
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
    print *, " calling getdata_aust"
    print *,"input file", faustsoc
    print *,"mp ms bgcopt=",    mp,ms,bgcopt

    status = nf90_open(faustsoc,nf90_nowrite,ncid)
    if(status /= nf90_noerr) print*, "Error opening faustsoc.nc"

    ! get variables
    status = nf90_inq_varid(ncid,"lat",varid)
    if(status /= nf90_noerr) print*, "Error inquiring data lat"
    status = nf90_get_var(ncid,varid,varx1float)
    if(status /= nf90_noerr) print*,"Error reading data lat"
    micglobal%lat = real(varx1float,kind=dp)

    status = nf90_inq_varid(ncid,"lon",varid)
    if(status /= nf90_noerr) print*, "Error inquiring data lon"
    status = nf90_get_var(ncid,varid,varx1float)
    if(status /= nf90_noerr) print*,"Error reading data lon"
    micglobal%lon=real(varx1float,kind=dp)

    status = nf90_inq_varid(ncid,"vtype",varid)
    if(status /= nf90_noerr) print*, "Error inquiring data PFT"
    status = nf90_get_var(ncid,varid,ivarx1)
    if(status /= nf90_noerr) print*,"Error reading data PFT"
    micglobal%pft = ivarx1

    status = nf90_inq_varid(ncid,"nsite",varid)
    if(status /= nf90_noerr) print*, "Error inquiring nsite"
    status = nf90_get_var(ncid,varid,ivarx1)
    if(status /= nf90_noerr) print*,"Error reading nsite"
    micglobal%siteid = ivarx1

    status = nf90_inq_varid(ncid,"ph",varid)
    if(status /= nf90_noerr) print*, "Error inquiring ph"
    status = nf90_get_var(ncid,varid,varx1float)
    if(status /= nf90_noerr) print*,"Error reading ph"
    micglobal%ph = real(varx1float,kind=dp)

    status = nf90_inq_varid(ncid,"clay",varid)
    if(status /= nf90_noerr) print*, "Error inquiring clay"
    status = nf90_get_var(ncid,varid,varx1float)
    if(status /= nf90_noerr) print*,"Error reading clay"
    micglobal%clay = real(varx1float,kind=dp)

    status = nf90_inq_varid(ncid,"silt",varid)
    if(status /= nf90_noerr) print*, "Error inquiring silt"
    status = nf90_get_var(ncid,varid,varx1float)
    if(status /= nf90_noerr) print*,"Error reading silt"
    micglobal%silt = real(varx1float,kind=dp)

    status = nf90_inq_varid(ncid,"bulkd",varid)
    if(status /= nf90_noerr) print*, "Error inquiring bulkd"
    status = nf90_get_var(ncid,varid,varx1float)
    if(status /= nf90_noerr) print*,"Error reading bulkd"
    micglobal%bulkd = real(varx1float,kind=dp)

    status = nf90_inq_varid(ncid,"soc",varid)
    if(status /= nf90_noerr) print*, "Error inquiring soc"
    status = nf90_get_var(ncid,varid,varx2float)
    if(status /= nf90_noerr) print*,"Error reading soc"
    soc3 = real(varx2float,kind=dp)

    status = nf90_inq_varid(ncid,"poc",varid)
    if(status /= nf90_noerr) print*, "Error inquiring poc"
    status = nf90_get_var(ncid,varid,varx1float)
    if(status /= nf90_noerr) print*,"Error reading poc"
    poc = real(varx1float,kind=dp)

    status = nf90_inq_varid(ncid,"hoc",varid)
    if(status /= nf90_noerr) print*, "Error inquiring hoc"
    status = nf90_get_var(ncid,varid,varx1float)
    if(status /= nf90_noerr) print*,"Error reading hoc"
    hoc = real(varx1float,kind=dp)

    status = nf90_inq_varid(ncid,"roc",varid)
    if(status /= nf90_noerr) print*, "Error inquiring roc"
    status = nf90_get_var(ncid,varid,varx1float)
    if(status /= nf90_noerr) print*,"Error reading roc"
    roc = real(varx1float,kind=dp)

    status = nf90_inq_varid(ncid,"npp",varid)
    if(status /= nf90_noerr) print*, "Error inquiring npp"
    status = nf90_get_var(ncid,varid,varx3float)
    if(status /= nf90_noerr) print*,"Error reading npp"
    npp10y = sum(real(varx3float,kind=dp),dim=2)/10.0  !(mp,ntime)

    status = nf90_inq_varid(ncid,"tsoil",varid)
    if(status /= nf90_noerr) print*, "Error inquiring tsoil"
    status = nf90_get_var(ncid,varid,varx3float)
    if(status /= nf90_noerr) print*,"Error reading tsoil"
    tsoil10y = sum(real(varx3float,kind=dp),dim=2)/10.0   !(mp,ntime)

    status = nf90_inq_varid(ncid,"moist10",varid)
    if(status /= nf90_noerr) print*, "Error inquiring moist10"
    status = nf90_get_var(ncid,varid,varx3float)
    if(status /= nf90_noerr) print*,"Error reading moist10"
    moist10y10 = sum(real(varx3float,kind=dp),dim=2)/10.0  !(mp,ntime)

    status = nf90_inq_varid(ncid,"moist100",varid)
    if(status /= nf90_noerr) print*, "Error inquiring moist100"
    status = nf90_get_var(ncid,varid,varx3float)
    if(status /= nf90_noerr) print*,"Error reading moist100"
    moist10y100= sum(real(varx3float,kind=dp),dim=2)/10.0 !(mp,ntime)

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
          end if
          if(ns==1) then
             micglobal%moist(np,ns,:)    = moist10y10(np,:)
          else
             micglobal%moist(np,ns,:) = moist10y100(np,:)
          end if
          micglobal%tsoil(np,ns,:)  = tsoil10y(np,:)
          micparam%csoilobsp(np,ns) = poc(np)   ! assign layer POC conc to all layers
          micparam%csoilobsm(np,ns) = hoc(np)   ! assign layer POC conc to all layers
       end do
       micglobal%avgts(np)   = sum(micglobal%tsoil(np,1,:))/real(ntime)
       micglobal%avgms(np)   = sum(micglobal%moist(np,2,:))/real(ntime)
       micglobal%npp(np)     = sum(npp10y(np,:))
    end do

    do k=1,ntime
       micglobal%time(k)= real(k*1.0,kind=dp)
    end do

    if(jglobal) then
       open(100,file="inputdata.txt")
       do np=1,mp
          write(100,101) micparam%siteid(np),micglobal%area(np),micparam%pft(np), &
          micparam%isoil(np),micparam%sorder(np),micparam%bgctype(np),   &
          micglobal%npp(np),sum(micglobal%dleaf(np,:))+sum(micglobal%dwood(np,:))+sum(micglobal%droot(np,:)), &
          micglobal%ph(np),micglobal%clay(np)+micglobal%silt(np),micglobal%bulkd(np), &
          micglobal%avgts(np),micglobal%avgms(np),sum(micparam%csoilobs(np,:)*zse(:))/sum(zse(:))
       end do
       close(100)
    end if
101 format(i5,1x,f8.4,1x,4(i3,1x),20(f10.4,1x))
103 format(" run site", 3(i6,1x),10(f10.3,1x))

    deallocate(ivarx1)
    deallocate(varx1float,avgts,avgms,soc3,poc,hoc,roc)
    deallocate(varx2float)
    deallocate(varx3float,npp10y,tsoil10y,moist10y10,moist10y100)
!    print *, 'exit getdata_hwsd'

end subroutine getdata_aust

end module mesc_inout_module

