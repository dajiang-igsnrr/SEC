!> declarre all variables and assign values to model constants
!! allocate   variables
!! deallocate variables 
!!

! this version is designed to work with monthly focings from CABLE/ORCHIDEE model
!+==========github checked version 0.0
! tasks to do
! (6) add comments and tidy up the codes
!
! tasks done
! (1) modify "bgc_fractions" to include woody litter
! (1) write/read RESTART file
! (3) add dimension (mpft) to "mic_param_xscale"
! (5) assigm the default "micpxdef" values in "function funct"
! (2) write output file
! (4) read in a PFT-dependent parameter table
! (7) check the CUE _T depenendence (switch it off)
! (8) check the soil moisture: MIMICS used MILLENNIAL2 function; the combined model uses Yan et al. (2018)1G
! (9) microbial turnover rates: varying with NPP (MIMICS); not varying with NPP (the combined model)
! (10) use bgctype based cluster analysis to group soils "micglobal%bgctype(mbgc)"
! (11) need to "functn_frc" and "functn_c14" working with new approach
!
! this version run the model for global sensitivity analysis for 
! for kinetcs=3 (the combined model)                        [defualt values] (scaling factor range)
!  1: xav:      scaling factor for V                            [1]              (0-30)               8.0e-6
!  2: xak:      scaling factor for K                            [1]              (0-30)               10.0
!  3: xfm:      scaling factor for fm                           [1]              (0.1-5.0)            0.05
!  4: xfs:      scaling factor for fs                           [1]              (0.1-5.0)            0.05
!  5: xtvmic:   scaling factor for tvmicR (0-10)                [1]              (0.1,10)             sqrt(NPPP)
!  5: xtvmic :  scaling factor for tvmicK (0-10)                [1]              =xtvmicR             sqrt(NPPP)
!  6: xtvp:     scaling factor for tvppool(0-10)                [1]            (0.1,10)               1/25  year-1    ! rate of disaggregation
!  7: xtvc:     scaling factor for tvcpool(0-10)                [1]            (0.1,10)               1/100 year-1    ! rate of MAOC breakdown
!  8: xtvac:    scaling factor for tvac   (0-10)                [1]            (0.1,10)               1/2.0 year-1    ! leaching rate
!  9: xkba:     scaling factor for kba    (0.2-5)               [1]            (0.5,10)               2.0             ! ratio of adsorption/desoprtion
! 10: xqmaxcoeff:coefficient of Qmax on clay+silt (0.4-0.8)     [1]            (0.5,5.0)              0.6
! 11: xdiffsoc:  SOC diffusion/bioturbation rate                [1]            (0.1,10.0)             (1.0/24.0)* 2.74e-3 (1/day)
! 12: xNPP:      carbon input                                   [1]            (0.5,2.0)              NPP
! 13: xrootbeta: scaling for depth-dependent of root C input    [1]            (0.5,5.0)              2.0
! 14: xvmaxbeta: scaling for depth-dependent of vmax            [1]            (0.5,5.0)              2.0
! 15: xfp2ax:    scaling factor for fp2ax                       [1]            (0.5-2.0)                              ! not used 
! 16: xdesorp:   desorption coefficient (kinetics=1,2)          [1]            (0.1,10.0)                             ! not used   
! 17: xbeta:     beta parameter 
module mic_variable
  use mic_constant
  implicit none
  save

  type mic_param_xscale
    ! parameter scaling
     real(r_2),dimension(:), allocatable :: &
      xav,         &
      xak,         &
      xfp2ax,      &
      xfm,         &
      xfs,         &
      xtvmic,      &
      xtvp,        &
      xtvc,        &
      xtvac,       &
      xkba,        &
      xqmaxcoeff,  &
      xbeta,       &
      xdiffsoc,    &
      xnpp,        &
      xdesorp,     &
      xrootbeta,   &
      xvmaxbeta        
  end type mic_param_xscale
  
  TYPE mic_param_default
     !default values for Michaelis-Menten K
     real(r_2)  ::   &
     sk =0.017,      &
     skx=0.027,      &
     ak = 10.0,      &
     bk = 3.19,      &
     xk1 =8.0,       &
     xk2 =2.0,       &
     xk3 =4.0,       &
     xj1 =2.0,       &
     xj2 =4.0,       &
     xj3 =6.0 
     !default values for Michaelis-Menten Vmax
     real(r_2)  ::   &     
     sv = 0.063,     &
     av = 10.0*8.0e-6,    &
     bv = 5.47,      &
     xv1= 10.0,      &
     xv2= 2.0,       &
     xv3= 10.0,      &
     xw1= 3.0,       &
     xw2= 3.0,       &
     xw3= 2.0
     ! default values of MM kinetics (Wieder et al. 2015)
     real(r_2)  ::   & 
     Q1=4.0,         &
     Q2=4.0,         &
     fm=0.1,         &
     fs=0.1
     ! microbial turnover rate parameter values (Wieder et al. (2015))
     real(r_2)  ::        &   
     xtv      = 100.0,    &
     betamic  = 2.0,      &
     tvmicR   = 0.00052,  &
     tvmicK   = 0.00024     
     !dependence on the partitioning of necromass on soil clay and substrate quality
     real(r_2)  ::   &    
     fmicsom1=0.432, &
     fmicsom2=0.098, &
     fmicsom3=10.56, &
     fmicsom4=29.78, &
     fmicsom5=2.61
     !microbial carbon use efficiency
     real(r_2)  ::          &    
     cuemax    = 0.80,      &
     cue_coef1 = 0.66,      &
     cue_coef2 = 1.23,      &
     epislon1 = 0.5,        &
     epislon2 = 0.25,       &
     epislon3 = 0.7,        &
     epislon4 = 0.35
     !adsorption dependence on soil pH (!Table A1 Abramoff et al. (2022))
     real(r_2)  ::          & 
     phcoeff1 = 0.2429,      &      
     phcoeff2 = -0.3632        
!     phcoeff1 = 0.186,      &      
!     phcoeff2 = 0.216   
     !dependence on soil moisture  Yan et al (2018)
     real(r_2)  ::          & 
     smkdesorp = 0.1,       & 
     smexpns   = 2.0,       & 
     smexpb    = 0.75 
     ! dependence of Qmax on soil texture
     real(r_2) :: qmaxcoeff = 0.4 * 0.5   ! Georgiou et al. (2022)
     ! SOC diffusion coefficient. see Table 1,Camino-Serrano et al. (2018)
     real(r_2):: diffsoc  =( 5.0/24.0)* 2.74e-3  !cm2/hour
     ! kinetic parameter values of rkinetics=3. Table A1  Abramoff2022
     real(r_2) ::           &
     kadsorpx = 0.001,      &
     kbax     = 6.0,        &
     fp2ax    = 1.143 * 0.33,       &
     tvcpoolx = 0.02*(1.0/1.60)*1.0/(365.0*24.0), &
     tvppoolx = 0.10*(1.0/0.44)*1.0/(365.0*24.0), &
     tvacx = 0.0015/24.0,        &
     rootbeta = 1.0,             &
     vmaxbeta = 0.5

   ! previous values
   !   rootbeta = 2.0,          &
   !   tvcpoolx = 0.102 * 0.02/24.0/2.0,  &
   !   tvppoolx = 4.705 * 0.019/24./10.00, &
   !   tvacx    = 0.1   * 0.0015/24.0,&                                                
  END TYPE mic_param_default

  TYPE mic_parameter
  real(r_2), dimension(:,:),    allocatable  :: K1,K2,K3,J1,J2,J3
  real(r_2), dimension(:,:),    allocatable  :: V1,V2,V3,W1,W2,W3
  real(r_2), dimension(:,:),    allocatable  :: desorp
  real(r_2), dimension(:,:),    allocatable  :: Q1,Q2,fm,fs
  real(r_2), dimension(:,:),    allocatable  :: mgeR1,mgeR2,mgeR3,mgeK1,mgeK2,mgeK3
  real(r_2), dimension(:,:),    allocatable  :: tvmicR,tvmicK,betamicR,betamicK
  real(r_2), dimension(:,:),    allocatable  :: fmetave
  real(r_2), dimension(:,:,:),  allocatable  :: cn_r
  real(r_2), dimension(:,:),    allocatable  :: fr2p,fk2p,fr2c,fk2c,fr2a,fk2a
  real(r_2), dimension(:),      allocatable  :: xcnleaf,xcnroot,xcnwood,fligleaf,fligroot,fligwood
  real(r_2), dimension(:),      allocatable  :: diffsocx
  ! additional parameters for kinetics3 
  real(r_2), dimension(:,:),    allocatable  :: kdesorp   !mg C cm-3 hour-1
  real(r_2), dimension(:,:),    allocatable  :: kadsorp   !1/hour
  real(r_2), dimension(:,:),    allocatable  :: fp2a
  real(r_2), dimension(:,:),    allocatable  :: tvcpool   !1/hour
  real(r_2), dimension(:,:),    allocatable  :: tvppool   !1/hour
  real(r_2), dimension(:,:),    allocatable  :: tvac      !1/hour (leaching rate coefficient)
  real(r_2), dimension(:,:),    allocatable  :: qmaxcoeff !coefficient relating qmax to soil clay+silt 
  
  ! the following are alrealy available in CABLE
  integer,   dimension(:),      allocatable  :: pft,bgctype,isoil,sorder,region,siteid,dataid
  real(r_2), dimension(:,:),    allocatable  :: sdepth,fracroot
  real(r_2), dimension(:,:),    allocatable  :: csoilobs,csoilobsp,csoilobsm
  real(r_2), dimension(:),      allocatable  :: c14soilobsp,c14soilobsm     !14C obs
  real(r_2), dimension(:,:,:),  allocatable  :: c14atm         !atmospheric 14C
  integer,   dimension(:),      allocatable  :: nyc14obs       !year at which 14C was observed
  integer,   dimension(:),      allocatable  :: top,bot
  
  END TYPE mic_parameter

  TYPE mic_input
  real(r_2), dimension(:,:),    allocatable  :: tavg,wavg,tair,ph,clay,silt,porosity,bulkd,matpot
  real(r_2), dimension(:),      allocatable  :: dleaf,dwood,droot
  real(r_2), dimension(:,:),    allocatable  :: cinputm
  real(r_2), dimension(:,:),    allocatable  :: cinputs
  real(r_2), dimensioN(:),      allocatable  :: fcnpp
  
  END TYPE mic_input

  TYPE mic_global_input
    real(r_2), dimension(:),      allocatable  :: lon,lat,time
    integer,   dimension(:),      allocatable  :: pft,bgctype,isoil,sorder,siteid
    real(r_2), dimension(:),      allocatable  :: area,npp,ph,clay,silt,poros,bulkd,avgts,avgms
    real(r_2), dimension(:,:),    allocatable  :: patchfrac
    real(r_2), dimension(:,:,:),  allocatable  :: tsoil,moist,matpot
    real(r_2), dimension(:),      allocatable  :: ligleaf,ligwood,ligroot
    real(r_2), dimension(:,:),    allocatable  :: dleaf,dwood,droot,cnleaf,cnwood,cnroot
  END TYPE mic_global_input
  
  TYPE mic_output
  real(r_2), dimension(:),    allocatable  :: fluxcinput   
  real(r_2), dimension(:),    allocatable  :: fluxrsoil   
  real(r_2), dimension(:),    allocatable  :: fluxcleach
  END TYPE mic_output
  
  TYPE mic_cpool
  real(r_2), dimension(:,:,:),  allocatable  :: cpool
  real(r_2), dimension(:,:,:),  allocatable  :: cpooleq
  real(r_2), dimension(:),      allocatable  :: cpooleqp,cpooleqm,c12pooleqp,c12pooleqm
  END TYPE mic_cpool
 
  TYPE mic_npool
  real(r_2), dimension(:,:),    allocatable  :: mineralN
  END TYPE mic_npool 
  
 
 CONTAINS

  subroutine mic_allocate_parameter(mpft,mbgc,mp,ms,micpxdef,micparam)
   IMPLICIT NONE
   TYPE(mic_parameter),    INTENT(INOUT)  :: micparam
   TYPE(mic_param_xscale), INTENT(INOUT)  :: micpxdef
   integer  mpft,mbgc,mp,ms

    allocate(micpxdef%xav(mbgc),  &
      micpxdef%xak(mbgc),         &
      micpxdef%xfp2ax(mbgc),      &
      micpxdef%xfm(mbgc),         &
      micpxdef%xfs(mbgc),         &
      micpxdef%xtvmic(mbgc),      &
      micpxdef%xtvp(mbgc),        &
      micpxdef%xtvc(mbgc),        &
      micpxdef%xtvac(mbgc),       &
      micpxdef%xkba(mbgc),        &
      micpxdef%xqmaxcoeff(mbgc),  &
      micpxdef%xbeta(mbgc),       &
      micpxdef%xdiffsoc(mbgc),    &
      micpxdef%xnpp(mpft),        &
      micpxdef%xdesorp(mbgc),     &
      micpxdef%xrootbeta(mpft),   &
      micpxdef%xvmaxbeta(mbgc))

    allocate(micparam%K1(mp,ms),  &
             micparam%K2(mp,ms),  & 
             micparam%K3(mp,ms),  & 
             micparam%J1(mp,ms),  & 
             micparam%J2(mp,ms),  & 
             micparam%J3(mp,ms),  & 
             micparam%V1(mp,ms),  & 
             micparam%V2(mp,ms),  & 
             micparam%V3(mp,ms),  & 
             micparam%W1(mp,ms),  & 
             micparam%W2(mp,ms),  & 
             micparam%W3(mp,ms),  & 
             micparam%desorp(mp,ms),  &
             micparam%Q1(mp,ms),      &
             micparam%Q2(mp,ms),      &
             micparam%fm(mp,ms),      &
             micparam%fs(mp,ms),      &
             micparam%mgeR1(mp,ms),   & 
             micparam%mgeR2(mp,ms),   & 
             micparam%mgeR3(mp,ms),   & 
             micparam%mgeK1(mp,ms),   & 
             micparam%mgeK2(mp,ms),   & 
             micparam%mgeK3(mp,ms),   & 
             micparam%fmetave(mp,ms), &
             micparam%tvmicR(mp,ms),  &
             micparam%tvmicK(mp,ms),  &
             micparam%betamicR(mp,ms),     &
             micparam%betamicK(mp,ms),     &
             micparam%cn_r(mp,ms,mcpool),  &
             micparam%fr2p(mp,ms),   & 
             micparam%fk2p(mp,ms),   & 
             micparam%fr2c(mp,ms),   & 
             micparam%fk2c(mp,ms),   &
             micparam%fr2a(mp,ms),   & 
             micparam%fk2a(mp,ms))

    allocate(micparam%xcnleaf(mp),   &
             micparam%xcnroot(mp),   &
             micparam%xcnwood(mp),   &
             micparam%fligleaf(mp),  &
             micparam%fligroot(mp),  &
             micparam%fligwood(mp),  &
             micparam%diffsocx(mp))

    allocate(micparam%pft(mp),       &
             micparam%bgctype(mp),   &
             micparam%isoil(mp),     &
             micparam%sorder(mp),    &
             micparam%region(mp),    &
             micparam%siteid(mp),    &
             micparam%dataid(mp))

    allocate(micparam%sdepth(mp,ms),   &
             micparam%fracroot(mp,ms), &
             micparam%csoilobs(mp,ms), &
             micparam%csoilobsp(mp,ms), &
             micparam%csoilobsm(mp,ms), &
             micparam%c14soilobsp(mp), &
             micparam%c14soilobsm(mp), &
             micparam%c14atm(79,5,2),   &
             micparam%nyc14obs(mp),    &
             micparam%top(mp),        &
             micparam%bot(mp))

! additional variables for kinetics3              
    allocate(micparam%kdesorp(mp,ms), &
             micparam%kadsorp(mp,ms), &
             micparam%fp2a(mp,ms),    &
             micparam%tvcpool(mp,ms), &
             micparam%tvppool(mp,ms), & 
             micparam%tvac(mp,ms),    &
             micparam%qmaxcoeff(mp,ms))
  end subroutine mic_allocate_parameter
  
  subroutine mic_allocate_input(mp,ms,nlon,nlat,ntime,micinput,micglobal)
   IMPLICIT NONE
   integer mp,ms,nlon,nlat,ntime
   TYPE(mic_input),        INTENT(INOUT)  :: micinput
   TYPE(mic_global_input), INTENT(INOUT)  :: micglobal

    allocate(micinput%tavg(mp,ms),    &
             micinput%wavg(mp,ms),    &
             micinput%ph(mp,ms),      &
             micinput%clay(mp,ms),    &
             micinput%silt(mp,ms),    &
             micinput%bulkd(mp,ms),   &
             micinput%porosity(mp,ms),&
             micinput%matpot(mp,ms),  &
             micinput%tair(mp,365),   &
             micinput%fcnpp(mp),      &
             micinput%dleaf(mp),      &
             micinput%dwood(mp),      &
             micinput%droot(mp),      &
             micinput%cinputm(mp,ms), &
             micinput%cinputs(mp,ms) )

    allocate(micglobal%lon(mp),             &
             micglobal%lat(mp),             &
             micglobal%time(ntime),         &
             micglobal%pft(mp),             &
             micglobal%bgctype(mp),         &             
             micglobal%isoil(mp),           &
             micglobal%sorder(mp),          &
             micglobal%siteid(mp),          &
             micglobal%area(mp),            &             
             micglobal%patchfrac(mp,mpft),  &       
             micglobal%npp(mp),             &   
             micglobal%ph(mp),              &
             micglobal%clay(mp),            &
             micglobal%silt(mp),            &
             micglobal%poros(mp),           &
             micglobal%bulkd(mp),           &
             micglobal%avgts(mp),           &
             micglobal%avgms(mp),           &
             micglobal%tsoil(mp,ms,ntime),  &
             micglobal%moist(mp,ms,ntime),  &             
             micglobal%matpot(mp,ms,ntime), &
             micglobal%ligleaf(mp),         &
             micglobal%ligwood(mp),         &
             micglobal%ligroot(mp),         &
             micglobal%dleaf(mp,ntime),     &
             micglobal%dwood(mp,ntime),     &
             micglobal%droot(mp,ntime),     &
             micglobal%cnleaf(mp,ntime),    &
             micglobal%cnwood(mp,ntime),    &
             micglobal%cnroot(mp,ntime))       
  end subroutine mic_allocate_input
  
  subroutine mic_allocate_output(mp,micoutput)
   IMPLICIT NONE
   TYPE(mic_output), INTENT(INOUT)  :: micoutput
   integer  mp

   allocate(micoutput%fluxcinput(mp))   
   allocate(micoutput%fluxrsoil(mp))
   allocate(micoutput%fluxcleach(mp))

 end subroutine mic_allocate_output

 subroutine mic_allocate_cpool(mp,ms,miccpool)
   IMPLICIT NONE
   integer mp,ms
   TYPE(mic_cpool), INTENT(INOUT)  :: miccpool
   allocate(miccpool%cpool(mp,ms,mcpool), &
            miccpool%cpooleq(mp,ms,mcpool), &
            miccpool%cpooleqp(mp),   &
            miccpool%cpooleqm(mp),  &
            miccpool%c12pooleqp(mp), &
            miccpool%c12pooleqm(mp)) 
 end subroutine mic_allocate_cpool 

 
  subroutine mic_allocate_npool(mp,ms,micnpool)
   IMPLICIT NONE
   integer mp,ms
   TYPE(mic_npool), INTENT(INOUT)  :: micnpool

   ALLOCATE(micnpool%mineralN(mp,ms))
   
  end subroutine mic_allocate_npool 
  
  ! deallocate to free up storage
  
  subroutine mic_deallocate_parameter(mpft,mbgc,mp,ms,micpxdef,micparam)
   IMPLICIT NONE
   TYPE(mic_parameter), INTENT(INOUT)     :: micparam
   TYPE(mic_param_xscale), INTENT(INOUT)  :: micpxdef
   integer  mpft,mbgc,mp,ms

    deallocate(micpxdef%xav,  &
      micpxdef%xak,         &
      micpxdef%xfp2ax,      &
      micpxdef%xfm,         &
      micpxdef%xfs,         &
      micpxdef%xtvmic,      &
      micpxdef%xtvp,        &
      micpxdef%xtvc,        &
      micpxdef%xtvac,       &
      micpxdef%xkba,        &
      micpxdef%xqmaxcoeff,  &
      micpxdef%xbeta,       &
      micpxdef%xdiffsoc,    &
      micpxdef%xnpp,        &
      micpxdef%xdesorp,     &
      micpxdef%xrootbeta,   &
      micpxdef%xvmaxbeta)


    deallocate(micparam%K1,  &
             micparam%K2,  & 
             micparam%K3,  & 
             micparam%J1,  & 
             micparam%J2,  & 
             micparam%J3,  & 
             micparam%V1,  & 
             micparam%V2,  & 
             micparam%V3,  & 
             micparam%W1,  & 
             micparam%W2,  & 
             micparam%W3,  & 
             micparam%desorp,  &
             micparam%Q1,      &
             micparam%Q2,      &
             micparam%fm,      &
             micparam%fs,      &
             micparam%mgeR1,   & 
             micparam%mgeR2,   & 
             micparam%mgeR3,   & 
             micparam%mgeK1,   & 
             micparam%mgeK2,   & 
             micparam%mgeK3,   & 
             micparam%fmetave, &
             micparam%tvmicR,  &
             micparam%tvmicK,  &
             micparam%betamicR,     &
             micparam%betamicK,     &
             micparam%cn_r,   &
             micparam%fr2p,   & 
             micparam%fk2p,   & 
             micparam%fr2c,   & 
             micparam%fk2c,   &
             micparam%fr2a,   & 
             micparam%fk2a)

    deallocate(micparam%xcnleaf,   &
             micparam%xcnroot,   &
             micparam%xcnwood,   &
             micparam%fligleaf,  &
             micparam%fligroot,  &
             micparam%fligwood,  &
             micparam%diffsocx)

    deallocate(micparam%pft,     &
             micparam%bgctype,     &
             micparam%isoil,     &
             micparam%sorder,    &    
             micparam%region,    &
             micparam%siteid)

    deallocate(micparam%sdepth,   &
             micparam%fracroot,   &
             micparam%csoilobs,   &
             micparam%csoilobsp,  &
             micparam%csoilobsm,  &
             micparam%c14soilobsp,&
             micparam%c14soilobsm,&
             micparam%c14atm,     &
             micparam%nyc14obs,   &
             micparam%top,        &
             micparam%bot)

! additional variables for kinetics3              
    deallocate(micparam%kdesorp,  &
             micparam%kadsorp,  &
             micparam%fp2a,     &
             micparam%tvcpool,  &
             micparam%tvppool,  & 
             micparam%tvac,     &
             micparam%qmaxcoeff)
  end subroutine mic_deallocate_parameter
  
  subroutine mic_deallocate_input(mp,ms,nlon,nlat,ntime,micinput,micglobal)
   IMPLICIT NONE
   integer mp,ms,nlon,nlat,ntime
   TYPE(mic_input), INTENT(INOUT)        :: micinput
   TYPE(mic_global_input), INTENT(INOUT) :: micglobal

    deallocate(micinput%tavg,    &
               micinput%wavg,    &
               micinput%ph,      &
               micinput%clay,    &
               micinput%silt,    &
               micinput%bulkd,   &
               micinput%porosity,&
               micinput%tair,    &
               micinput%fcnpp,   &
               micinput%dleaf,   &
               micinput%dwood,   &
               micinput%droot,   &
               micinput%cinputm, &
               micinput%cinputs)

               
    deallocate(micglobal%lon,     &
               micglobal%lat,     &
               micglobal%time,    &
               micglobal%pft,     &
               micglobal%bgctype, &               
               micglobal%isoil,   &
               micglobal%sorder,  &
               micglobal%siteid,  &
               micglobal%area,    &             
               micglobal%patchfrac, &         
               micglobal%npp,     &      
               micglobal%ph,      &
               micglobal%clay,    &
               micglobal%silt,    &
               micglobal%poros,   &
               micglobal%bulkd,   &
               micglobal%avgts,   &               
               micglobal%avgms,   &   
               micglobal%tsoil,   &
               micglobal%moist,   &             
               micglobal%matpot,  &
               micglobal%ligleaf, &
               micglobal%ligwood, &
               micglobal%ligroot, &
               micglobal%dleaf,   &
               micglobal%dwood,   &
               micglobal%droot,   &
               micglobal%cnleaf,  &
               micglobal%cnwood,  &
               micglobal%cnroot)       
             
  end subroutine mic_deallocate_input
  
  subroutine mic_deallocate_output(mp,micoutput)
   implicit none
   type(mic_output), intent(inout)  :: micoutput
   integer mp
    deallocate(micoutput%fluxcinput)   
    deallocate(micoutput%fluxrsoil)
    deallocate(micoutput%fluxcleach)
 
  end subroutine mic_deallocate_output

 subroutine mic_deallocate_cpool(mp,ms,miccpool)
   implicit none
   integer mp,ms
   type(mic_cpool), intent(inout)  :: miccpool
   deallocate(miccpool%cpool,  &
              miccpool%cpooleq, &
              miccpool%cpooleqp, &
              miccpool%cpooleqm, &
              miccpool%c12pooleqp,&
              miccpool%c12pooleqm) 
    
 end subroutine mic_deallocate_cpool 

 
  subroutine mic_deallocate_npool(mp,ms,micnpool)
   implicit none
   integer mp,ms
   type(mic_npool), intent(inout)  :: micnpool

   deallocate(micnpool%mineralN)
   
  end subroutine mic_deallocate_npool   
  
end module mic_variable