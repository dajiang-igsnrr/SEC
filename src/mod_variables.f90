!> Declares all model variables and provides allocation/deallocation routines.
!>
!> Defines derived types for model parameters, inputs, outputs, C/N pools,
!> and default parameter values. Provides paired allocation and deallocation
!> subroutines for each type to manage dynamic array storage.
module mic_variable
  use precision_module, only : dp
  use mic_constant, only : mcpool
  implicit none

  ! All module members are public by default
  public

  save

  !> Scaling factors for model parameters, per BGC type.
  type mic_param_xscale
      real(dp),dimension(:), allocatable :: xav          !! Vmax scaling factor [1] (0-30)
      real(dp),dimension(:), allocatable :: xak          !! Km scaling factor [1] (0-30)
      real(dp),dimension(:), allocatable :: xfp2ax       !! fp2a scaling factor [1] (0.5-2.0)
      real(dp),dimension(:), allocatable :: xfm          !! fm scaling factor [1] (0.1-5.0)
      real(dp),dimension(:), allocatable :: xfs          !! fs scaling factor [1] (0.1-5.0)
      real(dp),dimension(:), allocatable :: xtvmic       !! microbial turnover rate scaling [1] (0.1-10)
      real(dp),dimension(:), allocatable :: xtvp         !! POC disaggregation rate scaling [1] (0.1-10)
      real(dp),dimension(:), allocatable :: xtvc         !! MAOC breakdown rate scaling [1] (0.1-10)
      real(dp),dimension(:), allocatable :: xtvac        !! leaching rate scaling [1] (0.1-10)
      real(dp),dimension(:), allocatable :: xkba         !! adsorption/desorption ratio scaling [1] (0.5-10)
      real(dp),dimension(:), allocatable :: xqmaxcoeff   !! Qmax-clay+silt coefficient scaling [1] (0.5-5.0)
      real(dp),dimension(:), allocatable :: xbeta        !! beta parameter scaling
      real(dp),dimension(:), allocatable :: xdiffsoc     !! SOC diffusion/bioturbation scaling [1] (0.1-10.0)
      real(dp),dimension(:), allocatable :: xnpp         !! carbon input scaling [1] (0.5-2.0)
      real(dp),dimension(:), allocatable :: xdesorp      !! desorption coefficient scaling [1] (0.1-10.0)
      real(dp),dimension(:), allocatable :: xrootbeta    !! depth-dependent root C input scaling [1] (0.5-5.0)
      real(dp),dimension(:), allocatable :: xvmaxbeta    !! depth-dependent Vmax scaling [1] (0.5-5.0)
  end type mic_param_xscale

  !> Default parameter values for model calibration.
  !>
  !> Sensitivity analysis ranges (Abramoff et al. 2022):
  !> xav (0-30), xak (0-30), xfm (0.1-5.0), xfs (0.1-5.0), xtvmic (0.1-10),
  !> xtvp (0.1-10), xtvc (0.1-10), xtvac (0.1-10), xkba (0.5-10),
  !> xqmaxcoeff (0.5-5.0), xdiffsoc (0.1-10.0), xnpp (0.5-2.0),
  !> xrootbeta (0.5-5.0), xvmaxbeta (0.5-5.0), xfp2ax (0.5-2.0),
  !> xdesorp (0.1-10.0)
  type mic_param_default
      real(dp)  :: sk =0.017       !! Km soil depth coefficient
      real(dp)  :: skx=0.027       !! Km soil texture coefficient
      real(dp)  :: ak = 10.0       !! Km baseline
      real(dp)  :: bk = 3.19       !! Km depth decay
      real(dp)  :: xk1 =8.0        !! Km param 1
      real(dp)  :: xk2 =2.0        !! Km param 2
      real(dp)  :: xk3 =4.0        !! Km param 3
      real(dp)  :: xj1 =2.0        !! Km param 1 substr1
      real(dp)  :: xj2 =4.0        !! Km param 2 substr2
      real(dp)  :: xj3 =6.0        !! Km param 3 substr3
      real(dp)  :: sv = 0.063      !! Vmax soil depth coefficient
      real(dp)  :: av = 10.0*8.0e-6 !! Vmax baseline
      real(dp)  :: bv = 5.47       !! Vmax depth decay
      real(dp)  :: xv1= 10.0       !! Vmax param 1
      real(dp)  :: xv2= 2.0        !! Vmax param 2
      real(dp)  :: xv3= 10.0       !! Vmax param 3
      real(dp)  :: xw1= 3.0        !! Vmax param 1 substr1
      real(dp)  :: xw2= 3.0        !! Vmax param 2 substr2
      real(dp)  :: xw3= 2.0        !! Vmax param 3 substr3
      real(dp)  :: Q1=4.0          !! temperature sensitivity Q10_1
      real(dp)  :: Q2=4.0          !! temperature sensitivity Q10_2
      real(dp)  :: fm=0.1          !! substrate quality modifier 1
      real(dp)  :: fs=0.1          !! substrate quality modifier 2
      real(dp)  :: xtv      = 100.0     !! microbial pool turnover base
      real(dp)  :: betamic  = 2.0      !! microbial turnover beta
      real(dp)  :: tvmicR   = 0.00052  !! microbial turnover rate R
      real(dp)  :: tvmicK   = 0.00024  !! microbial turnover rate K
      real(dp)  :: fmicsom1=0.432  !! necromass partitioning coeff 1
      real(dp)  :: fmicsom2=0.098  !! necromass partitioning coeff 2
      real(dp)  :: fmicsom3=10.56  !! necromass partitioning coeff 3
      real(dp)  :: fmicsom4=29.78  !! necromass partitioning coeff 4
      real(dp)  :: fmicsom5=2.61   !! necromass partitioning coeff 5
      real(dp)  :: cuemax    = 0.80  !! max carbon use efficiency
      real(dp)  :: cue_coef1 = 0.66  !! CUE coefficient 1
      real(dp)  :: cue_coef2 = 1.23  !! CUE coefficient 2
      real(dp)  :: epislon1 = 0.5    !! CUE epsilon 1
      real(dp)  :: epislon2 = 0.25   !! CUE epsilon 2
      real(dp)  :: epislon3 = 0.7    !! CUE epsilon 3
      real(dp)  :: epislon4 = 0.35   !! CUE epsilon 4
      real(dp)  :: phcoeff1 = 0.2429    !! adsorption pH coefficient 1
      real(dp)  :: phcoeff2 = -0.3632   !! adsorption pH coefficient 2
      real(dp)  :: smkdesorp = 0.1   !! soil moisture desorption threshold
      real(dp)  :: smexpns   = 2.0   !! soil moisture exponent NS
      real(dp)  :: smexpb    = 0.75  !! soil moisture exponent B
      real(dp)  :: qmaxcoeff = 0.4 * 0.5 !! Qmax-clay+silt coefficient
      real(dp)  :: diffsoc  =( 5.0/24.0)* 2.74e-3 !! SOC diffusion [cm2/hour]
      real(dp)  :: kadsorpx = 0.001  !! adsorption rate coefficient
      real(dp)  :: kbax     = 6.0    !! adsorption/desorption ratio
      real(dp)  :: fp2ax    = 1.143 * 0.33 !! fraction DOC to POC
      real(dp)  :: tvcpoolx = 0.02*(1.0/1.60)*1.0/(365.0*24.0) !! MAOC turnover
      real(dp)  :: tvppoolx = 0.10*(1.0/0.44)*1.0/(365.0*24.0) !! POC turnover
      real(dp)  :: tvacx = 0.0015/24.0 !! leaching rate [1/hour]
      real(dp)  :: rootbeta = 1.0    !! root depth decay coefficient
      real(dp)  :: vmaxbeta = 0.5    !! Vmax depth decay coefficient
      !! Previous values (commented out): rootbeta=2.0, tvcpoolx=0.102*0.02/24.0/2.0,
      !! tvppoolx=4.705*0.019/24./10.00, tvacx=0.1*0.0015/24.0
   end type mic_param_default

  !> Computed model parameters per plot and soil layer.
  !>
  !> Km, Vmax, Q10, microbial turnover, necromass partitioning, and coefficients
  !> for adsorption, POC/MAOC turnover, and leaching.
  type mic_parameter
      real(dp), dimension(:,:), allocatable  :: K1,K2,K3 !! Michaelis constants for substrates 1-3
      real(dp), dimension(:,:), allocatable  :: J1,J2,J3  !! Michaelis constants for substrates 1-3
      real(dp), dimension(:,:), allocatable  :: V1,V2,V3  !! Vmax for substrates 1-3
      real(dp), dimension(:,:), allocatable  :: W1,W2,W3  !! Vmax for substrates 1-3
      real(dp), dimension(:,:), allocatable  :: desorp    !! desorption rates
      real(dp), dimension(:,:), allocatable  :: Q1,Q2     !! temperature sensitivity Q10
      real(dp), dimension(:,:), allocatable  :: fm,fs     !! substrate quality modifiers
      real(dp), dimension(:,:), allocatable  :: mgeR1,mgeR2,mgeR3 !! maintenance coeff R 1-3
      real(dp), dimension(:,:), allocatable  :: mgeK1,mgeK2,mgeK3 !! maintenance coeff K 1-3
      real(dp), dimension(:,:), allocatable  :: tvmicR,tvmicK       !! microbial turnover R/K
      real(dp), dimension(:,:), allocatable  :: betamicR,betamicK   !! microbial turnover beta R/K
      real(dp), dimension(:,:), allocatable  :: fmetave   !! average metabolic fraction
      real(dp), dimension(:,:,:), allocatable :: cn_r      !! C:N ratios per pool
      real(dp), dimension(:,:), allocatable  :: fr2p,fk2p !! necromass fractions to POC
      real(dp), dimension(:,:), allocatable  :: fr2c,fk2c !! necromass fractions to MAOC
      real(dp), dimension(:,:), allocatable  :: fr2a,fk2a !! necromass fractions to mineral-associated
      real(dp), dimension(:),  allocatable  :: xcnleaf,xcnroot,xcnwood !! C:N ratios for litter types
      real(dp), dimension(:),  allocatable  :: fligleaf,fligroot,fligwood !! lability fractions
      real(dp), dimension(:),  allocatable  :: diffsocx  !! SOC diffusion coefficient
      real(dp), dimension(:,:), allocatable  :: kdesorp   !! desorption rate [mg C cm-3 hour-1]
      real(dp), dimension(:,:), allocatable  :: kadsorp   !! adsorption rate [1/hour]
      real(dp), dimension(:,:), allocatable  :: fp2a      !! fraction DOC to POC
      real(dp), dimension(:,:), allocatable  :: tvcpool   !! MAOC turnover [1/hour]
      real(dp), dimension(:,:), allocatable  :: tvppool   !! POC turnover [1/hour]
      real(dp), dimension(:,:), allocatable  :: tvac      !! leaching rate [1/hour]
      real(dp), dimension(:,:), allocatable  :: qmaxcoeff !! Qmax-clay+silt coefficient
      integer,   dimension(:), allocatable  :: pft,bgctype,isoil,sorder,region,siteid,dataid !! site metadata
      real(dp), dimension(:,:), allocatable  :: sdepth,fracroot  !! soil depth [cm], root fraction
      real(dp), dimension(:,:), allocatable  :: csoilobs          !! measured SOC [mg C cm-3]
      real(dp), dimension(:,:), allocatable  :: csoilobsp,csoilobsm,fracaoc !! SOC profile components
      real(dp), dimension(:),  allocatable  :: c14soilobsp,c14soilobsm !! 14C observations
      real(dp), dimension(:,:,:), allocatable :: c14atm       !! atmospheric 14C
      integer,   dimension(:), allocatable  :: nyc14obs  !! year of 14C observation
      integer,   dimension(:), allocatable  :: top,bot   !! depth range indices
  end type mic_parameter

  !> Environmental and plant inputs per plot and time step.
  type mic_input
      real(dp), dimension(:,:), allocatable  :: tavg,wavg,tair    !! avg temp, avg water content, daily air temp
      real(dp), dimension(:,:), allocatable  :: ph                !! soil pH
      real(dp), dimension(:,:), allocatable  :: clay,silt         !! soil texture fractions
      real(dp), dimension(:,:), allocatable  :: porosity,bulkd    !! porosity, bulk density
      real(dp), dimension(:,:), allocatable  :: matpot            !! matric potential
      real(dp), dimension(:),  allocatable  :: dleaf,dwood,droot !! litter input depth profiles
      real(dp), dimension(:,:), allocatable  :: cinputm           !! monthly C input
      real(dp), dimension(:,:), allocatable  :: cinputs           !! summed C input
      real(dp), dimension(:),  allocatable  :: fcnpp             !! fine root C:N fraction of NPP
  end type mic_input

  !> Global-scale inputs with time series, for large-domain runs.
  type mic_global_input
      real(dp), dimension(:), allocatable  :: lon,lat    !! longitude, latitude [deg]
      real(dp), dimension(:), allocatable  :: time       !! simulation time [hour]
      integer,   dimension(:), allocatable  :: pft,bgctype,isoil,sorder,siteid !! site metadata
      real(dp), dimension(:), allocatable  :: area       !! grid cell area [km2]
      real(dp), dimension(:), allocatable  :: npp        !! net primary production
      real(dp), dimension(:), allocatable  :: ph,clay,silt !! soil properties
      real(dp), dimension(:), allocatable  :: poros,bulkd !! porosity, bulk density
      real(dp), dimension(:), allocatable  :: avgts,avgms !! avg temp, avg soil moisture
      real(dp), dimension(:,:), allocatable  :: patchfrac  !! PFT patch fractions
      real(dp), dimension(:,:,:), allocatable :: tsoil,moist,matpot !! time-varying soil state
      real(dp), dimension(:), allocatable  :: ligleaf,ligwood,ligroot !! litter lability
      real(dp), dimension(:,:), allocatable  :: dleaf,dwood,droot !! litter depth [mg C cm-2 time-1]
      real(dp), dimension(:,:), allocatable  :: cnleaf,cnwood,cnroot !! leaf/wood/root C:N
  end type mic_global_input

  !> Output fluxes for each plot.
  type mic_output
      real(dp), dimension(:), allocatable  :: fluxcinput  !! total C input
      real(dp), dimension(:), allocatable  :: fluxrsoil  !! soil respiration
      real(dp), dimension(:), allocatable  :: fluxcleach  !! dissolved C leaching
  end type mic_output

  !> Carbon pool state variables.
  type mic_cpool
      real(dp), dimension(:,:,:), allocatable :: cpool     !! C pool mass [mg C cm-3]
      real(dp), dimension(:,:,:), allocatable :: cpooleq  !! equilibrium C pool
      real(dp), dimension(:), allocatable  :: cpooleqp  !! prior equilibrium total
      real(dp), dimension(:), allocatable  :: cpooleqm  !! mean equilibrium total
      real(dp), dimension(:), allocatable  :: c12pooleqp  !! prior equilibrium C-12
      real(dp), dimension(:), allocatable  :: c12pooleqm  !! mean equilibrium C-12
  end type mic_cpool

  !> Nitrogen pool state variables.
  type mic_npool
      real(dp), dimension(:,:), allocatable  :: mineralN  !! mineral N [mg N cm-3]
  end type mic_npool


 contains

  !> Allocates all arrays in `mic_parameter` and `mic_param_xscale` derived types.
  subroutine mic_allocate_parameter(mpft,mbgc,mp,ms,micpxdef,micparam)
      integer, intent(in)  :: mpft  !! number of PFTs
      integer, intent(in)  :: mbgc  !! number of BGC types
      integer, intent(in)  :: mp    !! number of plots
      integer, intent(in)  :: ms    !! number of soil layers
      type(mic_parameter),    intent(out)  :: micparam
      type(mic_param_xscale), intent(out)  :: micpxdef

      allocate(micpxdef%xav(mbgc),         &
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
               micparam%fracaoc(mp,ms), &
               micparam%csoilobsp(mp,ms), &
               micparam%csoilobsm(mp,ms), &
               micparam%c14soilobsp(mp), &
               micparam%c14soilobsm(mp), &
               micparam%c14atm(79,5,2),   &
               micparam%nyc14obs(mp),    &
               micparam%top(mp),        &
               micparam%bot(mp))

      allocate(micparam%kdesorp(mp,ms), &
               micparam%kadsorp(mp,ms), &
               micparam%fp2a(mp,ms),    &
               micparam%tvcpool(mp,ms), &
               micparam%tvppool(mp,ms), &
               micparam%tvac(mp,ms),    &
               micparam%qmaxcoeff(mp,ms))
  end subroutine mic_allocate_parameter

  !> Allocates arrays in `mic_input` and `mic_global_input` derived types.
  subroutine mic_allocate_input(mp,ms,nlon,nlat,ntime,micinput,micglobal)
      use mic_constant, only : mpft

      integer, intent(in) :: mp      !! number of plots
      integer, intent(in) :: ms      !! number of soil layers
      integer, intent(in) :: nlon    !! number of longitude points
      integer, intent(in) :: nlat    !! number of latitude points
      integer, intent(in) :: ntime   !! number of time steps
      type(mic_input),        intent(out)  :: micinput
      type(mic_global_input), intent(out)  :: micglobal

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

  !> Allocates arrays in `mic_output` derived type.
  subroutine mic_allocate_output(mp,micoutput)
      integer, intent(in) :: mp        !! number of plots
      type(mic_output), intent(out)  :: micoutput

      allocate(micoutput%fluxcinput(mp))
      allocate(micoutput%fluxrsoil(mp))
      allocate(micoutput%fluxcleach(mp))

 end subroutine mic_allocate_output

  !> Allocates arrays in `mic_cpool` derived type.
  subroutine mic_allocate_cpool(mp,ms,miccpool)
      integer, intent(in) :: mp      !! number of plots
      integer, intent(in) :: ms      !! number of soil layers
      TYPE(mic_cpool), intent(out)  :: miccpool

      allocate(miccpool%cpool(mp,ms,mcpool), &
               miccpool%cpooleq(mp,ms,mcpool), &
               miccpool%cpooleqp(mp),   &
               miccpool%cpooleqm(mp),  &
               miccpool%c12pooleqp(mp), &
               miccpool%c12pooleqm(mp))
 end subroutine mic_allocate_cpool


  !> Allocates arrays in `mic_npool` derived type.
  subroutine mic_allocate_npool(mp,ms,micnpool)
      integer, intent(in) :: mp      !! number of plots
      integer, intent(in) :: ms      !! number of soil layers
      type(mic_npool), intent(out)  :: micnpool

      allocate(micnpool%mineralN(mp,ms))

  end subroutine mic_allocate_npool

  !> Deallocates all arrays in `mic_parameter` and `mic_param_xscale` derived types.
  !> @see mic_allocate_parameter
  subroutine mic_deallocate_parameter(mpft,mbgc,mp,ms,micpxdef,micparam)
      integer, intent(in)  :: mpft  !! number of PFTs
      integer, intent(in)  :: mbgc  !! number of BGC types
      integer, intent(in)  :: mp    !! number of plots
      integer, intent(in)  :: ms    !! number of soil layers
      TYPE(mic_parameter),    intent(inout)  :: micparam
      TYPE(mic_param_xscale), intent(inout)  :: micpxdef

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
               micparam%fracaoc,    &
               micparam%csoilobsp,  &
               micparam%csoilobsm,  &
               micparam%c14soilobsp,&
               micparam%c14soilobsm,&
               micparam%c14atm,     &
               micparam%nyc14obs,   &
               micparam%top,        &
               micparam%bot)

      deallocate(micparam%kdesorp,  &
               micparam%kadsorp,  &
               micparam%fp2a,     &
               micparam%tvcpool,  &
               micparam%tvppool,  &
               micparam%tvac,     &
               micparam%qmaxcoeff)
  end subroutine mic_deallocate_parameter

  !> Deallocates arrays in `mic_input` and `mic_global_input` derived types.
  !> @see mic_allocate_input
  subroutine mic_deallocate_input(mp,ms,nlon,nlat,ntime,micinput,micglobal)
      integer, intent(in) :: mp      !! number of plots
      integer, intent(in) :: ms      !! number of soil layers
      integer, intent(in) :: nlon    !! number of longitude points
      integer, intent(in) :: nlat    !! number of latitude points
      integer, intent(in) :: ntime   !! number of time steps
      TYPE(mic_input),        intent(inout)  :: micinput
      TYPE(mic_global_input), intent(inout)  :: micglobal

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

  !> Deallocates arrays in `mic_output` derived type.
  !> @see mic_allocate_output
  subroutine mic_deallocate_output(mp,micoutput)

      integer, intent(in) :: mp        !! number of plots
      TYPE(mic_output), intent(inout)  :: micoutput

      deallocate(micoutput%fluxcinput)
      deallocate(micoutput%fluxrsoil)
      deallocate(micoutput%fluxcleach)

  end subroutine mic_deallocate_output

  !> Deallocates arrays in `mic_cpool` derived type.
  !> @see mic_allocate_cpool
  subroutine mic_deallocate_cpool(mp,ms,miccpool)

      integer, intent(in) :: mp      !! number of plots
      integer, intent(in) :: ms      !! number of soil layers
      TYPE(mic_cpool), intent(inout)  :: miccpool

      deallocate(miccpool%cpool,  &
                 miccpool%cpooleq, &
                 miccpool%cpooleqp, &
                 miccpool%cpooleqm, &
                 miccpool%c12pooleqp,&
                 miccpool%c12pooleqm)

 end subroutine mic_deallocate_cpool


  !> Deallocates arrays in `mic_npool` derived type.
  !> @see mic_allocate_npool
  subroutine mic_deallocate_npool(mp,ms,micnpool)

      integer, intent(in) :: mp      !! number of plots
      integer, intent(in) :: ms      !! number of soil layers
      TYPE(mic_npool), intent(inout)  :: micnpool

      deallocate(micnpool%mineralN)

  end subroutine mic_deallocate_npool

end module mic_variable

