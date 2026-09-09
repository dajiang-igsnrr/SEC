!> Draft coupling API for driving MESC from an external land-surface model (LSM).
!!
!! This module defines exactly what an LSM (e.g. ORCHIDEE) would call:
!!
!! * [[mesc_coupling_init]] — one-off initialisation with time-invariant fields
!!   (grid, soil properties, vegetation and litter quality)
!! * [[mesc_coupling_step]] — advance one grid cell by one day with time-varying
!!   forcing (litterfall, soil temperature, soil moisture, water potential),
!!   returning the daily carbon fluxes
!! * [[mesc_coupling_get_cpool]] — retrieve the carbon pool state of a cell
!! * [[mesc_coupling_finalize]] — deallocate all state
!!
!! Typical usage from the LSM side:
!!
!! ```fortran
!! type(mesc_coupling_state) :: state
!! call mesc_coupling_init(state, ncell, nlev, npft, nbgc, ...) ! once
!! ! each day, for each land cell:
!! call mesc_coupling_step(state, icell, doy, year, ...)
!! call mesc_coupling_get_cpool(state, icell, pools)            ! whenever needed
!! call mesc_coupling_finalize(state)                           ! once
!! ```
!!
!! Design constraints:
!!
!! * One-way coupling: the LSM supplies soil temperature, soil moisture, water
!!   potential and litter inputs; MESC returns outputs only — soil carbon does
!!   not feed back into the LSM. (May change if nutrient coupling is added.)
!! * Vertical layers retained: MESC's vertical soil resolution is kept.
!! * RK4 retained as the ODE solver.
!! * Only `kinetics=3` is supported.
!! * Single interface: litter and SOM pools are advanced together by
!!   [[mesc_step]] (i.e., litter/SOM entry points are not separated).
!! * Minimal LSM-side changes: the LSM only passes inputs.
!!
!! Implementation notes:
!!
!! * The per-step forcing is staged through the `micglobal` structure so that
!!   the exact same [[mesc_step]] code path is used as in standalone runs —
!!   the coupled and standalone configurations therefore cannot diverge.
!! * Parameter values per BGC type are taken from the same lookup table file as
!!   the standalone forward run ([[getparam_global]]); the file name is passed
!!   to [[mesc_coupling_init]] so that the LSM configuration controls it.
!! * Only one `mesc_coupling_state` instance is supported at a time, because
!!   the model dimensions in `mic_constant` (`mp`, `ms`, `mpft`, ...) are
!!   module-level variables shared by all of MESC.
module mesc_coupling_module
  use precision_module, only : dp
  use mic_constant, only : mp, ms, mpft, mbgc, ntime, nlon, nlat, mcpool
  use mic_variable, only : mic_param_xscale, mic_param_default, mic_parameter, &
                           mic_input, mic_npool, mic_cpool, mic_output, mic_global_input, &
                           mic_allocate_parameter, mic_allocate_input, mic_allocate_output, &
                           mic_allocate_cpool, mic_allocate_npool, &
                           mic_deallocate_parameter, mic_deallocate_input, mic_deallocate_output, &
                           mic_deallocate_cpool, mic_deallocate_npool
  use mesc_inout_module, only : getparam_global
  use mesc_interface_module, only : vmic_param_constant, vmic_init, mesc_step
  implicit none

  ! All module members are public by default
  public

  !> Number of days per forcing year. MESC forcing is cyclic over one year.
  integer, parameter :: nday_year = 365
  !> Kinetics variant used for coupled runs. Only kinetics=3 is supported.
  integer, parameter :: kinetics_coupled = 3
  !> 14C tracking is disabled for coupled runs.
  integer, parameter :: isoc14_coupled = 0

  !> Complete state of a coupled MESC instance.
  !!
  !! The LSM declares one such variable, passes it to [[mesc_coupling_init]]
  !! once, and then passes it back to [[mesc_coupling_step]] each timestep.
  !! All members are conceptually private to MESC; the LSM should not modify
  !! them directly.
  type :: mesc_coupling_state
     type(mic_param_xscale)  :: micpxdef  !! BGC-type parameter scaling factors
     type(mic_param_default) :: micpdef   !! default parameter values
     type(mic_parameter)     :: micparam  !! computed model parameters
     type(mic_input)         :: micinput  !! per-cell per-day environmental inputs
     type(mic_global_input)  :: micglobal !! forcing staging area (per-day forcing written here)
     type(mic_cpool)         :: miccpool  !! carbon pool state [mg C cm-3]
     type(mic_npool)         :: micnpool  !! nitrogen pool state
     type(mic_output)        :: micoutput !! year-to-date flux accumulators [g C m-2]
     real(dp), dimension(:), allocatable :: zse !! soil layer thicknesses [m] (size nlev)
     integer :: ncell = 0  !! number of land grid cells
     integer :: nlev  = 0  !! number of soil layers
     integer :: npft  = 0  !! number of plant functional types
     integer :: nbgc  = 0  !! number of BGC (parameter cluster) types
     logical :: initialised = .false. !! whether [[mesc_coupling_init]] has completed
  end type mesc_coupling_state

contains

  !> Initialise a coupled MESC run.
  !!
  !! Replicates the standalone ORCHIDEE-forced setup ([[functn_global4]] with
  !! `jmodel==2`): allocates all structures, fills the time-invariant fields,
  !! reads the per-BGC parameter lookup table, sets constant parameters and
  !! initialises the carbon pools. Must be called once before any
  !! [[mesc_coupling_step]] call.
  !!
  !! Soil property inputs are clamped to the same ranges as in the standalone
  !! data loader: `ph` to [3.5, 9.5] and `bulkd` to [500, 1800] kg m-3;
  !! porosity is derived as `1 - bulkd/2650`.
  !!
  !! NOTE: litter quality (C:N ratios, lignin fractions) is applied per cell
  !!       via the dominant PFT index, as in standalone mode. If ORCHIDEE
  !!       prefers to pass per-cell values directly (e.g. for crops or sub-grid
  !!       PFT mixtures), the signature can be changed.
  !!
  !! NOTE: rooting depths are passed as a full per-PFT array (`npft` values)
  !!       directly to the parameter lookup; MESC does not index them by
  !!       dominant PFT. This lets ORCHIDEE supply its own values instead of
  !!       relying on internal PFT lookup tables.
  !!
  !! NOTE: `npp` is used by the microbial turnover rate calculation
  !!       ([[turnovert]]) and is currently fixed at init time (e.g. to a
  !!       climatology). A yearly update hook can be added if we want NPP to
  !!       evolve.
  subroutine mesc_coupling_init(state, ncell, nlev, npft, nbgc, zse, fparam, &
                                lon, lat, area, pft, &
                                rootdepth, cnleaf, cnwood, cnroot, &
                                ligleaf, ligwood, ligroot, npp, &
                                clay, silt, ph, bulkd, isoil, sorder, bgctype, &
                                cpool0)
    type(mesc_coupling_state), intent(inout) :: state    !! coupling state (initialised here)
    integer,                   intent(in)    :: ncell    !! number of land grid cells
    integer,                   intent(in)    :: nlev     !! number of soil layers
    integer,                   intent(in)    :: npft     !! number of PFTs (19 for ORCHIDEE)
    integer,                   intent(in)    :: nbgc     !! number of BGC types in the parameter table
    real(dp),                  intent(in)    :: zse(nlev)    !! soil layer thicknesses [m]
    character(len=*),          intent(in)    :: fparam   !! per-BGC parameter lookup table filename (CSV)
    real(dp),                  intent(in)    :: lon(ncell)   !! grid cell centre longitude [deg]
    real(dp),                  intent(in)    :: lat(ncell)   !! grid cell centre latitude [deg]
    real(dp),                  intent(in)    :: area(ncell)  !! grid cell area [km2]
    integer,                   intent(in)    :: pft(ncell)   !! dominant PFT index per cell (1..npft)
    real(dp),                  intent(in)    :: rootdepth(npft) !! rooting depth per PFT [m]
    real(dp),                  intent(in)    :: cnleaf(npft)  !! leaf litter C:N ratio per PFT [1]
    real(dp),                  intent(in)    :: cnwood(npft)  !! wood litter C:N ratio per PFT [1]
    real(dp),                  intent(in)    :: cnroot(npft)  !! root litter C:N ratio per PFT [1]
    real(dp),                  intent(in)    :: ligleaf(npft) !! leaf litter lignin fraction per PFT [1]
    real(dp),                  intent(in)    :: ligwood(npft) !! wood litter lignin fraction per PFT [1]
    real(dp),                  intent(in)    :: ligroot(npft) !! root litter lignin fraction per PFT [1]
    real(dp),                  intent(in)    :: npp(ncell)    !! annual NPP estimate per cell [g C m-2 yr-1]
    real(dp),                  intent(in)    :: clay(ncell)   !! clay fraction per cell [1]
    real(dp),                  intent(in)    :: silt(ncell)   !! silt fraction per cell [1]
    real(dp),                  intent(in)    :: ph(ncell)     !! soil pH per cell [1]
    real(dp),                  intent(in)    :: bulkd(ncell)  !! soil bulk density per cell [kg m-3]
    integer,                   intent(in)    :: isoil(ncell)  !! USDA soil texture class per cell (1..12)
    integer,                   intent(in)    :: sorder(ncell) !! USDA soil suborder per cell
    integer,                   intent(in)    :: bgctype(ncell)!! BGC type (parameter cluster) per cell (1..nbgc)
    real(dp), optional,        intent(in)    :: cpool0(ncell,nlev,mcpool)
      !! initial carbon pool state [mg C cm-3]; default: [[vmic_init]] cold-start values

    ! Local variables
    integer :: icell

    if (state%initialised) &
       error stop "ERROR mesc_coupling_init: state is already initialised"

    ! Set MESC's runtime dimensions (module-level variables in mic_constant).
    ! Time-varying forcing is staged one year at a time through micglobal.
    mp    = ncell
    ms    = nlev
    mpft  = npft
    mbgc  = nbgc
    ntime = nday_year
    nlon  = 0   ! not used outside the standalone NetCDF loaders
    nlat  = 0

    state%ncell = ncell
    state%nlev  = nlev
    state%npft  = npft
    state%nbgc  = nbgc
    allocate(state%zse(nlev))
    state%zse(:) = zse(:)

    call mic_allocate_parameter(mpft, mbgc, mp, ms, state%micpxdef, state%micparam)
    call mic_allocate_input(mp, ms, nlon, nlat, ntime, state%micinput, state%micglobal)
    call mic_allocate_output(mp, state%micoutput)
    call mic_allocate_cpool(mp, ms, state%miccpool)
    call mic_allocate_npool(mp, ms, state%micnpool)

    ! Time-invariant grid and soil fields (see getdata_global4_orchidee)
    state%micglobal%lon(:)     = lon(:)
    state%micglobal%lat(:)     = lat(:)
    state%micglobal%area(:)    = area(:)
    state%micglobal%pft(:)     = pft(:)
    state%micglobal%bgctype(:) = bgctype(:)
    state%micglobal%isoil(:)   = isoil(:)
    state%micglobal%sorder(:)  = sorder(:)
    state%micglobal%clay(:)    = clay(:)
    state%micglobal%silt(:)    = silt(:)
    state%micglobal%ph(:)      = min(9.5_dp, max(3.5_dp, ph(:)))
    state%micglobal%bulkd(:)   = max(500.0_dp, min(1800.0_dp, bulkd(:)))
    state%micglobal%poros(:)   = 1.0_dp - state%micglobal%bulkd(:)/2650.0_dp
    state%micglobal%npp(:)     = max(0.0_dp, npp(:))

    ! Time-invariant fields also held in the parameter structure. Litter
    ! quality is applied per cell from the PFT tables via the dominant PFT,
    ! as in getdata_global4_orchidee.
    state%micparam%pft(:)     = pft(:)
    state%micparam%bgctype(:) = bgctype(:)
    state%micparam%isoil(:)   = isoil(:)
    state%micparam%sorder(:)  = sorder(:)
    do icell = 1, ncell
       state%micparam%xcnleaf(icell)  = cnleaf(pft(icell))
       state%micparam%xcnroot(icell)  = cnroot(pft(icell))
       state%micparam%xcnwood(icell)  = cnwood(pft(icell))
       state%micparam%fligleaf(icell) = ligleaf(pft(icell))
       state%micparam%fligroot(icell) = ligroot(pft(icell))
       state%micparam%fligwood(icell) = ligwood(pft(icell))
    end do

    ! Per-BGC parameter values from the lookup table (forward-mode path of the
    ! standalone driver); also applies the per-PFT rooting depths.
    call getparam_global(fparam, rootdepth, state%micpxdef)

    ! Constant-in-time parameters (requires pft and bgctype to be set first)
    call vmic_param_constant(kinetics_coupled, state%micpxdef, state%micpdef, &
                             state%micparam, state%zse)

    ! Initial carbon pools: caller-supplied (e.g. from an offline spinup) or
    ! cold start
    if (present(cpool0)) then
       state%miccpool%cpool(:,:,:) = cpool0(:,:,:)
    else
       call vmic_init(state%miccpool, state%micnpool)
    end if

    state%micoutput%fluxcinput(:) = 0.0_dp
    state%micoutput%fluxrsoil(:)  = 0.0_dp
    state%micoutput%fluxcleach(:) = 0.0_dp

    state%initialised = .true.
  end subroutine mesc_coupling_init

  !> Advance one grid cell by one day.
  !!
  !! Stages the LSM forcing for cell `icell` and day `doy` into the forcing
  !! structure, then calls [[mesc_step]] — the same per-day code path as in
  !! standalone mode — to update the carbon pools. Returns the carbon fluxes
  !! for this day. Flux accumulators in the state are reset at the start of
  !! each year (`doy == 1`), so `state%micoutput` always holds year-to-date
  !! totals.
  !!
  !! Inputs are clamped to the same ranges as in the standalone data loader:
  !! `tsoil` to [-100, 50] degC, `moist` to [0, 0.8] m3 m-3 and `matpot` to
  !! [-1000, 0] MPa. Negative litter fluxes are reset to zero.
  !!
  !! NOTE: MESC steps daily. If the LSM uses a sub-daily timestep, forcing must
  !!       be aggregated to daily means/sums by the LSM (or its coupling layer)
  !!       before calling this routine.
  !!
  !! NOTE: mineral nitrogen is currently fixed internally at 0.1 g N kg-1 (as in
  !!       standalone mode, see [[variable_time]]); nitrogen coupling is out of
  !!       scope for the one-way setup.
  subroutine mesc_coupling_step(state, icell, doy, year, &
                                litter_leaf, litter_wood, litter_root, &
                                tsoil, moist, matpot, &
                                flux_cinput, flux_rsoil, flux_cleach)
    type(mesc_coupling_state), intent(inout) :: state       !! coupling state
    integer,                   intent(in)    :: icell       !! grid cell index (1..ncell)
    integer,                   intent(in)    :: doy         !! day of year (1..365)
    integer,                   intent(in)    :: year        !! simulation year counter (1-based)
    real(dp),                  intent(in)    :: litter_leaf !! leaf litterfall today [g C m-2 day-1]
    real(dp),                  intent(in)    :: litter_wood !! above-ground woody litterfall today [g C m-2 day-1]
    real(dp),                  intent(in)    :: litter_root !! below-ground litterfall today [g C m-2 day-1]
    real(dp),                  intent(in)    :: tsoil(state%nlev)  !! soil temperature per layer [degC]
    real(dp),                  intent(in)    :: moist(state%nlev)  !! soil water content per layer [m3 m-3]
    real(dp),                  intent(in)    :: matpot(state%nlev) !! soil matric potential per layer [MPa]
    real(dp),                  intent(out)   :: flux_cinput !! total C input today [g C m-2]
    real(dp),                  intent(out)   :: flux_rsoil  !! soil respiration today [g C m-2]
    real(dp),                  intent(out)   :: flux_cleach !! dissolved C leached from the bottom layer today [g C m-2]

    ! Local variables
    real(dp) :: cinput_prev, rsoil_prev
    integer  :: ny

    if (.not. state%initialised) &
       error stop "ERROR mesc_coupling_step: state is not initialised"
    if (icell < 1 .or. icell > state%ncell) &
       error stop "ERROR mesc_coupling_step: icell is outside 1:ncell"
    if (doy < 1 .or. doy > nday_year) &
       error stop "ERROR mesc_coupling_step: doy is outside 1:365"

    ! Reset year-to-date flux accumulators at the start of each year
    if (doy == 1) then
       state%micoutput%fluxcinput(icell) = 0.0_dp
       state%micoutput%fluxrsoil(icell)  = 0.0_dp
       state%micoutput%fluxcleach(icell) = 0.0_dp
    end if

    ! Stage today's forcing into micglobal for this cell; variable_time
    ! (called within mesc_step) forwards it into micinput with the required
    ! per-timestep unit conversions.
    state%micglobal%dleaf(icell,doy) = max(0.0_dp, litter_leaf)
    state%micglobal%dwood(icell,doy) = max(0.0_dp, litter_wood)
    state%micglobal%droot(icell,doy) = max(0.0_dp, litter_root)
    state%micglobal%tsoil(icell,:,doy) = max(-100.0_dp, min(50.0_dp, tsoil(:)))
    state%micglobal%moist(icell,:,doy) = max(0.0_dp, min(0.8_dp, moist(:)))
    state%micglobal%matpot(icell,:,doy) = max(-1000.0_dp, min(0.0_dp, matpot(:)))

    ! Year offset used by the 14C clock only; irrelevant with isoc14=0
    ny = year - 1

    cinput_prev = state%micoutput%fluxcinput(icell)
    rsoil_prev  = state%micoutput%fluxrsoil(icell)

    call mesc_step(icell, doy, year, ny, kinetics_coupled, isoc14_coupled, state%zse, &
                   state%micpxdef, state%micpdef, state%micparam, state%micinput, &
                   state%micglobal, state%miccpool, state%micnpool, state%micoutput)

    ! Daily fluxes: C input and respiration accumulate within mesc_step, so
    ! take differences; leaching is assigned (not accumulated) by mesc_step
    ! and therefore already is today's flux.
    flux_cinput = state%micoutput%fluxcinput(icell) - cinput_prev
    flux_rsoil  = state%micoutput%fluxrsoil(icell) - rsoil_prev
    flux_cleach = state%micoutput%fluxcleach(icell)
  end subroutine mesc_coupling_step

  !> Return the carbon pool state of one grid cell.
  !!
  !! Pool order follows MESC's `mcpool = 10` convention (1-2 litter, 3-4
  !! microbes, 5-7 protected/active SOM, 8-10 currently unused). To convert to
  !! areal densities, multiply layer `ns` by `state%zse(ns) * 1000` (giving
  !! g C m-2 per pool per layer).
  subroutine mesc_coupling_get_cpool(state, icell, cpool_out)
    type(mesc_coupling_state), intent(in)  :: state               !! coupling state
    integer,                   intent(in)  :: icell               !! grid cell index (1..ncell)
    real(dp),                  intent(out) :: cpool_out(state%nlev, mcpool)
      !! carbon pool state of cell `icell` [mg C cm-3]

    if (.not. state%initialised) &
       error stop "ERROR mesc_coupling_get_cpool: state is not initialised"
    if (icell < 1 .or. icell > state%ncell) &
       error stop "ERROR mesc_coupling_get_cpool: icell is outside 1:ncell"

    cpool_out(:,:) = state%miccpool%cpool(icell,:,:)
  end subroutine mesc_coupling_get_cpool

  !> Finalise a coupled MESC run.
  !!
  !! Deallocates all internal state. In coupled mode, restart and history
  !! output are owned by the LSM (MESC writes no files); the LSM can retrieve
  !! the final pool state beforehand via [[mesc_coupling_get_cpool]] and store
  !! it in its own restart files.
  subroutine mesc_coupling_finalize(state)
    type(mesc_coupling_state), intent(inout) :: state !! coupling state (deallocated here)

    if (.not. state%initialised) error stop "ERROR mesc_coupling_finalize: state is not initialised"

    call mic_deallocate_parameter(state%npft, state%nbgc, state%ncell, state%nlev, &
                                  state%micpxdef, state%micparam)
    call mic_deallocate_input(state%ncell, state%nlev, nlon, nlat, nday_year, &
                              state%micinput, state%micglobal)
    call mic_deallocate_output(state%ncell, state%micoutput)
    call mic_deallocate_cpool(state%ncell, state%nlev, state%miccpool)
    call mic_deallocate_npool(state%ncell, state%nlev, state%micnpool)

    deallocate(state%zse)
    state%ncell = 0
    state%nlev  = 0
    state%npft  = 0
    state%nbgc  = 0
    state%initialised = .false.
  end subroutine mesc_coupling_finalize

end module mesc_coupling_module
