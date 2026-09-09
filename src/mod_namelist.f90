!> Module for MESC namelist configuration.
module mesc_namelist
  use, intrinsic :: iso_fortran_env, only : error_unit, output_unit
  use precision_module, only: dp
  implicit none
  private

  integer, parameter :: path_len = 140

  logical, public :: use_modis_npp = .false.

  ! Enumerator for handling the different models supported by jmodel
  enum, bind(c)
    enumerator :: model_cable = 1
    enumerator :: model_orchidee = 2
  end enum

  !> Derived type for holding parameter values related to MESC.
  type, public :: mesc_config
    integer :: runcase = 0
    logical :: jglobal = .false.
    integer :: kinetics = 3
    integer :: bgcopt = 1
    logical :: jopt = .false.
    integer :: jrestart = 0
    integer :: jmodel = 1
    integer :: ifsoc14 = 0
    character(len=path_len) :: frestart_in = ''
    character(len=path_len) :: frestart_out = ''
    character(len=path_len) :: foutput = ''
    character(len=path_len) :: fparameter = ''
    character(len=path_len) :: cfraction = ''
    character(len=path_len) :: frac14c = ''
    character(len=path_len) :: f14c(5) = ''
    character(len=path_len) :: filecluster = ''
    character(len=path_len) :: fhwsdsoc = ''
    character(len=path_len) :: faustsoc = ''
    character(len=path_len) :: fmodis = ''
    character(len=path_len) :: fanoc = ''
    character(len=path_len) :: fglobal(7) = ''
    real(dp) :: xopt(16) = 1.0_dp
    integer :: nxopt(16) = [1, 2, 3, 4, 5, 6, 7, 8, &
                            9, 10, 11, 12, 13, 14, 15, 16]
  end type mesc_config

  public :: read_mesc_namelist, print_mesc_config, model_orchidee, model_cable

contains

  !> Populate the a mesc_config derived type with parameter values read from
  !> a namelist.
  subroutine read_mesc_namelist(filename, config)
    character(len=*), intent(in) :: filename
      !! Name of namelist file to read from
    type(mesc_config), intent(out) :: config
      !! Derived type instance to hold parameter values

    integer :: nml_unit, ios
    character(len=512) :: iomsg
    logical :: jglobal
    integer :: runcase, kinetics, bgcopt
    logical :: jopt
    integer :: jrestart, jmodel, ifsoc14
    character(len=path_len) :: frestart_in, frestart_out, foutput
    character(len=path_len) :: fparameter, cfraction, frac14c, f14c(5), filecluster
    character(len=path_len) :: fhwsdsoc, faustsoc
    character(len=path_len) :: fmodis, fanoc, fglobal(7)
    real(dp) :: xopt(16)
    integer :: nxopt(16)

    namelist /mesc/ runcase, jglobal, kinetics, bgcopt, jopt, &
      jrestart, jmodel, ifsoc14, frestart_in, frestart_out, &
      foutput, fparameter, cfraction, frac14c, f14c, filecluster, &
      fhwsdsoc, faustsoc, fmodis, fanoc, fglobal, xopt, nxopt, use_modis_npp

    ! Copy defaults into the local variables read by the namelist.
    runcase = config%runcase
    jglobal = config%jglobal
    kinetics = config%kinetics
    bgcopt = config%bgcopt
    jopt = config%jopt
    jrestart = config%jrestart
    jmodel = config%jmodel
    ifsoc14 = config%ifsoc14
    frestart_in = config%frestart_in
    frestart_out = config%frestart_out
    foutput = config%foutput
    fparameter = config%fparameter
    cfraction = config%cfraction
    frac14c = config%frac14c
    f14c = config%f14c
    filecluster = config%filecluster
    fhwsdsoc = config%fhwsdsoc
    faustsoc = config%faustsoc
    fmodis = config%fmodis
    fanoc = config%fanoc
    fglobal = config%fglobal
    xopt = config%xopt
    nxopt = config%nxopt

    open(newunit=nml_unit, file=trim(filename), status='old', &
         action='read', iostat=ios, iomsg=iomsg)
    if (ios /= 0) call fatal_error('Cannot open "' // trim(filename) // '": ' // trim(iomsg))

    read(nml_unit, nml=mesc, iostat=ios, iomsg=iomsg)
    close(nml_unit)
    if (ios /= 0) call fatal_error('Cannot read &mesc from "' // &
      trim(filename) // '": ' // trim(iomsg))

    config%runcase = runcase
    config%jglobal = jglobal
    config%kinetics = kinetics
    config%bgcopt = bgcopt
    config%jopt = jopt
    config%jrestart = jrestart
    config%jmodel = jmodel
    config%ifsoc14 = ifsoc14
    config%frestart_in = frestart_in
    config%frestart_out = frestart_out
    config%foutput = foutput
    config%fparameter = fparameter
    config%cfraction = cfraction
    config%frac14c = frac14c
    config%f14c = f14c
    config%filecluster = filecluster
    config%fhwsdsoc = fhwsdsoc
    config%faustsoc = faustsoc
    config%fmodis = fmodis
    config%fanoc = fanoc
    config%fglobal = fglobal
    config%xopt = xopt
    config%nxopt = nxopt
  end subroutine read_mesc_namelist

  !> Print the parameter values associated with a mesc_config derived type
  !> instance.
  subroutine print_mesc_config(config)
    type(mesc_config), intent(in) :: config
      !! Derived type instance holding parameter values
    write(output_unit, '(a,i0)') 'runcase     = ', config%runcase
    write(output_unit, '(a,l1)') 'jglobal     = ', config%jglobal
    write(output_unit, '(a,i0)') 'kinetics    = ', config%kinetics
    write(output_unit, '(a,i0)') 'bgcopt      = ', config%bgcopt
    write(output_unit, '(a,i0)') 'jopt        = ', config%jopt
    write(output_unit, '(a,i0)') 'jrestart    = ', config%jrestart
    write(output_unit, '(a,i0)') 'jmodel      = ', config%jmodel
    write(output_unit, '(a,i0)') 'ifsoc14     = ', config%ifsoc14
    write(output_unit, '(a,a)')  'foutput     = ', trim(config%foutput)
    write(output_unit, '(a,a)')  'fparameter  = ', trim(config%fparameter)
    write(output_unit, '(a,a)')  'fhwsdsoc    = ', trim(config%fhwsdsoc)
    write(output_unit, '(a,a)')  'fanoc       = ', trim(config%fanoc)
  end subroutine print_mesc_config

  !> Raise a fatal error, reporting a given message.
  subroutine fatal_error(message)
    character(len=*), intent(in) :: message  !! Message to report
    write(error_unit, '(a)') 'ERROR: ' // trim(message)
    error stop 1
  end subroutine fatal_error

end module mesc_namelist
