module mic_constant

  IMPLICIT NONE
  integer,  parameter  :: r_2 = SELECTED_REAL_KIND(12, 60)
  integer,  parameter  :: diag=0       ! =1 for printout 0 no prinout
  integer,  parameter  :: outp=1       ! output site
  !integer,  parameter  :: msite=213   ! number of sites
  integer                 mp           ! number of site the model runs for
  integer                 ntime        ! 365  !12 * 4 ! 4 year's monthly global forcings
  integer                 mpft         ! =17 !15      ! number of PFTs =17 FOR cable AND =19 FOR orchidee 
  integer                 mbgc         ! number of soil categories
  integer                 ms
  integer                 nlon      
  integer                 nlat     
!  integer,  parameter  :: ms= 10      !7       ! soil layers
!  real(r_2) zse(ms)
!  data zse/0.2,0.2,0.2,0.2,0.2,0.5,0.5/
!  data zse/0.02,0.04,0.06,0.08,0.2,0.2,0.2,0.2,0.5,0.5/
!  integer,  parameter  :: nlon =180
!  integer,  parameter  :: nlat =90
  integer,  parameter  :: mcpool=10    ! number of C pools
  integer,  parameter  :: nfvar=22     ! number of data input variables
  real(r_2),parameter  :: delt= 1.0    ! one hour
  real(r_2),parameter  :: tvc14 = (1.0/(24.0*365.0))* alog(2.0)/5730.0    ! 1/hour 
  integer,  parameter  :: nyic14=1940  ! year 0 of 14C record 
  integer,  parameter  :: nyec14=2020  ! last yr of 14C calculation   
  real(r_2),parameter  :: thresh_patchfrac=1.0e-6   ! minimial patch area fraction
!  real(r_2),parameter  :: diffsoc  =(1.0/24.0)* 2.74e-3  !cm2/hour   
!                                       ! m2/hour  ! see Table 1,Camino-Serrano et al. (2018)
  ! CABLE PFT-dependent parameter values
  real(r_2), dimension(17) :: cnleaf1,cnroot1,cnwood1,ligleaf1,ligroot1,ligwood1
  data cnLeaf1/99.60,46.20,118.60,62.80,75.20,69.60,88.00,98.40,43.20,50.00,99.60,46.20,62.80,100.00,80.00,80.00,80.00/
  data cnwood1/250.63,142.00,256.63,164.42,149.58,157.89,157.89,155.05,157.89,131.58,250.63,142.00,164.42,157.89,157.89,142.11,157.89/
  data cnroot1/81.89,68.00,83.33,70.22,74.56,71.67,69.67,76.67,67.44,78.89,81.89,68.00,70.22,78.89,78.89,78.89,78.89/
  data ligleaf1/0.25,0.20,0.20,0.20,0.20,0.10,0.10,0.10,0.10,0.10,0.25,0.20,0.20,0.15,0.15,0.25,0.10/
  data ligwood1/0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40/
  data ligroot1/0.25,0.20,0.20,0.20,0.20,0.10,0.10,0.10,0.10,0.10,0.25,0.20,0.20,0.15,0.15,0.25,0.10/

  ! ORCHIDEE PFT-dependent parameter values
  real(r_2), dimension(18) :: cnleaf2,cnroot2,cnwood2,ligleaf2,ligroot2,ligwood2
  data cnleaf2/47.12,51.42,94.78,49.99,54.55,94.78,54.55,94.78,75.29,101.21,75.29,93.80,75.29,101.21,75.29,101.21,75.29,101.21/
  data cnwood2/115.74,113.64,208.96,125.00,145.45,208.96,145.45,36.36,16.00,22.00,16.00,16.00,16.00,22.00,16.00,22.00,16.00,22.00/
  data cnroot2/81.43,82.53,151.87,87.49,100.00,151.87,100.00,65.57,45.64,61.60,45.64,54.90,45.64,61.60,45.64,61.60,45.64,61.60/
  data ligleaf2/0.20,0.20,0.25,0.20,0.20,0.25,0.20,0.25,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10/
  data ligwood2/0.25,0.25,0.30,0.25,0.25,0.30,0.25,0.30,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10/
  data ligroot2/0.20,0.20,0.25,0.20,0.20,0.25,0.20,0.25,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10/  

  real(r_2), dimension(17) :: xrootcable
  real(r_2), dimension(18) :: xrootorchidee   
  data xrootcable/1.43,0.94,1.43,1.04,0.77,0.85,0.62,1.77,0.94,0.94,1.43,0.94,1.04,0.53,1.00,1.00,1.00/
  data xrootorchidee/0.94,0.94,1.04,1.04,1.04,1.43,1.43,1.43,0.85,0.62,0.94,0.94,0.85,0.85,0.85,0.85,0.85,0.85/
  
end module mic_constant
