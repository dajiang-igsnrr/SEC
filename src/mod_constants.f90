!> Model constants, PFT-specific parameter tables, and runtime-configurable dimensions
module mic_constant
  use precision_module, only : dp
  IMPLICIT NONE

  ! All module members are public by default
  public

  integer,  parameter :: diag=0       !! diagnostic flag: 1 for printout, 0 for silent
  integer,  parameter :: outp=1       !! output site index
  integer             :: mp           !! number of sites the model runs for
  integer             :: ntime        !! number of time steps
  integer             :: mpft         !! number of PFTs (17 for CABLE, 19 for ORCHIDEE)
  integer             :: mbgc         !! number of soil categories
  integer             :: ms           !! number of soil layers
  integer             :: nlon         !! number of longitude grid cells
  integer             :: nlat         !! number of latitude grid cells
  integer,  parameter :: mcpool=10    !! number of carbon pools
  integer,  parameter :: nfvar=22     !! number of data input variables
  real(dp),parameter  :: delt= 1.0    !! model time step in hours
  real(dp),parameter  :: tvc14 = (1.0/(24.0*365.0))* log(2.0)/5730.0    !! 14C decay rate (1/hour)
  integer,  parameter :: nyic14=1940  !! start year of 14C record
  integer,  parameter :: nyec14=2020  !! end year of 14C calculation
  real(dp),parameter  :: thresh_patchfrac=1.0e-6   !! minimal patch area fraction for numerical filtering

  !> C:N ratios and lignin fractions for each of the 17 CABLE PFTs (cnleaf1, cnroot1, cnwood1, ligleaf1, ligroot1, ligwood1)
  real(dp), dimension(17) :: cnleaf1,cnroot1,cnwood1,ligleaf1,ligroot1,ligwood1
  data cnLeaf1/99.60,46.20,118.60,62.80,75.20,69.60,88.00,98.40,43.20,50.00,99.60,46.20,62.80,100.00,80.00,80.00,80.00/
  data cnwood1/250.63,142.00,256.63,164.42,149.58,157.89,157.89,155.05,157.89,131.58,250.63,142.00,164.42,157.89,157.89,142.11,157.89/
  data cnroot1/81.89,68.00,83.33,70.22,74.56,71.67,69.67,76.67,67.44,78.89,81.89,68.00,70.22,78.89,78.89,78.89,78.89/
  data ligleaf1/0.25,0.20,0.20,0.20,0.20,0.10,0.10,0.10,0.10,0.10,0.25,0.20,0.20,0.15,0.15,0.25,0.10/
  data ligwood1/0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40/
  data ligroot1/0.25,0.20,0.20,0.20,0.20,0.10,0.10,0.10,0.10,0.10,0.25,0.20,0.20,0.15,0.15,0.25,0.10/

  !> C:N ratios and lignin fractions for each of the 19 ORCHIDEE PFTs (cnleaf2, cnroot2, cnwood2, ligleaf2, ligroot2, ligwood2); values sourced from CALIPSO spreadsheet
  real(dp), dimension(19) :: cnleaf2,cnroot2,cnwood2,ligleaf2,ligroot2,ligwood2
  data cnleaf2/100.0,46.2,46.2,99.6,46.2,62.8,99.6,62.8,118.6,69.6,88.0,43.2,50.0,69.6,88.0,69.6,88.0,69.6,88.0/
  data cnwood2/157.9,142.00,142.00,250.63,142.00,164.42,250.63,164.42,256.63,157.89,157.89,157.89,131.58,157.89,157.89,157.89,157.89,157.89,157.89/
  data cnroot2/78.9,68.00,68.00,81.89,68.00,70.22,81.89,70.22,83.33,71.67,69.67,67.44,78.89,71.67,69.67,71.67,69.67,71.67,69.67/
  data ligleaf2/0.15,0.2,0.2,0.25,0.2,0.2,0.25,0.2,0.2,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1/
  data ligwood2/0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4/
  data ligroot2/0.15,0.2,0.2,0.25,0.2,0.2,0.25,0.2,0.2,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1/

  real(dp), dimension(17) :: xrootcable  !! CABLE rooting depth per PFT (m)
  data xrootcable/1.43,0.94,1.43,1.04,0.77,0.85,0.62,1.77,0.94,0.94,1.43,0.94,1.04,0.53,1.00,1.00,1.00/
  real(dp), dimension(19) :: xrootorchidee  !! ORCHIDEE rooting depth per PFT (m)
  data xrootorchidee/0.94,0.94,1.04,1.04,1.04,1.43,1.43,1.43,0.85,0.62,0.94,0.94,0.85,0.85,0.85,0.85,0.85,0.85,0.85/

 !! @section legacy_configs Legacy commented-out configurations
 !! ~~~~
 !! integer,  parameter  :: msite=213   ! number of sites
 !! integer,  parameter  :: ms= 10      !7       ! soil layers
 !! real(dp) zse(ms)
 !! data zse/0.2,0.2,0.2,0.2,0.2,0.5,0.5/
 !! data zse/0.02,0.04,0.06,0.08,0.2,0.2,0.2,0.2,0.5,0.5/
 !! integer,  parameter  :: nlon =180
 !! integer,  parameter  :: nlat =90
 !! real(dp),parameter  :: diffsoc  =(1.0/24.0)* 2.74e-3  !cm2/hour
 !!                                       ! m2/hour  ! see Table 1,Camino-Serrano et al. (2018)
 !! ORCHIDEE C:N ratios (original CALIPSO spreadsheet values, superseded)
 !! data cnleaf2/100.0,47.12,51.42,94.78,49.99,54.55,94.78,54.55,94.78,75.29,101.21,75.29,93.80,75.29,101.21,75.29,101.21,75.29,101.21/
 !! data cnwood2/157.89,115.74,113.64,208.96,125.00,145.45,208.96,145.45,36.36,16.00,22.00,16.00,16.00,16.00,22.00,16.00,22.00,16.00,22.00/
 !! data cnroot2/78.89,81.43,82.53,151.87,87.49,100.00,151.87,100.00,65.57,45.64,61.60,45.64,54.90,45.64,61.60,45.64,61.60,45.64,61.60/
 !! data ligleaf2/0.15,0.20,0.20,0.25,0.20,0.20,0.25,0.20,0.25,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10/
 !! data ligwood2/0.4,0.25,0.25,0.30,0.25,0.25,0.30,0.25,0.30,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10/
 !! data ligroot2/0.15,0.20,0.20,0.25,0.20,0.20,0.25,0.20,0.25,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10/
 !! ~~~~

end module mic_constant
