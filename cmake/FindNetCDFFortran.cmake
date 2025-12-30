# FindNetCDFFortran.cmake
#
# Exports:
#   NetCDFFortran_FOUND
#   NetCDFFortran_INCLUDE_DIRS
#   NetCDFFortran_LIBRARIES   (may contain raw flags from nf-config)
#
# Strategy:
#   1) Use nf-config (best on HPC modules)
#   2) Fallback to prefix hints (NetCDFFortran_ROOT / NETCDF_FORTRAN / NETCDF)

find_program(NF_CONFIG_EXECUTABLE nf-config)

# Collect prefix hints
set(_NETCDF_HINTS "")
if(DEFINED NetCDFFortran_ROOT)
  list(APPEND _NETCDF_HINTS "${NetCDFFortran_ROOT}")
endif()
if(DEFINED ENV{NETCDF_FORTRAN})
  list(APPEND _NETCDF_HINTS "$ENV{NETCDF_FORTRAN}")
endif()
if(DEFINED ENV{NETCDF})
  list(APPEND _NETCDF_HINTS "$ENV{NETCDF}")
endif()

# ------------------------------------------------------------
# 1) Preferred: nf-config
# ------------------------------------------------------------
if(NF_CONFIG_EXECUTABLE)
  execute_process(COMMAND ${NF_CONFIG_EXECUTABLE} --includedir
                  OUTPUT_VARIABLE _nf_inc
                  OUTPUT_STRIP_TRAILING_WHITESPACE)

  execute_process(COMMAND ${NF_CONFIG_EXECUTABLE} --flibs
                  OUTPUT_VARIABLE _nf_flibs
                  OUTPUT_STRIP_TRAILING_WHITESPACE)

  # Basic sanity
  set(NetCDFFortran_INCLUDE_DIRS "${_nf_inc}")
  set(NetCDFFortran_LIBRARIES "${_nf_flibs}")

  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(NetCDFFortran DEFAULT_MSG
    NetCDFFortran_INCLUDE_DIRS NetCDFFortran_LIBRARIES
  )
  return()
endif()

# ------------------------------------------------------------
# 2) Fallback: manual search by prefix hints
# ------------------------------------------------------------
find_path(NetCDFFortran_INCLUDE_DIRS
  NAMES netcdf.mod netcdf.inc netcdf.h
  HINTS ${_NETCDF_HINTS}
  PATH_SUFFIXES include include/netcdf
)

find_library(NetCDFFortran_F90_LIB
  NAMES netcdff
  HINTS ${_NETCDF_HINTS}
  PATH_SUFFIXES lib lib64
)

find_library(NetCDFFortran_C_LIB
  NAMES netcdf
  HINTS ${_NETCDF_HINTS}
  PATH_SUFFIXES lib lib64
)

set(NetCDFFortran_LIBRARIES "")
if(NetCDFFortran_F90_LIB)
  list(APPEND NetCDFFortran_LIBRARIES ${NetCDFFortran_F90_LIB})
endif()
if(NetCDFFortran_C_LIB)
  list(APPEND NetCDFFortran_LIBRARIES ${NetCDFFortran_C_LIB})
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(NetCDFFortran DEFAULT_MSG
  NetCDFFortran_INCLUDE_DIRS NetCDFFortran_LIBRARIES
)
