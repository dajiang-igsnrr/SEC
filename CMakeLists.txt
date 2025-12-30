cmake_minimum_required(VERSION 3.18)
project(SEC_MESC LANGUAGES Fortran)

# ------------------------------------------------------------
# 0) Fortran standard
# ------------------------------------------------------------
set(CMAKE_Fortran_STANDARD 2008)
set(CMAKE_Fortran_STANDARD_REQUIRED ON)

# ------------------------------------------------------------
# 1) Let CMake find our custom modules
# ------------------------------------------------------------
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# ------------------------------------------------------------
# 2) Find NetCDF Fortran
#   - Prefer nf-config from module environment
#   - Fallback to NetCDFFortran_ROOT / env NETCDF_FORTRAN / env NETCDF
# ------------------------------------------------------------
find_package(NetCDFFortran REQUIRED)

message(STATUS "NetCDFFortran include: ${NetCDFFortran_INCLUDE_DIRS}")
message(STATUS "NetCDFFortran libs   : ${NetCDFFortran_LIBRARIES}")

# ------------------------------------------------------------
# 3) Sources and executable
#   (name matches your previous -o main)
# ------------------------------------------------------------
add_executable(main
  src/main.f90
  src/mesc_cost.f90
  src/mesc_function.f90
  src/mesc_inout.f90
  src/mesc_interface.f90
  src/mesc_model.f90
  src/mesc_variable.f90
)

# Put .mod files into build/mod
set_target_properties(main PROPERTIES
  Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/mod"
)

# ------------------------------------------------------------
# 4) Include + Link
#   - include: netcdf.mod location
#   - link: nf-config --flibs output or fallback libs
# ------------------------------------------------------------
target_include_directories(main PRIVATE
  "${CMAKE_BINARY_DIR}/mod"
  ${NetCDFFortran_INCLUDE_DIRS}
)

# NetCDFFortran_LIBRARIES may be a list of raw flags (e.g., -L... -lnetcdff -lnetcdf ...)
# CMake is OK with passing these through to the linker.
target_link_libraries(main PRIVATE
  ${NetCDFFortran_LIBRARIES}
)

# ------------------------------------------------------------
# 5) NVHPC tweaks (optional)
# ------------------------------------------------------------
if(CMAKE_Fortran_COMPILER_ID MATCHES "NVHPC|PGI")
  target_compile_options(main PRIVATE -Mbackslash)
endif()

# ------------------------------------------------------------
# 6) POST-BUILD: copy main executable to SEC/test
# ------------------------------------------------------------
add_custom_command(TARGET main POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy
          $<TARGET_FILE:main>
          ${CMAKE_SOURCE_DIR}/test/main
  COMMENT "Copying main to SEC/test/"
)
